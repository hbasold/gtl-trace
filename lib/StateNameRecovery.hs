{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}

module StateNameRecovery (
  StateName (..)
  , State (..)
  , StateStructureMap
  , parseStateStructureMap
) where

import Prelude hiding (foldl)
import qualified Data.Map as Map (map)
import Data.Map as Map hiding (null, map)
import qualified Data.Set as Set (Set, toAscList, fromList, map)
import Data.List (intercalate)
import Data.Foldable (foldl)

import Text.ParserCombinators.UU as UU hiding (Steps)
import Text.ParserCombinators.UU.Utils as UU (pSymbol, pParens, pBraces, pComma, lexeme, pParentheticalString, pNatural, runParser)
import Text.ParserCombinators.UU.BasicInstances as UU (Parser)

import Data.GraphViz (Labellable(..))
import Data.GraphViz.Attributes.Complete (Label(HtmlLabel))
import Data.GraphViz.Attributes.HTML
import Data.Text.Lazy (pack)

import Debug.Hood.Observe
--import Debug.Trace.LocationTH

data StateName = NatLab Integer | StrLab String deriving (Eq, Ord)
data State =
  Simple StateName
  | ProductState State State
  | SetState (Set.Set State)
  deriving (Eq,Ord)

instance Show StateName where
  show (NatLab i) = show i
  show (StrLab s) = show s

instance Show State where
  show (Simple s) = show s
  show (ProductState s1 s2) = "(" ++ show s1 ++ "," ++ show s2 ++ ")"
  show (SetState sts) =
    let stL = Set.toAscList sts
        str = if null stL then "" else (intercalate ",") $ map show stL
    in "{" ++ str ++ "}"

instance Labellable State where
  toLabelValue s = HtmlLabel $ stateHtmlLabel s


stateHtmlLabel :: State -> HtmlLabel
stateHtmlLabel (Simple s) = HtmlText [HtmlStr $ pack $ show s]
stateHtmlLabel (ProductState s1 s2) = HtmlTable $ HTable Nothing [] [HtmlRow [HtmlLabelCell [] $ stateHtmlLabel s1], HtmlRow [HtmlLabelCell [] $ stateHtmlLabel s2]]
stateHtmlLabel (SetState sts) = HtmlTable $ HTable Nothing [] [HtmlRow $ foldl (\cells s -> (HtmlLabelCell [] $ stateHtmlLabel s) : cells) [] sts]

type HistoryStateMap = Map.Map Integer State

data AutomatonHistory =
  NoHistory
  | Rename HistoryStateMap AutomatonHistory
  | Minimize AutomatonHistory
  | Power AutomatonHistory
  | Product AutomatonHistory AutomatonHistory
  deriving Show

type StateStructureMap = Map.Map Integer State

instance Observable AutomatonHistory where
  observer = observeBase

instance (Show a, Show b) => Observable (Map a b) where
  observer = observeBase

pBool :: Parser Bool
pBool = (const True) <$> pSymbol "True" <|> (const False) <$> pSymbol "False"

historyP :: Parser AutomatonHistory
historyP = noHistoryP <|> renameP <|> minimizeP <|> powerP <|> pProduct

noHistoryP :: Parser AutomatonHistory
noHistoryP = (const NoHistory) <$> pSymbol "none"

minimizeP :: Parser AutomatonHistory
minimizeP = Minimize <$> (pSymbol "minimize" *> (pParens historyP))

powerP :: Parser AutomatonHistory
powerP = Power <$> (pSymbol "power" *> (pParens historyP))

pProduct :: Parser AutomatonHistory
pProduct = pSymbol "product" *> (pParens $ Product <$> historyP <*> (pComma *> historyP))

pState :: Parser State
pState = simpleState <|> productState <|> setState
  where
    simpleState :: Parser State
    simpleState = Simple <$> ((NatLab <$> pNatural) <|> (StrLab <$> (lexeme $ pParentheticalString '%')))
    productState :: Parser State
    productState = pParens $ ProductState <$> pState <*> (pComma *> (pState <??> pOptLevel))
    pOptLevel :: Parser (State -> State)
    pOptLevel = (const id) <$> pComma <* pBool
    setState :: Parser State
    setState = (SetState . Set.fromList) <$> pBraces (pListSep pComma pState)

renameP :: Parser AutomatonHistory
renameP = pSymbol "rename" *> pParens renameCont
  where
    renameCont = (flip Rename) <$> historyP <*> (pSymbol "," *> renameMapP)
    renameMapP :: Parser HistoryStateMap
    renameMapP = Map.fromList <$> pBraces (pListSep pComma mapEntry)
    mapEntry :: Parser (Integer, State)
    mapEntry = (flip (,)) <$> pState <*> (pSymbol "->" *> pNatural) -- mapping is expected to be a bijection

parseStateHistory :: String -> AutomatonHistory
parseStateHistory = runParser "history" historyP

{-
renameState :: Map.Map Integer State -> State -> State
renameState m (Simple (NatLab s)) = m ! s
renameState _s s@(Simple (StrLab _)) = s -- not further renamable
renameState m (ProductState s1 s2) = ProductState (renameState m s1) (renameState m s2)
renameState m (SetState sts) = SetState $ Set.map (renameState m) sts
-}


renameState :: Map.Map Integer State -> StateName -> State
renameState m (NatLab s) = m ! s
renameState _ s@(StrLab _) = Simple s -- not further renamable


{-
getProductStates :: State -> (State, State)
getProductStates (ProductState s1@(Simple _) s2) = (s1, getProductState s2)
getProductStates (ProductState s1 s2@(Simple _)) = (getProductState s1, s2)
-- getProductStates (ProductState s1 s2) =
getProductStates (SetState sts) = (SetState . Set.map fst) &&& (SetState . Set.map snd) $ Set.map getProductStates sts
getProductStates s = error $ "Expecting product state while renaming product history, got " ++ show s
-}

-- type StateMap = Map State State

mergeHistory :: StateStructureMap -> AutomatonHistory -> StateStructureMap
mergeHistory m hist = Map.map (stateHistory hist) m

stateHistory :: AutomatonHistory -> State -> State
stateHistory NoHistory s = s
stateHistory (Rename m h) (Simple s) = stateHistory h (renameState m s)
stateHistory (Minimize h) (SetState s) = SetState $ Set.map (stateHistory h) s
stateHistory (Power h) (SetState s) = SetState $ Set.map (stateHistory h) s
stateHistory (Product h1 h2) (ProductState s1 s2) = ProductState (stateHistory h1 s1) (stateHistory h2 s2)
stateHistory h s = error $ "Invalid combination of state type and history. State: " ++ (show s) ++ ", History: " ++ (show h)

{-
productMap :: StateMap -> StateMap -> StateMap
productMap m1 m2 = Map.foldlWithKey (\m s1 s1' ->
                    Map.foldlWithKey (\m' s2 s2' ->
                      Map.insert (ProductState s1 s2) (ProductState s1' s2') m') m m2) Map.empty m1

mergeHistory :: AutomatonHistory -> StateMap
mergeHistory NoHistory = Map.empty
mergeHistory (Rename m h) =
  let m1 = observe "before rename" $ invert $ (mergeHistory h)
      m2 = Map.toAscList m
  in observe "after rename" $ foldl (mergeName m1) Map.empty m2
  where
    mergeName :: StateMap -> StateMap -> (State, Integer) -> StateMap
    mergeName m1 m' (f,t) = Map.insert (fromMaybe f (Map.lookup f m1)) (Simple $ NatLab t) m'
mergeHistory (Minimize h) = mergeHistory h
mergeHistory (Power h) = mergeHistory h
mergeHistory (Product h1 h2) =
  let m1 = mergeHistory h1
      m2 = mergeHistory h2
  in productMap m1 m2
-}

-- TODO: history for set states has to be implemented.
-- For product states the names are generated, that is no longer
-- possible for set states. So this solution is not very good,
-- as it does not use the available state names.

{-
mergeHistory :: StateStructureMap -> AutomatonHistory -> StateStructureMap
mergeHistory m NoHistory = m
mergeHistory m (Rename m' h) = mergeHistory (Map.map (renameState m') m) h
mergeHistory m (Minimize h) = mergeHistory m h
mergeHistory m (Power h) = mergeHistory m h
mergeHistory m (Product h1 h2) =
  let ms = Map.map getProductStates $ observe "product map before" m
      m1 = Map.map fst ms
      m2 = Map.map snd ms
      m1' = mergeHistory m1 h1
      m2' = mergeHistory m2 h2
  in Map.map (renameProd m1' m2') m
  where
    -- prodToPair (ProductState s1 s2) = (s1, s2)
    -- prodToPair s = error $ "Expecting product state while renaming product history, got " ++ show s
    -- fstState = fst . prodToPair
    -- sndState = snd . prodToPair
    renameProd m1' m2' (ProductState s1 s2) = ProductState (renameState m1' s1) (renameState m2' s2)
    renameProd _ _ s = error $ "Expecting product state while renaming product history, got " ++ show s
-}

{-
invert :: Ord b => Map a b -> Map b a
invert = Map.fromList . (map swap) . Map.toList

makeStructureMap :: StateStructureMap -> StateMap -> StateStructureMap
makeStructureMap ms m = Map.map (m!) ms
-}

parseStateStructureMap :: StateStructureMap -> String -> StateStructureMap
parseStateStructureMap m = (mergeHistory m) . parseStateHistory
