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

data StateName = NatLab Integer | StrLab String deriving (Eq, Ord)
data State =
  Simple StateName
  | ProductState [State]
  | SetState (Set.Set State)
  deriving (Eq,Ord)

instance Show StateName where
  show (NatLab i) = show i
  show (StrLab s) = s

instance Show State where
  show (Simple s) = show s
  show (ProductState sts) = "(" ++ (intercalate "," $ map show sts) ++ ")"
  show (SetState sts) =
    let stL = Set.toAscList sts
        str = if null stL then "" else (intercalate ",") $ map show stL
    in "{" ++ str ++ "}"

instance Labellable State where
  toLabelValue s = HtmlLabel $ stateHtmlLabel s


stateHtmlLabel :: State -> HtmlLabel
stateHtmlLabel (Simple s) = HtmlText [HtmlStr $ pack $ show s]
stateHtmlLabel (ProductState sts) = HtmlTable $ HTable Nothing [HtmlBorder 0, HtmlCellPadding 5] $ map (\s -> HtmlRow [HtmlLabelCell [HtmlBorder 1] $ stateHtmlLabel s]) sts
stateHtmlLabel (SetState sts) = HtmlTable $ HTable Nothing [HtmlBorder 0, HtmlCellPadding 5] [HtmlRow $ foldl (\cells s -> (HtmlLabelCell [HtmlBorder 1] $ stateHtmlLabel s) : cells) [] sts]

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
    productState = pParens $ (\s1 s2 -> ProductState [s1, s2]) <$> pState <*> (pComma *> (pState <??> pOptLevel))
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

renameState :: Map.Map Integer State -> StateName -> State
renameState m (NatLab s) = m ! s
renameState _ s@(StrLab _) = Simple s -- not further renamable

mergeHistory :: StateStructureMap -> AutomatonHistory -> StateStructureMap
mergeHistory m hist = Map.map (stateHistory hist) m

stateHistory :: AutomatonHistory -> State -> State
stateHistory NoHistory s = s
stateHistory (Rename m h) (Simple s) = stateHistory h (renameState m s)
stateHistory (Minimize h) (SetState s) = SetState $ Set.map (stateHistory h) s
stateHistory (Power h) (SetState s) = SetState $ Set.map (stateHistory h) s
stateHistory (Product h1 h2) (ProductState [s1, s2]) = ProductState [(stateHistory h1 s1), (stateHistory h2 s2)]
stateHistory h s = error $ "Invalid combination of state type and history. State: " ++ (show s) ++ ", History: " ++ (show h)

flattenProductState :: State -> State
flattenProductState = (\sts -> case sts of {[s] -> s; _ -> ProductState sts}) . flattenProductState'
  where
    flattenProductState' :: State -> [State]
    flattenProductState' (ProductState [s1, s2]) = (flattenProductState' s1) ++ (flattenProductState' s2)
    flattenProductState' s@(Simple _) = [s]
    flattenProductState' (SetState sts) = [SetState $ Set.map flattenProductState sts]
    flattenProductState' (ProductState _) = error "invalid product state, should be pair after parsing"

flattenTopSetState :: State -> State
flattenTopSetState (SetState st) =
  case Set.toAscList st of
    [SetState sts] -> SetState sts
    _ -> (SetState st)
flattenTopSetState s = s

flattenStates :: StateStructureMap -> StateStructureMap
flattenStates = Map.map (flattenTopSetState . flattenProductState)

parseStateStructureMap :: StateStructureMap -> String -> StateStructureMap
parseStateStructureMap m = flattenStates . (mergeHistory m) . parseStateHistory
