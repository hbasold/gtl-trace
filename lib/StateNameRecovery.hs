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
import qualified Data.Set as Set (Set(..), toAscList, fromList, map)
import Data.Foldable (foldl)

import Text.ParserCombinators.UU as UU hiding (Steps)
import Text.ParserCombinators.UU.Utils as UU (pSymbol, pParens, pBraces, pComma, lexeme, pParentheticalString, pNatural, runParser)
import Text.ParserCombinators.UU.BasicInstances as UU (Parser)

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
        str = if null stL then "" else foldl (\str s -> str ++ "," ++ show s) (show $ head stL) (tail stL)
    in "{" ++ str ++ "}"

data AutomatonHistory =
  NoHistory
  | Rename (Map.Map Integer State) AutomatonHistory
  | Minimize AutomatonHistory
  | Power AutomatonHistory
  | Product AutomatonHistory AutomatonHistory
  deriving Show

type StateStructureMap = Map.Map Integer State

historyP :: Parser AutomatonHistory
historyP = noHistoryP <|> renameP <|> minimizeP <|> powerP

noHistoryP :: Parser AutomatonHistory
noHistoryP = (const NoHistory) <$> pSymbol "none"

minimizeP :: Parser AutomatonHistory
minimizeP = Minimize <$> (pSymbol "minimize" *> (pParens historyP))

powerP :: Parser AutomatonHistory
powerP = Power <$> (pSymbol "power" *> (pParens historyP))

renameP :: Parser AutomatonHistory
renameP = pSymbol "rename" *> pParens renameCont
  where
    renameCont = (flip Rename) <$> historyP <*> (pSymbol "," *> renameMapP)
    renameMapP :: Parser (Map.Map Integer State)
    renameMapP = Map.fromList <$> pBraces (pListSep pComma mapEntry)
    mapEntry :: Parser (Integer,State)
    mapEntry = (flip (,)) <$> state <*> (pSymbol "->" *> pNatural) -- mapping is expected to be a bijection
    state :: Parser State
    state = setState <|> simpleState
    simpleState :: Parser State
    simpleState = Simple <$> ((NatLab <$> pNatural) <|> (StrLab <$> (lexeme $ pParentheticalString '%')))
    setState :: Parser State
    setState = (SetState . Set.fromList) <$> pBraces (pListSep pComma state)

parseStateHistory :: String -> AutomatonHistory
parseStateHistory = runParser "history" historyP

renameState :: Map.Map Integer State -> State -> State
renameState m (Simple (NatLab s)) = m ! s
renameState m s@(Simple (StrLab _)) = s -- not further renamable
renameState m (ProductState s1 s2) = ProductState (renameState m s1) (renameState m s2)
renameState m (SetState sts) = SetState $ Set.map (renameState m) sts

mergeHistory :: Map.Map Integer State -> AutomatonHistory -> Map.Map Integer State
mergeHistory m NoHistory = m
mergeHistory m (Rename m' h) = mergeHistory (Map.map (renameState m') m) h
mergeHistory m (Minimize h) = mergeHistory m h
mergeHistory m (Power h) = mergeHistory m h
mergeHistory m (Product h1 h2) = mergeHistory m h1 -- TODO

parseStateStructureMap m = (mergeHistory m) . parseStateHistory
