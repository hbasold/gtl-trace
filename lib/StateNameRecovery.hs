{-# LANGUAGE FlexibleContexts, Rank2Types, ScopedTypeVariables #-}

module StateNameRecovery where

import Data.Map as Map

import Text.ParserCombinators.UU as UU hiding (Steps)
import Text.ParserCombinators.UU.Utils as UU (pSymbol, pParens, pBraces, pComma, lexeme, pParentheticalString, runParser)
import Text.ParserCombinators.UU.BasicInstances as UU (Parser)

data AutomatonHistory =
  NoHistory
  | Rename (Map String String) AutomatonHistory
  | Minimize AutomatonHistory
  | Power AutomatonHistory
  | Product AutomatonHistory AutomatonHistory
  deriving Show

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
    renameMapP = Map.fromList <$> pBraces (pListSep pComma mapEntry)
    mapEntryStr = lexeme $ pParentheticalString '%'
    -- reverse the parsed mapping
    mapEntry = (flip (,)) <$> mapEntryStr <*> (pSymbol "->" *> mapEntryStr)

parseStateHistory :: String -> AutomatonHistory
parseStateHistory = runParser "history" historyP

mergeHistory :: Map String String -> AutomatonHistory -> Map String String
mergeHistory m NoHistory = m
mergeHistory m (Rename m' h) = mergeHistory updateMap h
  where
    updateMap = Map.foldlWithKey (\m s s' -> Map.alter (const $ Just s') s m) m m'
mergeHistory m (Minimize h) = mergeHistory m h
mergeHistory m (Power h) = mergeHistory m h

