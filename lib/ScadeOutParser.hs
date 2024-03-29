{-# LANGUAGE FlexibleContexts #-}

module ScadeOutParser (
  StepData(..)
  , Steps
  , parseScadeOutput
  --, parseScadeOutputStep
) where

import Text.ParserCombinators.UU as UU hiding (Steps)
import Text.ParserCombinators.UU.Utils as UU (pLetter, pDigit, lexeme, pSymbol, pNatural, pSpaces, runParser)
import Text.ParserCombinators.UU.BasicInstances as UU (Parser, pSym)

import Data.List (intercalate)

newtype Path = Path [String]
type Input = (Path, String) -- (path, value)
type Output = (Path, String) -- (path, value)
type State a = (Path, a) -- (path, state name)

data TransitionType = Strong | Weak deriving Show
type Transition a = (State a, Int, TransitionType) -- (state path, number, type)

data StepData a = StepData {
  stepInputs :: [Input]
  , stepOutputs :: [Output]
  , stepStates :: [State a] -- active states
  , stepTransitions :: [Transition a]
}

type Steps a = [StepData a]

instance Show Path where
  show (Path p) = intercalate "/" p

instance Show a => Show (StepData a) where
  show d =
    "Step: \n" ++
    "inputs: \n  " ++ (intercalate "\n  " (map showAssign $ stepInputs d)) ++
    "\noutputs: \n  " ++ (intercalate "\n  " (map showAssign $ stepOutputs d)) ++
    "\nactive states: \n  " ++ (intercalate "\n  " (map show $ stepStates d)) ++
    "\nactive transitions: \n  " ++ (intercalate "\n  " (map show $ stepTransitions d))
    where
      showAssign (v, x) = show v ++ " = " ++ show x

-----
-- Base parser

-- | Parse an alphabetic letter or a digit
pAlphaNum :: Parser Char
pAlphaNum = pDigit <|> pLetter


-- We are going to parse states named by strings:
type RawState = State String
type RawTrans = Transition String
type RawStepData = StepData String
type RawStepDataEntry = StepDataEntry String
type RawSteps = Steps String

----

identifier :: Parser String
identifier = lexeme $ pList1 (pAlphaNum <|> pSym '_')

value :: Parser String
value = identifier

path :: Parser Path
path = Path <$> pList1Sep_ng (pSym '/') identifier

input :: Parser Input
input = (const (,)) <$> pSymbol "INPUT" <*> path <* pSymbol "=" <*> value

output :: Parser Input
output = (const (,)) <$> pSymbol "OUTPUT" <*> path <* pSymbol "=" <*> value

statePath :: Parser RawState
statePath = (,) <$> path <* pSym '/' <*> stateName
  where
    stateName :: Parser String
    stateName = pPacked (pSym ':') (pSym ':') identifier


state :: Parser (Maybe RawState)
state = (\s a -> if a then Just s else Nothing) <$>
        (pSymbol "STATE" *> statePath) <*>
        (pSpaces *> pSymbol "=" *>
          (pSymbol "<Active>" *> pure True
          <|> pSymbol "<Not Active>" *> pure False)
        )

transition :: Parser (Maybe RawTrans)
transition = (\t s n a -> if a then Just (s, n, t) else Nothing) <$>
              transType <*
              pSymbol "TRANSITION" <*>
              statePath <*>
              transNumber <*
              pSym ':' <*>
              (pSpaces *> pSymbol "=" *>
                (pSymbol "<Fired>" *> pure True
                <|> pSymbol "<Not Fired>" *> pure False)
              )
  where
    transType =
      const Strong <$> pSymbol "STRONG"
      -- <|> const Weak <$> pSymbol "WEAK"
    transNumber = pPacked (pSym '<') (pSym '>') pNatural

data StepDataEntry a = Inp Input | Outp Output | St (State a) | Tr (Transition a) | NoEntry
putEntry :: StepDataEntry a -> StepData a -> StepData a
putEntry (Inp x) d = d { stepInputs = x : stepInputs d }
putEntry (Outp x) d = d { stepOutputs = x : stepOutputs d }
putEntry (St x) d = d { stepStates = x : stepStates d }
putEntry (Tr x) d = d { stepTransitions = x : stepTransitions d }
putEntry NoEntry d = d

step :: Parser RawStepData
step = pSymbol "STEP" *> pNat *> stepContent
  where
    stepContent :: Parser RawStepData
    stepContent = pFoldr (putEntry, StepData [] [] [] []) stepEntry

    stepEntry :: Parser RawStepDataEntry
    stepEntry =
      (Inp <$> input)
      <|> (Outp <$> output)
      <|> (maybe NoEntry St <$> state)
      <|> (maybe NoEntry Tr <$> transition)

    pNat :: Parser Integer
    pNat = pNatural

steps :: Parser RawSteps
steps = pList step

-- parseScadeOutputStep :: String -> (StepData, [Error LineColPos])
-- parseScadeOutputStep = parse ((,) <$> step <*> pEnd) . createStr (LineColPos 0 0 0)
-- parseScadeOutputStep = runParser "scade output step" step
-- parseScadeOutputStep = run step

parseScadeOutput :: String -> RawSteps
parseScadeOutput = runParser "scade output" steps
