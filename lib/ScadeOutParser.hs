{-# LANGUAGE FlexibleContexts #-}

module ScadeOutParser (
  StepData(..),
  Steps,
  parseScadeOutput,
  parseScadeOutputStep
) where

import Text.ParserCombinators.UU as UU hiding (Steps)
import Text.ParserCombinators.UU.Utils as UU (pLetter, pDigit, lexeme, pSymbol, pNatural, pSpaces, runParser)
import Text.ParserCombinators.UU.BasicInstances as UU (Parser, pSym)
import Text.ParserCombinators.UU.Demo.Examples (run)

import Data.List (intercalate)

newtype Path = Path [String]
type Input = (Path, String) -- (path, value)
type Output = (Path, String) -- (path, value)
type State = (Path, String) -- (path, state name)

data TransitionType = Strong | Weak deriving Show
type Transition = (State, Int, TransitionType) -- (state path, number, type)

data StepData = StepData {
  stepInputs :: [Input]
  , stepOutputs :: [Output]
  , stepStates :: [State] -- active states
  , stepTransitions :: [Transition]
}

type Steps = [StepData]

instance Show Path where
  show (Path p) = intercalate "/" p

instance Show StepData where
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

statePath :: Parser State
statePath = (,) <$> path <* pSym '/' <*> stateName
  where
    stateName :: Parser String
    stateName = pPacked (pSym ':') (pSym ':') identifier


state :: Parser (Maybe State)
state = (\s a -> if a then Just s else Nothing) <$>
        (pSymbol "STATE" *> statePath) <*>
        (pSpaces *> pSymbol "=" *>
          (pSymbol "<Active>" *> pure True
          <|> pSymbol "<Not Active>" *> pure False)
        )

transition :: Parser (Maybe Transition)
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

data StepDataEntry = Inp Input | Outp Output | St State | Tr Transition | NoEntry
putEntry (Inp x) d = d { stepInputs = x : stepInputs d }
putEntry (Outp x) d = d { stepOutputs = x : stepOutputs d }
putEntry (St x) d = d { stepStates = x : stepStates d }
putEntry (Tr x) d = d { stepTransitions = x : stepTransitions d }
putEntry NoEntry d = d

step :: Parser StepData
step = pSymbol "STEP" *> pNatural *> stepContent
  where
    stepContent :: Parser StepData
    stepContent = pFoldr (putEntry, StepData [] [] [] []) stepEntry

    stepEntry :: Parser StepDataEntry
    stepEntry =
      (Inp <$> input)
      <|> (Outp <$> output)
      <|> (maybe NoEntry St <$> state)
      <|> (maybe NoEntry Tr <$> transition)

steps :: Parser Steps
steps = pList step

--parseScadeOutputStep = runParser "scade output step" step
parseScadeOutputStep = run step

parseScadeOutput :: String -> Steps
parseScadeOutput = runParser "scade output" steps
