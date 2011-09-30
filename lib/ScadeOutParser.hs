module ScadeOutParser where

import Data.Char
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import Text.ParserCombinators.UU.Demo.Examples (run)

type Path = [String]
type Input = (Path, String) -- (path, value)
type Output = (Path, String) -- (path, value)
type State = Path

data TransitionType = Strong | Weak
type Transition = (Path, TransitionType)

data StepData = StepData {
  stepInputs :: [Input]
  , stepOutputs :: [Output]
  , stepStates :: [State] -- active states
  , stepTransitions :: [Transition]
}

type Steps = [StepData]

input = (,) <$> 
