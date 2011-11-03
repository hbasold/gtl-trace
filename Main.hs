import ScadeStateGraph
import ScadeOutParser as STrace

import Language.Scade.Parser
import Language.Scade.Lexer
import Language.Scade.Syntax
import StateNameRecovery

import Data.GraphViz.Types.Graph
import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Types
import Data.GraphViz.Attributes.Complete

import Data.Map as Map

import qualified Data.Graph.Inductive.Graph as Gr

--import Data.Map as Map hiding (map)

import Data.Text.Lazy.IO as LText (writeFile)
import Control.Monad

renderParams :: StateGraph -> Int -> StepData -> GraphvizParams Gr.Node NodeLabel EdgeLabel String NodeLabel
renderParams g i step = defaultParams {
    globalAttributes = (GraphAttrs [RankDir FromLeft]) : globalAttributes nonClusteredParams
    , clusterBy = \(n,nl) -> C ("Step " ++ show i) $ N (n,nl)
    , fmtNode = \(_,(n,_)) -> highlightNode n ++ [toLabel n]
    , fmtEdge = \(f,_,(t,expr)) -> highlightTransition f t ++ [toLabel expr] }
  where
    -- TODO: respect path
    highlightNode n = if any (\(_,n') -> n == n') $ stepStates step then [color Blue] else []
    highlightTransition f t = if any (\((_,s), i, _) -> (maybe False (s ==) (fmap fst $ Gr.lab g f)) && t == i) $ stepTransitions step then [color Blue] else []

main = do
  str <- readFile "train-minimal-StrassenSignal_StrassenSignal.scade"
  let decls = scade $ alexScanTokens str
  let (UserOpDecl _ _ _ opName _ _ _ _ opCont) = head decls
  let stateGraph = makeStateGraph opName opCont
  stepsStr <- readFile "train-minimal-StrassenSignal_StrassenSignal-proof-counterex.out"
  let steps = parseScadeOutput stepsStr
  nameMapStr <- readFile "train-minimalStrassenSignal_StrassenSignal-statemap.txt"
  let h = parseStateHistory nameMapStr
  print h
  print $ mergeHistory (Map.fromList [("st0","0"), ("st1", "1")]) h
  case stateGraph of
    Just sg -> foldM_ (\ i step -> do
      let dotParams = renderParams sg i step
      void $ runGraphviz (graphToDot dotParams sg) Svg ("train-minimal-StrassenSignal_StrassenSignal-" ++ show i ++ ".svg")
      return (i+1))
      1 steps
    Nothing -> print "No automaton found"
