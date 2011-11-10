import ScadeStateGraph
import ScadeOutParser as STrace

import Language.Scade.Parser
import Language.Scade.Lexer
import Language.Scade.Syntax (Declaration(UserOpDecl))
import StateNameRecovery

import Data.GraphViz.Types.Graph
import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Types
import Data.GraphViz.Attributes.Complete

import Data.Map as Map hiding (map)

import qualified Data.Graph.Inductive.Graph as Gr
import Data.Graph.Inductive.PatriciaTree
import Control.Arrow (first,second)

import Data.Text.Lazy.IO as LText (writeFile)
import Control.Monad

idMap = Map.fromList . (map duplicate)
  where duplicate x = (x, Simple $ NatLab x)

-- Special structure required: states are named "sti", where i € Nat.
parseNode :: String -> Integer
parseNode ('s':'t':i) = read i
parseNode "fail" = -1
parseNode n = error $ n ++ " invalid state name"

type NodeLabelI = (Integer,Bool)
type StateGraphI = Gr NodeLabelI EdgeLabel

relabelGraph :: StateGraph -> StateGraphI
relabelGraph = Gr.nmap (first parseNode)

relabelSteps :: Steps String -> Steps Integer
relabelSteps = map relabelStep
  where
    relabelStep (StepData i o s t) = StepData i o (map relabelState s) (map (first3 relabelState) t)
    relabelState = second parseNode
    first3 f (x1,x2,x3) = (f x1, x2, x3)

instance Labellable State where
  toLabelValue = toLabelValue . show

renderParams :: StateStructureMap -> StateGraphI -> Int -> StepData Integer -> GraphvizParams Gr.Node NodeLabelI EdgeLabel String NodeLabelI
renderParams m g i step = defaultParams {
    globalAttributes = (GraphAttrs [RankDir FromLeft]) : globalAttributes nonClusteredParams
    , clusterBy = \(n,nl) -> C ("Step " ++ show i) $ N (n,nl)
    , fmtNode = \(_,(n,_)) -> highlightNode n ++ [toLabel (m!n)]
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
  let steps = relabelSteps $ parseScadeOutput stepsStr
  nameMapStr <- readFile "train-minimalStrassenSignal_StrassenSignal-statemap.txt"
  let h = parseStateHistory nameMapStr
  let sMap = mergeHistory (Map.insert (-1) (Simple $ StrLab "fail") $ idMap [0,1]) h
  print h
  print sMap
  case stateGraph of
    Just (sg,sm) ->
      let sgi = relabelGraph sg
      in renderAll sMap sgi steps
    Nothing -> print "No automaton found"
  where
    renderAll sMap sg = foldM_ (\ i step -> do
      let dotParams = renderParams sMap sg i step
      void $ runGraphviz (graphToDot dotParams sg) Svg ("train-minimal-StrassenSignal_StrassenSignal-" ++ show i ++ ".svg")
      return (i+1))
      1
