import ScadeStateGraph
import ScadeOutParser as STrace

import Language.Scade.Parser
import Language.Scade.Lexer (alexScanTokens)
import Language.Scade.Syntax (Declaration(UserOpDecl))
import StateNameRecovery

import Data.GraphViz (GraphvizParams(..), defaultParams, nonClusteredParams, NodeCluster(..), toLabel, color, runGraphviz, graphToDot, GraphvizOutput(Svg))
import Data.GraphViz.Types (GlobalAttributes(..))
import Data.GraphViz.Attributes.Complete (Attribute(RankDir), RankDir(FromLeft), X11Color(Blue))

import Data.Map as Map ((!), insert, fromList)

import qualified Data.Graph.Inductive.Graph as Gr (Node, lab, nmap)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Control.Arrow (first,second)

import Control.Monad (foldM_, void)

import System.Environment (getArgs)
import System.FilePath (dropExtension, addExtension)

idMap :: [Integer] -> StateStructureMap
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

renderParams :: String -> StateStructureMap -> StateGraphI -> Int -> StepData Integer -> GraphvizParams Gr.Node NodeLabelI EdgeLabel String NodeLabelI
renderParams op m g i step = defaultParams {
    globalAttributes = (GraphAttrs [RankDir FromLeft]) : globalAttributes nonClusteredParams
    , clusterBy = \(n,nl) -> C (op ++ ": Step " ++ show i) $ N (n,nl)
    , fmtNode = \(_,(n,_)) -> highlightNode n ++ [toLabel (m!n)]
    , fmtEdge = \(f,_,(t,expr)) -> highlightTransition f t ++ [toLabel expr] }
  where
    -- TODO: respect path
    highlightNode n = if any (\(_,n') -> n == n') $ stepStates step then [color Blue] else []
    highlightTransition f t = if any (\((_,s), t', _) -> (maybe False (s ==) (fmap fst $ Gr.lab g f)) && t == t') $ stepTransitions step then [color Blue] else []

main :: IO ()
main = do
  (gtlFile:_) <- getArgs
  let file = dropExtension gtlFile
  str <- readFile $ addExtension file ".scade"
  let decls = scade $ alexScanTokens str
  let (UserOpDecl _ _ _ opName _ _ _ _ opCont) = head decls
  let stateGraph = makeStateGraph opCont
  stepsStr <- readFile $ file ++ "-proof-counterex.out"
  let steps = relabelSteps $ parseScadeOutput stepsStr
  nameMapStr <- readFile $ file ++ "-statemap.txt"
  let sMap = parseStateStructureMap (Map.insert (-1) (Simple $ StrLab "fail") $ idMap [0,1,2]) nameMapStr
  case stateGraph of
    Just (sg,_) ->
      let sgi = relabelGraph sg
      in renderAll file opName sMap sgi steps
    Nothing -> print "No automaton found"
  where
    renderAll file op sMap sg = foldM_ (\ i step -> do
      let dotParams = renderParams op sMap sg i step
      void $ runGraphviz (graphToDot dotParams sg) Svg (file ++ show i ++ ".svg")
      return (i+1))
      1
