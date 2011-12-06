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

import Data.Graph.Inductive.Graph as Gr (Node, lab, nmap, ufold, empty, (&), Context)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Control.Arrow (first,second)

import Control.Monad (foldM_, void)

import System.Environment (getArgs)
import System.FilePath (dropExtension, addExtension)

import Debug.Hood.Observe

idMap :: [Integer] -> StateStructureMap
idMap = Map.fromList . (map duplicate)
  where duplicate x = (x, Simple $ NatLab x)

makeBaseNodeStructureMap :: StateGraphI -> StateStructureMap
makeBaseNodeStructureMap = idMap . filter (/= -1) . (Gr.ufold (\(_,_,(st,_),_) sts -> st:sts) [])

addFailState :: StateStructureMap -> StateStructureMap
addFailState = Map.insert (-1) (Simple $ StrLab "fail")

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

activeContext :: StepData Integer -> StateGraphI -> StateGraphI
activeContext step = Gr.ufold (\context g -> if someActive context then context & g else g) Gr.empty
  where
    someActive :: Context NodeLabelI EdgeLabel -> Bool
    someActive (i, _, (s, _), o) =
          (any (\(_,s') -> s == s') $ stepStates step)
          || any (\((t, _), _) -> any (\((_,s'), t', _) -> s == s' && t == t') $ stepTransitions step) i
          || any (\((t, _), _) -> any (\((_,s'), t', _) -> s == s' && t == t') $ stepTransitions step) o


m00 :: StateStructureMap
m00 = Map.fromList [(-1, Simple $ StrLab "fail"),(0, Simple $ NatLab 0),(1, Simple $ NatLab 1),(2, Simple $ NatLab 2),(3, Simple $ NatLab 3),(4, Simple $ NatLab 4),(5, Simple $ NatLab 5),(6, Simple $ NatLab 6),(7, Simple $ NatLab 7),(8, Simple $ NatLab 8),(9, Simple $ NatLab 9),(10, Simple $ NatLab 10),(11, Simple $ NatLab 11),(12, Simple $ NatLab 12),(13, Simple $ NatLab 13),(14, Simple $ NatLab 14),(15, Simple $ NatLab 15),(16, Simple $ NatLab 16),(17, Simple $ NatLab 17),(18, Simple $ NatLab 18),(19, Simple $ NatLab 19),(20, Simple $ NatLab 20),(21, Simple $ NatLab 21),(22, Simple $ NatLab 22),(23, Simple $ NatLab 23),(24, Simple $ NatLab 24),(25, Simple $ NatLab 25),(26, Simple $ NatLab 26),(27, Simple $ NatLab 27),(28, Simple $ NatLab 28),(29, Simple $ NatLab 29),(30, Simple $ NatLab 30),(31, Simple $ NatLab 31),(32, Simple $ NatLab 32),(33, Simple $ NatLab 33),(34, Simple $ NatLab 34),(35, Simple $ NatLab 35),(36, Simple $ NatLab 36),(37, Simple $ NatLab 37),(38, Simple $ NatLab 38),(39, Simple $ NatLab 39),(40, Simple $ NatLab 40),(41, Simple $ NatLab 41),(42, Simple $ NatLab 42),(43, Simple $ NatLab 43),(44, Simple $ NatLab 44),(45, Simple $ NatLab 45),(46, Simple $ NatLab 46),(47, Simple $ NatLab 47)]
p0 :: IO()
p0 = do
  s <- readFile "train-minimal-StrassenSignal_StrassenSignal-statemap.txt"
  print $ parseStateStructureMap m00 s

main :: IO ()
main = runO $ do
-- main = do
  (gtlFile:_) <- getArgs
  -- let file = dropExtension "train-minimal-StrassenSignal_StrassenSignal.scade"
  let file = dropExtension gtlFile
  str <- readFile $ addExtension file ".scade"
  let decls = scade $ alexScanTokens str
  let (UserOpDecl _ _ _ opName _ _ _ _ opCont) = head decls
  let stateGraph = makeStateGraph opCont
  stepsStr <- readFile $ file ++ "-proof-counterex.out"
  let steps = relabelSteps $ parseScadeOutput stepsStr
  nameMapStr <- readFile $ file ++ "-statemap.txt"
  case stateGraph of
    Just (sg,_) ->
      let sgi = relabelGraph sg
          m0 = makeBaseNodeStructureMap sgi
          sMap = addFailState $ parseStateStructureMap m0 nameMapStr
      in print sMap >> renderAll file opName sMap sgi steps
    Nothing -> print "No automaton found"
  where
    renderAll file op sMap sg = foldM_ (\ i step -> do
      let contextSg = activeContext step sg
      print $ labNodes contextSg
      let dotParams = renderParams op sMap contextSg i step
      void $ runGraphviz (graphToDot dotParams $ contextSg) Svg (file ++ show i ++ ".svg")
      return (i+1))
      1
