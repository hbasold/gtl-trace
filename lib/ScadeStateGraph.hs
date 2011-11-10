module ScadeStateGraph (
  NodeLabel
  , EdgeLabel
  , StateGraph
  , makeStateGraph
) where

import Language.Scade.Syntax
import Language.Scade.Pretty (prettyExpr)
import Data.Graph.Inductive.Graph as Gr (Node, empty, newNodes, insNode, insEdge, match, (&))
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Map as Map hiding (map, union)

type NodeLabel = (String,Bool)
type EdgeLabel = (Int,String)
type StateGraph = Gr NodeLabel EdgeLabel

makeStateGraph :: DataDef -> Maybe (StateGraph, Map String Node)
makeStateGraph def =
  let e:[] = dataEquations def
  in case e of
    StateEquation m _ _ -> Just $ stateGraph m
    _ -> Nothing

stateGraph :: StateMachine -> (StateGraph, Map String Node)
stateGraph (StateMachine _ states) = foldl makeContext (Gr.empty, Map.empty) states
  where
    makeContext :: (StateGraph, Map String Node) -> State -> (StateGraph, Map String Node)
    makeContext (g,sm) s =
      let node = stateName s
          transitions = stateUnless s -- only strong transitions supported
          (g', sm') = insertNode node g sm
          g'' = setInitial (sm'!node) (stateInitial s) g'
      in (\(x,y,_) -> (x,y)) $ foldl (insertTransition $ sm' ! node) (g'', sm', 1) transitions

    insertNode n g sm =
      if not (Map.member n sm) then
        let sm' = Map.insert n (head $ newNodes 1 g) sm
        in (insNode ((sm' ! n),(n,False)) g, sm')
      else (g,sm)

    insertTransition n (g, sm, i) (Transition expr _ fork) =
      let target = getNode fork
          (g', sm') = insertNode target g sm
      in (insEdge (n, sm' ! target, (i,show $ prettyExpr 15 expr)) g', sm', i+1)

    getNode (TargetFork _ n) = n -- history ignored / conditional not supported
    getNode _ = error "History and conditional not supported"

    setInitial n i g =
      if i then
        let (Just (t, _, (l,_), f), _) = Gr.match n g
        in (t, n, (l,True), f) & g
      else g
