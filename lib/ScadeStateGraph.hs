module ScadeStateGraph (makeStateGraph) where

import Language.Scade.Syntax
import Language.Scade.Pretty (prettyExpr)
-- import Data.GraphViz.Types.Graph as DotGr
import Data.Graph.Inductive.Graph as Gr
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Attributes (Attributes, toLabel, style, bold)
-- import Data.Text.Lazy as LText ()
-- import Data.Text.Lazy.Builder (toLazyText, fromString)
-- import Data.Set as Set hiding (map)
import Data.Map as Map hiding (map, union)

type StateGraph = Gr Attributes Attributes

makeStateGraph :: String -> DataDef -> Maybe StateGraph
makeStateGraph name def =
  let e:[] = dataEquations def
  in case e of
    StateEquation m _ _ -> Just $ stateGraph m
    _ -> Nothing

stateGraph :: StateMachine -> StateGraph
stateGraph (StateMachine name states) =
  let cluster = undefined -- fmap (DotGr.Str . toLazyText . fromString) name
  in fst $ foldl makeContext (Gr.empty, Map.empty) states
  where
    makeContext :: (StateGraph, Map String Node) -> State -> (StateGraph, Map String Node)
    makeContext (g,sm) s =
      let node = stateName s
          transitions = stateUnless s -- only strong transitions supported
          (g', sm') = insertNode node g sm
          g'' = setInitial (sm'!node) (stateInitial s) g'
      in foldl (insertTransition $ sm' ! node) (g'', sm') transitions

    insertNode n g sm =
      if not (Map.member n sm) then
        let sm' = Map.insert n (head $ newNodes 1 g) sm
        in (insNode ((sm' ! n),[toLabel n]) g, sm')
      else (g,sm)

    insertTransition n (g, sm) (Transition expr _ fork) =
      let target = getNode fork
          (g', sm') = insertNode target g sm
      in (insEdge (n, sm' ! target, [toLabel $ show $ prettyExpr 15 expr]) g', sm')

    getNode (TargetFork _ n) = n -- history ignored / conditional not supported

    setInitial n i g =
      if i then
        let (Just (t, _, l, f), r) = Gr.match n g
        in (t, n, style bold : l, f) & g
      else g

{-
stateGraph :: StateMachine -> DotGraph String
stateGraph (StateMachine name states) =
  let cluster = fmap (DotGr.Str . toLazyText . fromString) name
      (nodes, edges) = foldl (makeGraph cluster) (Map.empty, Set.empty) states
  in mkGraph (toNodeList nodes) (Set.toList edges)
  where
    toNodeList ns = map (\(n, a) -> DotNode n a) $ Map.toAscList ns
    makeGraph :: Maybe GraphID -> (Map String Attributes, Set (DotEdge String)) -> State -> (Map String Attributes, Set (DotEdge String))
    makeGraph c (ns, es) s =
      let node = stateName s
          transitions = map (transitionToEdge node) (stateUnless s) -- only strong transitions supported
          attrs = (isInitial s) ++ []
      in (Map.alter (const $ Just attrs) node ns, es `union` (Set.fromList transitions))
      {-
    makeContext c s =

      in Cntxt {
          node = stateName s
          , inCluster = c
          , attributes = attrs
          , predecessors = []
          , successors = transitions
        }
      -}
      where
        transitionToEdge n1 (Transition expr _ fork) = DotEdge n1 (getNode fork) [toLabel $ show $ prettyExpr 15 expr]
        getNode (TargetFork _ n) = n -- history ignored / conditional not supported
        isInitial s = if stateInitial s then [style bold] else []
-}
