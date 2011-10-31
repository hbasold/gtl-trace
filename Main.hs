import ScadeStateGraph
import Language.Scade.Parser
import Language.Scade.Lexer
import Language.Scade.Syntax

import Data.GraphViz.Types.Graph
--import qualified Data.GraphViz.Types.Canonical as Canonical
import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Types

--import Data.Map as Map hiding (map)

import Data.Text.Lazy.IO as LText (writeFile)

{-
-- original version reverses edges, don't know why
toCanonical :: DotGraph -> Canonical.DotGraph
toCanonical g = Canonical.DotGraph False True (graphId g) (Canonical.DotStmts [] [] nodes edges)
  where
    vs = values g
    nodes = map (\(n,i) -> Canonical.DotNode n (_attributes i)) $ Map.toAscList vs
-}

main = do
  str <- readFile "train-minimal-StrassenSignal_StrassenSignal.scade"
  let decls = scade $ alexScanTokens str
  let (UserOpDecl _ _ _ opName _ _ _ _ opCont) = head decls
  let stateGraph = makeStateGraph opName opCont
  let dotParams = nonClusteredParams { fmtNode = snd, fmtEdge = \(_,_,attrs) -> attrs }
  case stateGraph of
    Just sg -> do
      -- print $ edgeInformation False sg
      -- print $ toCanonical sg
      let dot = toDot $ graphToDot dotParams sg
      LText.writeFile "train-minimal-StrassenSignal_StrassenSignal.dot" $ renderDot dot
    Nothing -> print "No automaton found"
