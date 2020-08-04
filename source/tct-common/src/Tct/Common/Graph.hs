-- | This module provides a unified \graph interface\.
module Tct.Common.Graph
    (
     -- * Unified Graph Interface
     -- ** Datatypes
    Graph
    , NodeId
    , empty
    , mkGraph
     -- | A node in the graph. Most operations work on @NodeId@s.
     -- Use @lookupNodeLabel@ to get the label of the node

     -- ** Operations
    , context
    -- | Returns the context of a given node.
    , nodes
    -- | Returns the set of nodes.
    , lnodes
    -- | Returns the set of nodes together with their labels.
     , roots
     -- | Returns the list of nodes without predecessor.
     , leafs
     -- | Returns the list of nodes without successor.
     , lookupNodeLabel
     -- | Returns the label of the given node, if present.
     , lookupNodeLabel'
     -- | Returns the label of the given node, if present.
     -- Otherwise an exception is raised
     , lookupNode
     -- | Returns the node with given label.
     , lookupNode'
     -- | Returns the node with given label.
     -- Otherwise an exception is raised
     , withNodeLabels
     -- | List version of @lookupNodeLabel@.
     , withNodeLabels'
     -- | List version of @lookupNodeLabel'@.
     , inverse
     -- | Returns the same graph with edges inversed.
     , topsort
     -- | returns a /topologically sorted/ set of nodes.
     , sccs
     -- | returns the list of /strongly connected components/, including trivial ones.
     , undirect
     -- | Make the graph undirected, i.e. for every edge from A to B, there exists an edge from B to A.
     , successors
    -- | Returns the list of successors in a given node.
     , reachablesDfs
    -- | @reachable gr ns@ closes the list of nodes @ns@ under
    -- the successor relation with respect to @ns@ in a depth-first manner.
     , reachablesBfs
    -- | @reachable gr ns@ closes the list of nodes @ns@ under
    -- the successor relation with respect to @ns@ in a breath-first manner.
     , lsuccessors
    -- | Returns the list of successors in a given node, including their labels.
     , predecessors
    -- | Returns the list of predecessors in a given node.
     , lpredecessors
    -- | Returns the list of predecessors in a given node, including their labels.
    , isEdgeTo
    -- | @isEdgeTo dg n1 n2@ checks wheter @n2@ is a successor of @n1@ in
    -- the dependency graph @dg@
    , isLEdgeTo
    -- | @isLEdgeTo dg n1 l n2@ checks wheter @n2@ is a successor of @n1@ in
    -- the dependency graph @dg@, where the edge from @n1@ to @n2@ is
    -- labeled by @l@.
    , subGraph
    -- | Computes the subgraph based on the given nodes.
    , removeNodes
    -- | Removes nodes.
    , removeEdges
    -- | Removes edges.
    , updateLabels
    -- | Update labels.

    -- -- ** Utilities
    -- , pprintCWDGNode
    --   -- | @pprintCWDGNode cdg sig vars node@ is a default printer for the
    --   -- CDG-node @node@. It shows the nodes of the dependency graph denoted by @node@  as a set.
    -- , pprintCWDG
    --   -- | Default pretty printer for CDGs. Prints the given CDG in a tree-like shape.
    -- , pprintNodeSet
    --   -- | Default pretty printer for set of nodes.
    -- , toXml
    --  -- | Xml representation of given 'DG'.
    -- , toGraphViz
    --   -- | translates 'DG' into a GraphViz graph.
    -- , saveGraphViz
    --   -- | output 'DG' as Svg.
    -- , graphVizSVG
    --   -- | return 'DG' as Svg string.
    -- , graphvizShowDG
    --   -- | show a 'DG' in a GraphViz canvas.
    -- -- * Misc
    -- , pprintLabeledRules
    ) where

import           Data.Graph.Inductive.Basic     (undir)
import qualified Data.Graph.Inductive.Graph     as Gr
import           Data.Graph.Inductive.Query.BFS (bfsn)
import           Data.Graph.Inductive.Query.DFS (dfs)
import qualified Data.Graph.Inductive.Query.DFS as DFS
import qualified Data.Graph.Inductive.Tree      as Gr

-- import qualified Data.GraphViz.Types.Monadic as GV
-- import qualified Data.GraphViz.Attributes as GVattribs
-- import qualified Data.GraphViz.Attributes.Complete as GVattribsc
-- import Data.GraphViz.Types.Generalised
-- import qualified Data.GraphViz.Commands as GVcommands
-- import System.IO
-- import System.IO.Unsafe (unsafePerformIO)
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BSC

-- import           Control.Monad                  (liftM)
-- import qualified Control.Monad.State.Lazy       as State
-- import           Control.Monad.Trans            (lift)
-- import           Data.List                      (delete, sortBy)
import qualified Data.List                      as List
-- import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe)
-- import           Data.Typeable


type Graph nl el = Gr.Gr nl el
type NodeId = Gr.Node

empty :: Graph nl el
empty = Gr.empty

mkGraph ::  [(NodeId,nl)] -> [(NodeId,NodeId,el)] -> Graph nl el
mkGraph = Gr.mkGraph


--- * graph inspection -----------------------------------------------------------------------------------------------

context :: Graph nl el -> NodeId -> ([(el,NodeId)],NodeId,nl,[(el,NodeId)])
context = Gr.context

nodes :: Graph nl el -> [NodeId]
nodes = Gr.nodes

lnodes :: Graph nl el -> [(NodeId,nl)]
lnodes = Gr.labNodes

roots :: Graph nl el -> [NodeId]
roots gr = [n | n <- Gr.nodes gr, Gr.indeg gr n == 0]

leafs :: Graph nl el -> [NodeId]
leafs gr = [n | n <- Gr.nodes gr, Gr.outdeg gr n == 0]

lookupNodeLabel :: Graph nl el -> NodeId -> Maybe nl
lookupNodeLabel = Gr.lab

lookupNodeLabel' :: Graph nl el -> NodeId -> nl
lookupNodeLabel' gr n = err `fromMaybe` lookupNodeLabel gr n
  where err = error "Graph.lookupNodeLabel': node id not found"

withNodeLabels :: Graph nl el -> [NodeId] -> [(NodeId, Maybe nl)]
withNodeLabels gr ns = [(n,lookupNodeLabel gr n) | n <- ns]

withNodeLabels' :: Graph nl el -> [NodeId] -> [(NodeId, nl)]
withNodeLabels' gr ns = [(n,lookupNodeLabel' gr n) | n <- ns]

lookupNode :: Eq nl => Graph nl el -> nl -> Maybe NodeId
lookupNode gr n = lookup n [(n',i) | (i,n') <- Gr.labNodes gr]

lookupNode' :: Eq nl => Graph nl el -> nl -> NodeId
lookupNode' gr n = err `fromMaybe` lookupNode gr n
  where err = error "Graph.lookupNodeLabel': label not found"

inverse :: Graph nl el -> Graph nl el
inverse gr = Gr.mkGraph ns es
  where ns = Gr.labNodes gr
        es = [ (n2, n1, i) | (n1,n2,i) <- Gr.labEdges gr ]

topsort :: Graph nl el -> [NodeId]
topsort = DFS.topsort

sccs :: Graph nl el -> [[NodeId]]
sccs = DFS.scc

undirect :: Eq el => Graph nl el -> Graph nl el
undirect = undir

successors :: Graph nl el -> NodeId -> [NodeId]
successors = Gr.suc

reachablesBfs :: Graph nl el -> [NodeId] -> [NodeId]
reachablesBfs = flip bfsn

reachablesDfs :: Graph nl el -> [NodeId] -> [NodeId]
reachablesDfs = flip dfs

predecessors :: Graph nl el -> NodeId -> [NodeId]
predecessors = Gr.pre

lsuccessors :: Graph nl el -> NodeId -> [(NodeId, nl, el)]
lsuccessors gr nde = [(n, lookupNodeLabel' gr n, e) | (n,e) <- Gr.lsuc gr nde]

lpredecessors :: Graph nl el -> NodeId -> [(NodeId, nl, el)]
lpredecessors gr nde = [(n, lookupNodeLabel' gr n, e) | (n,e) <- Gr.lpre gr nde]

isEdgeTo :: Graph nl el -> NodeId -> NodeId -> Bool
isEdgeTo g n1 n2 = n2 `elem` successors g n1

isLEdgeTo :: Eq el => Graph nl el -> NodeId -> el -> NodeId -> Bool
isLEdgeTo g n1 e n2 = n2 `elem` [n | (n, _, e2) <- lsuccessors g n1, e == e2]

subGraph :: Graph nl el -> [NodeId] -> Graph nl el
subGraph g ns = Gr.delNodes (nodes g List.\\ ns) g

removeNodes :: Graph nl el -> [NodeId] -> Graph nl el
removeNodes = flip Gr.delNodes

removeEdges :: Graph nl el -> [(NodeId,NodeId)] -> Graph nl el
removeEdges = flip Gr.delEdges

updateLabels :: (nl -> ml) -> Graph nl el -> Graph ml el
updateLabels = Gr.nmap

{-
----------------------------------------------------------------------
--- pretty printing

pprintNodeSet :: [NodeId] -> Doc
pprintNodeSet ns = braces $ hcat $ punctuate (text ",") [ text $ show n | n <- ns]

pprintCWDGNode :: CDG -> F.Signature -> V.Variables -> NodeId -> Doc
-- pprintCWDGNode cwdg _ _ n = text (show n) <> (text ":") <> pprintNodeSet (congruence cwdg n)
pprintCWDGNode cwdg _ _ n = pprintNodeSet (congruence cwdg n)

pprintCWDG :: CDG -> F.Signature -> V.Variables -> ([NodeId] -> NodeId -> Doc) -> Doc
pprintCWDG cwdg sig vars ppLabel | isEmpty = text "empty"
                                 | otherwise =
    printTree 45 ppNode ppLabel pTree
    $+$ text ""
    $+$ text "Here dependency-pairs are as follows:"
    $+$ text ""
    $+$ pprintLabeledRules "Strict DPs" sig vars (rs StrictDP)
    $+$ pprintLabeledRules "Weak DPs" sig vars (rs WeakDP)
    where isEmpty = null $ allRulesFromNodes cwdg (nodes cwdg)
          ppNode _ n    = printNodeId n
          pTree = PPTree { pptRoots = sortBy compareLabel $ roots cwdg
                         , pptSuc = sortBy compareLabel . snub . successors cwdg}
          compareLabel n1 n2 = congruence cwdg n1 `compare` congruence cwdg n2
          printNodeId = pprintCWDGNode cwdg sig vars
          rs strictness = sortBy compFst $ concatMap (\ (_, cn) -> [ (n, rule) | (n, (s, rule)) <- theSCC cn, s == strictness]) (Graph.labNodes cwdg)
            where (a1,_) `compFst` (a2,_) = a1 `compare` a2

instance PrettyPrintable (CDG, F.Signature, V.Variables) where
  pprint (cwdg, sig, vars) = pprintCWDG cwdg sig vars (\ _ _ -> PP.empty)

pprintLabeledRules :: PrettyPrintable l => String -> F.Signature -> V.Variables -> [(l,R.Rule)] -> Doc
pprintLabeledRules _    _   _ [] = PP.empty
pprintLabeledRules name sig vars rs = text name <> text ":"
                                      $+$ indent (pprintTrs pprule rs)
  where pprule (l,r) = pprint l <> text ":" <+> pprint (r, sig, vars)


-- graphviz output of dgs



toGraphViz :: [(DG,F.Signature,V.Variables)] -> Bool -> DotGraph String
toGraphViz dgs showLabels = GV.digraph' $ mapM digraph $ zip [(1::Int)..] dgs
  where digraph (i,(dg,sig,vars)) = do
          GV.graphAttrs [GVattribsc.Size size]
          GV.cluster (Str $ pack $ "dg_" ++ show i) $
            if showLabels then graphM >> labelM else graphM
          where nds   = nodes dg
                size = GVattribsc.GSize {
                                      GVattribsc.width = 12.0
                                    , GVattribsc.height = Just 6.0
                                    , GVattribsc.desiredSize = True }
                graphM = do
                  mapM_ sccToGV $ zip [(1::Int)..] (DFS.scc dg)
                  mapM_ edgesToGV nds
                labelM = GV.graphAttrs [GVattribs.toLabel pplabel]
                lrules = [(n,r) | (n,(_,r)) <- withNodeLabels' dg nds]
                pprule r = st (R.lhs r) ++ " -> " ++ st (R.rhs r) ++ "\\l"
                  where st t = [c | c <- show $ pprint (t,sig,vars), c /= '\n']
                pplabel = "Below rules are as follows:\\l" ++ concatMap (\ (n,r) -> " " ++ show n ++ ": " ++ pprule r) lrules

                sccToGV (j,scc) = GV.cluster (Str $ pack $ show i ++ "_" ++ show j) $ mapM nodesToGV $ withNodeLabels' dg scc
                nodesToGV (n,(strictness,r)) = GV.node (nde n) attribs
                  where
                    attribs = [ GVattribs.toLabel (show n)
                              , GVattribsc.Tooltip (pack $ pprule r) ]
                               ++ shape strictness
                    shape StrictDP = [GVattribs.shape GVattribs.Circle]
                    shape WeakDP   = [GVattribs.shape GVattribs.Circle, GVattribs.style GVattribs.dotted]
                edgesToGV n = mapM (\ (m,_,k) -> GV.edge (nde n) (nde m) [GVattribs.toLabel (show k)]) (lsuccessors dg n)
                nde n = show i ++ "_" ++ show n

saveGraphViz :: [(DG,F.Signature,V.Variables)] -> Bool -> FilePath -> IO FilePath
saveGraphViz dgs showLabels = GVcommands.runGraphvizCommand GVcommands.Dot (toGraphViz dgs showLabels) GVcommands.Svg

graphVizSVG :: [(DG,F.Signature,V.Variables)] -> Bool -> String
graphVizSVG dgs showLabels = unsafePerformIO $
  GVcommands.graphvizWithHandle GVcommands.Dot (toGraphViz dgs showLabels) GVcommands.Svg rd
    where rd h = do
            bs <- BS.hGetContents h
            hClose h
            return $! BSC.unpack bs



graphvizShowDG :: [(DG,F.Signature,V.Variables)] -> IO ()
graphvizShowDG dgs = GVcommands.runGraphvizCanvas GVcommands.Dot (toGraphViz dgs True) GVcommands.Gtk


toXml :: (DG, F.Signature, V.Variables) -> Xml.XmlContent
toXml (dg,sig,vs) =
    Xml.elt "dependencygraph" []
           [
            -- Xml.elt "svg" [] [ Xml.text $ graphVizSVG [(dg,sig,vs)] False ]
           Xml.elt "nodes" [] [ XmlE.rule r (Just n) sig vs | (n,(_,r)) <- lnodes dg  ]
           , Xml.elt "edges" [] [ Xml.elt "edge" [] $
                                          Xml.elt "source" [] [Xml.int n] : [ Xml.elt "target" [] [Xml.int  m] | m <- successors dg n ]
                                  | n <- nodes dg] ]
           -- , Xml.elt "edges" [] [ Xml.elt "edge" [] $
           --                                Xml.elt "source" [] [Xml.rule r (Just n) sig vs]
           --                                       : [ Xml.elt "targets" [] [Xml.rule s (Just m) sig vs | (m,(_,s),_) <- lsuccessors dg n ] ]
           --                        | (n,(_,r)) <- lnodes dg] ]
           --                        -}
