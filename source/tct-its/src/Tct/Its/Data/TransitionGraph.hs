module Tct.Its.Data.TransitionGraph
  (
  TGraph, CGraph
  -- * Construction
  , estimateGraph
  -- * Queries
  {-, initialRules -}
  , predecessors
  , successors
  , descendants
  , incoming
  , rootsPaths
  , sccs
  , nextSCC
  , nextSCCAny
  , upToNextSCC
  , fromNextSCC
  , trivialSCCs
  , nonTrivialSCCs
  , simpleCycles
  -- * Update
  , deleteNodes
  , restrictToNodes
  , insertEdges
  ) where


import           Control.Monad           (void)
import qualified Data.Graph.Inductive    as Gr
import qualified Data.IntMap             as IM
import qualified Data.List               as L (nub)
import           Data.Maybe              (catMaybes)
import qualified Data.Set                as S

import qualified Tct.Core.Common.Pretty  as PP

import qualified Tct.Its.Data.Timebounds as TB
import           Tct.Its.Data.Types
import           Debug.Trace


type Graph nl el = Gr.Gr nl el
type NodeId      = Gr.Node
-- | Transition (or Control-Flow) graph:
--    * Nodes correspond to indices of rules.
--    * There is an edge between two nodes if there exists an evaluation st the latter rule follows the former rule.
--    * Edges are labelled with the corresponding position of the compound symbol.
type TGraph = Gr.Gr () ComId
type CGraph = Gr.Gr (SCC RuleId) (RuleId,ComId)



-- | Default estimation. See 'functionSymbols'.
estimateGraph :: Rules -> TGraph
estimateGraph = estimateGraphWith functionSymbols

estimateGraphWith :: (Rule -> Rule -> [Int])  -> Rules -> TGraph
estimateGraphWith f rs = Gr.mkGraph ns es
  where
    irs = IM.toList rs
    ns  = map void irs
    es  = [ (n1, n2, cid) | (n1,r1) <- irs, (n2,r2) <- irs, cid <- f r1 r2 ]

-- | Only compares the function symbol.
functionSymbols :: Rule -> Rule -> [ComId]
functionSymbols r1 r2 = [ cid | (cid,r) <- zip [0..] (rhs r1), fun r == fun (lhs r2) ]

-- | Returns all nodes without predecessors.
{-initialRules :: TGraph -> [RuleId]-}
{-initialRules tgraph = filter (\n -> Gr.indeg tgraph n == 0) (Gr.nodes tgraph)-}

predecessors :: TGraph -> RuleId -> [RV']
predecessors = Gr.lpre

successors :: TGraph -> RuleId -> [RV']
successors = Gr.lsuc

roots :: Graph nl el -> [NodeId]
roots gr = [ n | n <- Gr.nodes gr, Gr.indeg gr n == 0 ]

rootsPaths :: TGraph -> [[SCC RuleId]]
rootsPaths gr = [ map scc p | p <- paths, not $ any (S.fromList p `S.isProperSubsetOf`) pathsS ]
  where
    cgr = toCongruenceGraph gr

    paths  = L.nub $ concatMap (walk []) (roots cgr)
    pathsS = map S.fromList paths

    walk path n
      | null sucs = [reverse path']
      | otherwise = concatMap (walk path') sucs
      where
        sucs  = Gr.suc cgr n
        path' = n:path
    scc = Gr.lab' . Gr.context cgr



toCongruenceGraph :: TGraph -> CGraph
toCongruenceGraph gr = Gr.mkGraph ns es
  where
    ns    = zip [1..] [sccNode scc | scc <- sccs gr]
    sccNode [n] | n `notElem` Gr.suc gr n = Trivial n
    sccNode nx  = NonTrivial nx

    es = [ (n1, n2, i) | (n1, cn1) <- ns, (n2, cn2) <- ns,  n1 /= n2, i <- cn1 `edgesTo` cn2 ]
    cn1 `edgesTo` cn2 =
      [ (n,i) | n1 <- theSCC cn1, (n,i) <- Gr.lsuc gr n1, n `elem` theSCC cn2]

sccs :: TGraph -> [[RuleId]]
sccs = Gr.scc

trivialSCCs :: TGraph -> [RuleId]
trivialSCCs tgraph = concat . filter acyclic  $ Gr.scc tgraph
  where
    acyclic [scc] = scc `notElem` Gr.suc tgraph scc
    acyclic _     = False

nonTrivialSCCs :: TGraph -> [[RuleId]]
nonTrivialSCCs tgraph = filter cyclic $ Gr.scc tgraph
  where
    cyclic [scc] = scc `elem` Gr.suc tgraph scc
    cyclic _     = True


-- | Returns nextSCC with an open rule.
nextSCC :: TGraph -> TB.Timebounds -> [RuleId]
nextSCC tgraph tbounds = go (sccs tgraph)
  where
    undefineds = TB.nonDefined tbounds
    go [] = []
    go (scc : ss)
      | any (`elem` undefineds) scc = scc
      | otherwise = go ss

nextSCCAny :: TGraph -> [RuleId]
nextSCCAny tgraph = case nonTrivialSCCs tgraph of {scc : _ -> scc; _ -> []}


-- | Returns nextSCC with an open rule.
upToNextSCC :: TGraph -> TB.Timebounds -> [[RuleId]]
upToNextSCC tgraph tbounds = go (sccs tgraph)
  where
    undefineds = TB.nonDefined tbounds
    go [] = []
    go (scc : ss)
      | any (`elem` undefineds) scc = [scc]
      | otherwise                   = scc : go ss

-- | Returns all SCCs with an oppen rule.
fromNextSCC :: TGraph -> TB.Timebounds -> [[RuleId]]
fromNextSCC tgraph tbounds = k (sccs tgraph)
  where
    undefineds = TB.nonDefined tbounds
    k = filter (any (`elem` undefineds))

descendants :: TGraph -> [RuleId] -> [RuleId]
descendants g ns =
  let
    ns' = concat [ map fst (successors g n) | n <- ns]
  in
    if all (\n -> n `elem` ns) ns' then ns else descendants g (L.nub (ns ++ ns'))

instance PP.Pretty TGraph where
  pretty gr = PP.list . map k $ Gr.nodes gr
    where k n = PP.int n PP.<> PP.string "->" PP.<> PP.set' (Gr.suc gr n)

incoming :: TGraph -> [RuleId] -> [RV']
incoming tgraph somerules = S.toList $ S.filter ((`S.notMember` ous) . fst) ins
  where
    ins = S.unions $ map (S.fromList . Gr.lpre tgraph) somerules
    ous = S.fromList somerules

deleteNodes :: [RuleId] -> TGraph -> TGraph
deleteNodes = Gr.delNodes

restrictToNodes :: [RuleId] -> TGraph -> TGraph
restrictToNodes rs gr = deleteNodes invrs gr
  where invrs = S.toList $ S.fromList (Gr.nodes gr) `S.difference` S.fromList rs

insertEdges :: [(RuleId, RuleId, ComId)] -> TGraph -> TGraph
insertEdges = Gr.insEdges


--- * cycles ---------------------------------------------------------------------------------------------------------
-- compute simple paths (sequence of edges) which are cycles

data Edge = Edge
 { from :: Fun
 , to   :: Fun
 , rid  :: Int }
 deriving (Show, Eq)

data RPath = Path Edge [Edge] | Cycle [Edge]
  deriving Show

simpleCycles :: Rules -> [[RuleId]]
simpleCycles rs = fmap S.toList $ L.nub $ fmap S.fromList [ fromEdges es | Cycle es <- rpaths (fromRules rs) ]

fromRules :: Rules  -> [Edge]
fromRules rs = [ Edge {from = fun (lhs r), to = fun (head $ rhs r), rid = i} | (i,r) <- IM.toList rs ]

fromEdges :: [Edge] -> [RuleId]
fromEdges es = [ rid e | e <- es ]

isLoop, notIsLoop :: Edge -> Bool
isLoop e    = from e == to e
notIsLoop e = from e /= to e

isSimple :: Edge -> [Edge] -> Bool
isSimple e es = e `notElem` es

prependSimple :: Edge -> RPath -> Maybe RPath
prependSimple e (Path v [])
  | isLoop v        = Just $ Cycle [v]
  | notIsLoop e
  && from v == to e = Just $ Path v [e]
  | otherwise       = Nothing
prependSimple e (Path v ws@(w:_))
  | to v == from w  = Just $ Cycle (v:ws)
  | to e == from w
  && notIsLoop  e
  && isSimple e ws  = Just $ Path v (e:ws)
  | otherwise       = Nothing
prependSimple _ _   = Nothing

rpaths :: [Edge] -> [RPath]
rpaths es = go [ Path e [] | e <- es ] [] where
  go new old
    | null new  = old
    | otherwise = go (step new) (new ++ old)
  step ps = catMaybes [ e `prependSimple` p | e <- es, p <- ps ]

