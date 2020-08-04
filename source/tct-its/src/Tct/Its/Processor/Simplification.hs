{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tct.Its.Processor.Simplification
  (
  -- * Trivial SCCs
  boundTrivialSCCs
  , boundTrivialSCCsDeclaration

  -- * Knowledge Propagation
  , knowledgePropagation
  , knowledgePropagationDeclaration

  -- * Unsat Rules Removal
  , unsatRules
  , unsatRulesDeclaration

  -- * argument filter
  , argumentFilter
  , unusedFilter
  , argumentFilterDeclaration
  
  -- * Transition Graph
  , unsatPaths
  , unsatPathsDeclaration
  -- ** Unreachable Rules Removal
  , unreachableRules
  , unreachableRulesDeclaration
  -- ** Leaf Rules Removal
  , leafRules
  , leafRulesDeclaration

  -- * 
  , testUnsatRule
  , testUnsatRule'
  ) where


import           Control.Monad
import qualified Data.Graph.Inductive         as Gr
import qualified Data.IntMap.Strict           as IM
import qualified Data.Set                     as S
import qualified Data.Traversable             as F

import qualified Tct.Core.Common.Pretty       as PP
import qualified Tct.Core.Common.Xml          as Xml
import qualified Tct.Core.Data                as T

import           Tct.Common.ProofCombinators
import qualified Tct.Common.Polynomial        as P
import           Tct.Common.SMT ((.&&), (.>=))
import qualified Tct.Common.SMT as SMT
import           Tct.Common.Ring

import           Tct.Its.Data.Complexity
import           Tct.Its.Data.Problem
import qualified Tct.Its.Data.Timebounds      as TB
import qualified Tct.Its.Data.TransitionGraph as TG
import           Tct.Its.Data.Types
import           Tct.Its.Data.Rule


data PropagationProcessor
  = TrivialSCCs -- ^ Trivial SCCs in the transition graph are trivially bounded.
  | KnowledgePropagation
  deriving Show

data PropagationProof
  = PropagationProof
    { pproc_ :: PropagationProcessor
    , times_ :: TB.TimeboundsMap }
  | NoPropagationProof
  deriving Show

instance PP.Pretty PropagationProof where
  pretty NoPropagationProof = PP.text "Nothing to propagate."
  pretty PropagationProof{..} = case pproc_ of
    TrivialSCCs          -> PP.text "All trivial SCCs of the transition graph admit timebound 1."
    KnowledgePropagation -> PP.text "We propagate bounds from predecessors."

instance Xml.Xml PropagationProof where
  toXml NoPropagationProof = Xml.elt "nopropagation" []
  toXml PropagationProof{} = Xml.elt "propagation" []

instance T.Processor PropagationProcessor where
  type ProofObject PropagationProcessor = ApplicationProof PropagationProof
  type In  PropagationProcessor           = Its
  type Out PropagationProcessor           = Its
  type Forking PropagationProcessor     = T.Optional T.Id

  execute _ prob | isClosed prob = closedProof prob
  execute p prob = case execute' prob of
    Nothing                -> progress NoProgress (Applicable NoPropagationProof)
    Just (pproof, newprob) -> progress (Progress newprob) (Applicable pproof)
    where
      execute' = case p of
        TrivialSCCs          -> executeTrivialSCCs
        KnowledgePropagation -> executeKnowledgePropagation


-- trivial sccs
executeTrivialSCCs :: Its -> Maybe (PropagationProof, Its)
executeTrivialSCCs prob
  | null pptimes = Nothing
  | otherwise    = Just (pproof, newprob)
  where
    graph = (tgraph_ prob)
    sccs = TG.trivialSCCs graph
    pptimes = [(rid,c) | rid <- sccs, rid `notElem` TB.defined (timebounds_ prob), TG.incoming graph [rid]  == [], let c = one]
    pproof = PropagationProof
      { pproc_ = TrivialSCCs
      , times_ = IM.fromList pptimes }
    newprob = prob {timebounds_ = TB.inserts old new }
      where (old,new) = (timebounds_ prob, times_ pproof)

boundTrivialSCCs :: ItsStrategy
boundTrivialSCCs = T.Apply TrivialSCCs

-- FIXME: MS only sound in the non-recursive case
boundTrivialSCCsDeclaration :: T.Declaration ('[] T.:-> ItsStrategy)
boundTrivialSCCsDeclaration = T.declare "simp" [desc]  () boundTrivialSCCs
  where desc = "Trivial SCCs in the transition graph admit a timebound 1. This processor always Succeeds."


-- knowledge propagation

executeKnowledgePropagation :: Its -> Maybe (PropagationProof, Its)
executeKnowledgePropagation prob = case propagateRules tgraph tbounds rs of
  ([],_)       -> Nothing
  (ris,tbounds') -> Just (mkPProof ris tbounds', prob {timebounds_ = tbounds'})
  where
    mkPProof ris tbounds' = PropagationProof
      { pproc_ = KnowledgePropagation
      , times_ = IM.fromList $ map (\ri -> (ri,tbounds' `TB.tboundOf` ri)) ris }
    tbounds = timebounds_ prob
    tgraph  = tgraph_ prob
    rs      = Gr.topsort tgraph

propagateRules :: TG.TGraph -> TB.Timebounds -> [RuleId] -> ([RuleId],TB.Timebounds)
propagateRules tgraph tbounds = foldl k ([],tbounds)
  where
    k acc@(ps,tbound) r
      | tbound `TB.tboundOf` r /= Unknown = acc
      | otherwise = case propagateRule tgraph tbound r of
          Nothing -> acc
          (Just tbound') -> (r:ps,tbound')

propagateRule :: TG.TGraph -> TB.Timebounds -> RuleId -> Maybe TB.Timebounds
propagateRule tgraph tbounds ru
  | ppbound == Unknown = Nothing
  | otherwise          = Just (TB.update ru ppbound tbounds)
  where ppbound = bigAdd [ tbounds `TB.tboundOf` t | (t,_) <- TG.predecessors tgraph ru ]


knowledgePropagation :: ItsStrategy
knowledgePropagation = T.Apply KnowledgePropagation

knowledgePropagationDeclaration :: T.Declaration ('[] T.:-> ItsStrategy)
knowledgePropagationDeclaration = T.declare "know" [desc]  () knowledgePropagation
  where desc = "Propagates complexities from the predecessors."

-- * Rule Removal

data RuleRemovalProcessor
  = UnsatRules
  | UnreachableRules
  | LeafRules
  deriving (Show)

data RuleRemovalProof
  = RuleRemovalProof
    { rproc_  :: RuleRemovalProcessor
    , rrules_ :: [RuleId]}
  | NoRuleRemovalProof
    { rproc_ :: RuleRemovalProcessor }
  deriving Show

instance PP.Pretty RuleRemovalProof where
  pretty RuleRemovalProof{..} =
    case rproc_ of
      UnsatRules       ->
        PP.text "Following transitions have unsatisfiable constraints and are removed: " PP.<+> PP.pretty rrules_
      UnreachableRules ->
        PP.text "Following transitions are not reachable from the starting states and are removed:" PP.<+> PP.pretty rrules_
      LeafRules ->
        PP.text "Following transitions are estimated by its predecessors and are removed" PP.<+> PP.pretty rrules_
  pretty NoRuleRemovalProof{..} =
    case rproc_ of
      UnsatRules       -> PP.text "No constraint could have been show to be unsatisfiable. No rules are removed."
      UnreachableRules -> PP.text "All transitions are reachable from the starting states. No rules are removed."
      LeafRules        -> PP.text "No leaf rules. No rules are removed."

instance Xml.Xml RuleRemovalProof where
  toXml NoRuleRemovalProof{} = Xml.elt "noruleremoval" []
  toXml RuleRemovalProof{}   = Xml.elt "ruleremoval" []
 
-- * Rechability

instance T.Processor RuleRemovalProcessor where
  type ProofObject RuleRemovalProcessor = ApplicationProof RuleRemovalProof
  type In  RuleRemovalProcessor           = Its
  type Out RuleRemovalProcessor           = Its
  type Forking RuleRemovalProcessor     = T.Optional T.Id

  execute _ prob | isClosed prob = closedProof prob
  execute UnsatRules prob        = solveUnsatRules prob
  execute UnreachableRules prob  = solveUnreachableRules prob
  execute LeafRules prob         = solveLeafRules prob

solveUnsatRules :: Its -> T.TctM (T.Return RuleRemovalProcessor)
solveUnsatRules prob = do
  unsats <- do
    res <- F.sequence $ IM.map testUnsatRule' nrules
    return . IM.keys $ IM.filter id res
  if null unsats
    then progress NoProgress (Applicable (NoRuleRemovalProof p))
    else progress (Progress $ removeRules unsats prob) (Applicable (RuleRemovalProof p unsats))
  where
    p = UnsatRules
    nrules     = IM.filterWithKey (\k _ -> k `elem` nonDefined) (irules_ prob)
    nonDefined = TB.nonDefined (timebounds_ prob)

testUnsatRule :: Rule -> T.TctM Bool
testUnsatRule (Rule _ _ []) = return False
testUnsatRule (Rule _ _ cs) = do
  s :: SMT.Result () <- SMT.smtSolveSt SMT.yices $ do
    SMT.setLogic SMT.QF_LIA
    SMT.assert $ SMT.bigAnd (map (SMT.bigOr . (map encodeAtom)) cs)
    return $ SMT.decode ()
  return (SMT.isUnsat s)

-- use farkas lemma: try to derive C |= 0 >= 1
testUnsatRule' :: Rule -> T.TctM Bool
testUnsatRule' (Rule _ _ []) = return False
testUnsatRule' (Rule _ _ cs) = do
  s :: SMT.Result () <- SMT.smtSolveSt SMT.yices $ do
    let 
      absolute p              = SMT.bigAnd [ c SMT..== zero | c <- P.coefficients p ]
      bottom                  = P.constant (SMT.neg SMT.one)
      eliminateFarkas ply ds1 = do
        ds2 <- forM ds1 (\c -> P.scale <$> SMT.nvarM' <*> pure c)
        let
          (p1,pc1) = P.splitConstantValue ply
          (p2,pc2) = P.splitConstantValue (bigAdd ds2)
        return $ absolute (p1 `sub` p2) .&& (pc1 .>= pc2)


    SMT.setLogic SMT.QF_LIA
    SMT.assert (SMT.top :: SMT.Formula Int)
    -- FIXME consider disjunction
    SMT.assertM $ eliminateFarkas bottom [ P.mapCoefficients SMT.num c | [Gte c _] <- toGte cs ]

    return $ SMT.decode ()
  return $ SMT.isSat s
  

solveUnreachableRules :: Its -> T.TctM (T.Return RuleRemovalProcessor)
solveUnreachableRules prob =
  let unreachable = Gr.nodes tgraph `minus` Gr.dfs starts tgraph in
  if null unreachable
    then progress NoProgress (Applicable (NoRuleRemovalProof p))
    else progress (Progress $ removeRules unreachable prob) (Applicable (RuleRemovalProof p unreachable))
  where
    p         = UnreachableRules
    tgraph    = tgraph_ prob
    starts    = IM.keys (startrules prob)
    minus a b = S.toList $ S.fromList a `S.difference` S.fromList b

solveLeafRules :: Its -> T.TctM (T.Return RuleRemovalProcessor)
solveLeafRules prob =
  let leafs = solveLeafRule (tgraph_ prob) [] in
  if null leafs
    then progress NoProgress (Applicable (NoRuleRemovalProof p))
    else progress (Progress $ mkproof leafs) (Applicable (RuleRemovalProof p leafs))
  where
    mkproof leafs = let prob' = removeRules leafs prob in prob'{timebounds_ = TB.addLeafCost (timebounds_ prob') (length leafs)}
    p         = LeafRules
    isLeave gr n = Gr.indeg gr n > 0 && Gr.outdeg gr n == 0
    solveLeafRule gr leafs =
      let leafs' = filter (isLeave gr)  (Gr.nodes gr) in
      if null leafs'
        then leafs
        else solveLeafRule (Gr.delNodes leafs' gr) (leafs' ++ leafs)

-- * unsat path removal
data PathRemovalProcessor = UnsatPaths
  deriving Show

data PathRemovalProof
  = PathRemovalProof [(RuleId, RuleId)]
  | NoPathRemovalProof
  deriving Show

instance PP.Pretty PathRemovalProof where
  pretty NoPathRemovalProof    = PP.text "Nothing happend"
  pretty (PathRemovalProof es) = PP.text "We remove following edges from the transition graph: " PP.<> PP.pretty es

instance Xml.Xml PathRemovalProof where
  toXml NoPathRemovalProof   = Xml.elt "nopathremoval" []
  toXml (PathRemovalProof _) = Xml.elt "pathremoval" []

instance T.Processor PathRemovalProcessor where
  type ProofObject PathRemovalProcessor = ApplicationProof PathRemovalProof
  type In  PathRemovalProcessor           = Its
  type Out PathRemovalProcessor           = Its
  type Forking PathRemovalProcessor     = T.Optional T.Id

  execute _ prob | isClosed prob = closedProof prob
  execute UnsatPaths prob        = solveUnsatPaths prob

solveUnsatPaths :: Its -> T.TctM (T.Return PathRemovalProcessor)
solveUnsatPaths prob = do
  unsats <- filterM solveUnsatPath (Gr.edges tgraph)
  if null unsats
    then progress NoProgress (Applicable NoPathRemovalProof)
    else progress (Progress (mkprob unsats)) (Applicable (PathRemovalProof unsats))
  where
    tgraph = tgraph_ prob
    irules = irules_ prob

    mkprob es = prob {tgraph_ = Gr.delEdges es tgraph}
    solveUnsatPath (n1,n2) = case chain (irules IM.! n1) (irules IM.! n2) of
      Nothing -> return False
      Just r  -> testUnsatRule' r


-- * argument filtering

data ArgumentFilterProcessor = ArgumentFilter [Int] -- against the convention: lists indices to remove
  deriving Show

data ArgumentFilterProof
  = ArgumentFilterProof [Int]
  | NoArgumentFilterProof
  deriving Show

instance PP.Pretty ArgumentFilterProof where
  pretty NoArgumentFilterProof    = PP.text "Nothing happend"
  pretty (ArgumentFilterProof es) = PP.text "We remove following argument positions: " PP.<> PP.pretty es PP.<> PP.dot

instance Xml.Xml ArgumentFilterProof where
  toXml NoArgumentFilterProof   = Xml.elt "noargumentfilter" []
  toXml (ArgumentFilterProof _) = Xml.elt "argumentfilter" []


instance T.Processor ArgumentFilterProcessor where
  type ProofObject ArgumentFilterProcessor = ApplicationProof ArgumentFilterProof
  type In  ArgumentFilterProcessor           = Its
  type Out ArgumentFilterProcessor           = Its
  type Forking ArgumentFilterProcessor     = T.Optional T.Id

  execute _ prob | isClosed prob = closedProof prob
  execute p prob                 = solveArgumentFilter prob p

solveArgumentFilter :: Its -> ArgumentFilterProcessor -> T.TctM (T.Return ArgumentFilterProcessor)
solveArgumentFilter prob (ArgumentFilter as)
  | null as   = progress NoProgress (Applicable NoArgumentFilterProof)
  | otherwise = progress (Progress nprob) (Applicable (ArgumentFilterProof as))
  where
    nprob = prob
      { irules_          = IM.map afOnRule (irules_ prob)
      , startterm_       = afOnTerm (startterm_ prob)
      , rvgraph_         = Nothing
      , sizebounds_      = Nothing
      , localSizebounds_ = Nothing }
    afOnRule (Rule l r cs) = Rule (afOnTerm l) (map afOnTerm r) cs
    afOnTerm (Term fs ars) = Term fs (fst . unzip . filter ((`notElem` as) . snd) $ zip ars [0..])

-- select positions not occuring in constraints
unusedFilter :: Its -> [Int]
unusedFilter prob = indices $ foldr (S.union . unusedR) S.empty allrules
  where
    allrules = IM.elems (irules_ prob)
    indices vs = fst . unzip . filter ((`S.member` unused) . snd) $ zip [0..] (domain prob)
      where unused = S.fromList (domain prob) `S.difference` vs
    unusedR r = foldr (S.union . S.fromList . P.variables) S.empty (celems $ con r)


-- * instances
unsatRules :: ItsStrategy
unsatRules = T.Apply UnsatRules

unsatRulesDeclaration :: T.Declaration ('[] T.:-> ItsStrategy)
unsatRulesDeclaration = T.declare "unsatRules" [desc]  () unsatRules
  where desc = "This processor removes rules with unsatisfiable constraints."

unsatPaths :: ItsStrategy
unsatPaths = T.Apply UnsatPaths

unsatPathsDeclaration :: T.Declaration ('[] T.:-> ItsStrategy)
unsatPathsDeclaration = T.declare "unsatPaths" [desc]  () unsatPaths
  where desc = "This processor tests wether rule2 can follow rule1 for all edges in the flow graph."

unreachableRules :: ItsStrategy
unreachableRules = T.Apply UnreachableRules

unreachableRulesDeclaration :: T.Declaration ('[] T.:-> ItsStrategy)
unreachableRulesDeclaration = T.declare "unreachableRules" [desc]  () unreachableRules
  where desc = "This processor removes rules not reachable from the starting location."

leafRules :: ItsStrategy
leafRules = T.Apply LeafRules

leafRulesDeclaration :: T.Declaration ('[] T.:-> ItsStrategy)
leafRulesDeclaration = T.declare "leafRules" [desc]  () leafRules
  where desc = "This processor removes leafs in the transition graph."

argumentFilter :: [Int] -> ItsStrategy
argumentFilter = T.Apply . ArgumentFilter

argumentFilterDeclaration :: T.Declaration ('[T.Argument 'T.Optional Filter] T.:-> ItsStrategy)
argumentFilterDeclaration = T.declare "argumentFilter" [desc] (T.OneTuple arg) argumentFilter'
  where
    desc = "Removes argument positions acoording to the provided argument."
    arg = filterArg `T.optional` Unused
    argumentFilter' = const $ T.withProblem (argumentFilter . unusedFilter)

data Filter = Unused 
  deriving (Show, Enum, Bounded)

filterArg :: T.Argument 'T.Required Filter
filterArg = T.flag "filter" ["Specifies the filter to apply."]

