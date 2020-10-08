{-# LANGUAGE ImplicitParams #-}
module Tct.Its.Strategies
  (
  itsDeclarations
  , runtime
  , runtime'
  , runtimeDeclaration
  , module Tct.Its.Processors
  ) where


import           Control.Monad                (foldM)
import           Tct.Core
import qualified Tct.Core.Data                as T

import           Tct.Its.Data.Selector
import           Tct.Its.Data.Problem
import           Tct.Its.Data.TransitionGraph as TG
import           Tct.Its.Processors
import qualified Data.IntMap.Strict           as IM
import           Debug.Trace


itsDeclarations :: [StrategyDeclaration Its Its]
itsDeclarations = [
  SD emptyDeclaration
  , SD farkasDeclaration
  , SD knowledgePropagationDeclaration
  , SD leafRulesDeclaration
  , SD pathAnalysisDeclaration
  , SD polyDeclaration
  , SD sizeboundsDeclaration
  , SD unreachableRulesDeclaration
  , SD unsatRulesDeclaration
  , SD runtimeDeclaration
  ]

runtimeDeclaration :: T.Declaration ('[Argument 'Optional Bool, Argument 'Optional Bool] T.:-> ItsStrategy)
runtimeDeclaration = strategy "runtime" (atarg, afarg) def where
  atarg = bool "useTransitionAbstraction" ["Whether predicate abstraction should be used."] `optional` False
  afarg = bool "useArgumentFilter" ["Whether argument filtering should be used."] `optional` False

wellformed :: ItsStrategy
wellformed = withProblem $ \prob -> 
  when (not $ validate prob) (failing "Problem is not well-formed.")

runtime :: ItsStrategy
runtime  = T.deflFun runtimeDeclaration

runtime' :: Bool -> Bool -> ItsStrategy
runtime' = T.declFun runtimeDeclaration


def :: Bool -> Bool -> ItsStrategy
def useAT useAF =
  let
    ?maxChain  = 2 :: Int
    ?nInChain  = 5 :: Int
    ?nOutChain = 10 :: Int
    ?useAT    = useAT
    ?useAF    = useAF
  in

  wellformed
  .>>> try simpl1
  .>>> try (when ?useAT (withProblem (transitionAbstraction . monotonicityPredicates)))
  .>>> try (when ?useAF (withProblem (argumentFilter . unusedFilter)))
  .>>> try unreachableRules
  .>>> try sizebounds
  -- .>>> try pathAnalysis -- FIXME: update rvgraph error; just re-compute it
  .>>> st
  .>>> (
    (afterChaining chainLoops .>>> try st .>>> empty) 
    -- .<|>
    -- (try st .>>> (afterChaining chainLoops .>>> try st .>>> empty))
    )
  where
    st =
      try simpl2
      .>>> te (withKnowledgePropagation farkas)
      -- .>>> te (try sizebounds .>>> withKnowledgePropagation farkas)
      .>>> te (try sizebounds .>>> usingTimebounds)
    chainLoops =
        try simpl2
        .>>> try sizebounds
        .>>> try locationConstraints
        .>>> te constantFarkas
        .>>> te farkas
        .>>> try (withProblem $ \prob -> when (hasRecPotential prob) (te combineAll) .>>> loopRecurrence)
    usingTimebounds = withProblem $
      \prob -> es $ fastestN 8 [ withKnowledgePropagation (timebounds  c) | c <- timeboundsCandidates (selNextSCC prob)]

-- FIXME: boundtrivialsccs is not always 1 in the recursive case; take max label
simpl1 :: ItsStrategy
simpl1 = force $
  try boundTrivialSCCs
  .>>> try restrictVars
  .>>> try unsatRules

simpl2 :: ItsStrategy
simpl2 = force $
  try unsatPaths
  .>>> try unreachableRules
  .>>> try leafRules

-- withArgumentFilter :: ItsStrategy -> ItsStrategy
-- withArgumentFilter st = st .>>> try af
--   where af = withProblem (argumentFilter . unusedFilter)

loopRecurrence :: ItsStrategy
loopRecurrence = withProblem (\prob -> foldl try_scc identity (sccs prob))
  where
    sccs prob = TG.nonTrivialSCCs (tgraph_ prob)
    try_scc s scc = s .>>> try (loopAnalysis "toplevel" scc)

withKnowledgePropagation :: ItsStrategy -> ItsStrategy
withKnowledgePropagation st = st .>>> try knowledgePropagation

combineAll :: ItsStrategy
combineAll = withProblem $ \prob -> combine (IM.keys (irules_ prob))

innerChaining :: ItsStrategy
innerChaining = withProblem $ \prob -> chaining . chainingCandidates k prob $ selNextSCC prob
  where k prob r = maxCost 10 prob r && maxOuts 10 prob r

innerChainingAll :: ItsStrategy
innerChainingAll = withProblem $ \prob -> chaining . chainingCandidates k prob $ (\p -> IM.keys (irules_ p)) prob
  where k prob r = maxCost 10 prob r && maxOuts 10 prob r

outerChaining :: ItsStrategy
outerChaining = withProblem $ \prob -> chaining . chainingCandidates k prob $ selToNextSCC prob
  where k prob r = isUnknown prob r && maxCost 30 prob r && maxOuts 8 prob r

withChaining :: (?maxChain :: Int, ?nInChain :: Int, ?nOutChain :: Int) => ItsStrategy -> ItsStrategy
withChaining st = exhaustivelyN ?maxChain  $ try st .>>> (exhaustivelyN ?nInChain innerChaining .<|> exhaustivelyN ?nOutChain outerChaining) .>>> try empty

afterChaining :: (?maxChain :: Int, ?nInChain :: Int, ?nOutChain :: Int) => ItsStrategy -> ItsStrategy
-- withChaining st = es $ try st .>>> (exhaustivelyN ?nInChain innerChaining <|> exhaustivelyN ?nOutChain outerChaining)
afterChaining st = (
  try (exhaustivelyN 5 (innerChainingAll .>>> try unreachableRules)))
  .>>> try st

