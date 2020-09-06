module Tct.Its.Processor.Chaining
  ( chaining
  , chaining1
  , chainingCandidates
  , isUnknown
  , maxCost
  , maxOuts
  ) where

import           Control.Monad
import qualified Data.IntMap.Strict           as IM
import qualified Data.List                           as L (nub)


import qualified Tct.Core.Common.Pretty       as PP
import qualified Tct.Core.Common.Xml          as Xml
import qualified Tct.Core.Data                as T

import           Tct.Common.ProofCombinators

import           Tct.Its.Data.Problem
import           Tct.Its.Data.Rule
import qualified Tct.Its.Data.Timebounds      as TB
import qualified Tct.Its.Data.Sizebounds      as SB
import qualified Tct.Its.Data.TransitionGraph as TG
import           Tct.Its.Data.Types
import           Debug.Trace


data ChainProcessor = ChainProcessor Bool [RuleId]
  deriving Show

data ChainProof
  = ChainProof
    { removedRule  :: RuleId
    , addedRules   :: [RuleId] }
  | NoChainProof
  deriving Show

instance PP.Pretty ChainProof where
  pretty NoChainProof
    = PP.text "No rule found for the application."
  pretty pproof = PP.hsep
    [ PP.text "We chained rule"
    , PP.int (removedRule pproof)
    , PP.text "to obtain the rules"
    , PP.pretty (addedRules pproof)
    , PP.dot ]

instance Xml.Xml ChainProof where
  toXml NoChainProof = Xml.elt "nochain" []
  toXml _            = Xml.elt "chain" []

instance T.Processor ChainProcessor where
  type ProofObject ChainProcessor = ApplicationProof ChainProof
  type In  ChainProcessor         = Its
  type Out ChainProcessor         = Its
  type Forking ChainProcessor     = T.Optional T.Id

  --execute _ prob | isClosed prob = closedProof prob
  execute (ChainProcessor non_forking choice) prob =
    case foldl (\acc r -> acc `mplus` (chainOne non_forking) prob r) Nothing choice of
      Nothing              -> progress NoProgress (Applicable NoChainProof)
      Just (nprob, pproof) -> progress (Progress nprob) (Applicable pproof)


chainOne :: Bool -> Its -> RuleId -> Maybe (Its, ChainProof)
chainOne onlySingle prob r = do
  let
    rrule  = irules IM.! r
    succs  = map fst (TG.successors (tgraph_ prob) r)
    irules = irules_ prob
    lhsroot rid = fun (lhs (irules IM.! rid))
    -- avoid adding tuple rules
    succs1 = [rid | rid <- succs, length (rhs (irules IM.! rid)) <= 1]
    -- avoid that rrule and successor have same lhs root: happens if the rhs of 
    -- rrule is also same symbol, and self-loops should not be unrolled
    -- (but if rrule has multiple rhs terms, it may be a self loop nevertheless)
    succs2 = [rid | rid <- succs1, lhsroot rid /= lhsroot r]
    succ_roots = L.nub [ fun (lhs rl) | rl <- map (\i -> irules IM.! i) succs2] -- get lhs roots
    succs_groups = [ [rl | rl <- succs2, fun (lhs (irules IM.! rl)) == f] | f <- succ_roots ]
    succs_groups1 = [ g | g <- succs_groups, not (any (\r -> isSelfLoop (irules IM.! r)) g)]
    -- if there are multiple terms on rhs, replace only one symbol/argument
    the_succs = if length (rhs rrule) == 1 then succs1 else head succs_groups1
    long_succ = any (\rid -> length (rhs (irules IM.! rid)) > 1) the_succs
  msuccs <- if onlySingle || succs_groups1 == [] || long_succ || the_succs == [] then Nothing else Just the_succs
  nrules <- forM msuccs (chain rrule . (irules IM.!))
  let
    nextid = maximum (IM.keys irules) + 1
    nirules = zip [nextid ..] nrules
    ris = fst (unzip nirules)
    newrules = IM.union (IM.fromList nirules) (IM.delete r irules)
    nprob = prob
      { irules_          = newrules
      , tgraph_          = TG.estimateGraph newrules
      , timebounds_      = TB.bridge (timebounds_ prob) r (zip msuccs ris)
      , localSizebounds_ = Nothing
      , rvgraph_         = Nothing
      , sizebounds_      = sbounds r (zip msuccs ris)}
  return (nprob, ChainProof r ris)
  where
    sbounds r succrls = case sizebounds_ prob of
      Just sb -> Just (SB.bridge sb r succrls)
      _ -> Nothing


chaining :: [RuleId] -> ItsStrategy
chaining = T.Apply . (ChainProcessor False)

chaining1 :: [RuleId] -> ItsStrategy
chaining1 = T.Apply . (ChainProcessor True)

chainingCandidates :: (Its -> RuleId -> Bool) -> Its -> [RuleId] -> [RuleId]
chainingCandidates f prob r = filter (f prob) r

isUnknown :: Its -> RuleId -> Bool
isUnknown prob = (`elem` TB.nonDefined (timebounds_ prob))

maxCost :: Int -> Its -> RuleId -> Bool
maxCost n prob r = TB.tcostOf (timebounds_ prob) r <=  n

-- FIXME: we should compute out wrt to the function symbol not the rule
maxOuts :: Int -> Its -> RuleId -> Bool
maxOuts n prob r = null $ drop n (TG.successors (tgraph_ prob) r)

