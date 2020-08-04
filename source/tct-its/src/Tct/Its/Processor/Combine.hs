module Tct.Its.Processor.Combine
  ( combine
  ) where

import           Control.Monad
import qualified Data.IntMap.Strict           as IM

import qualified Tct.Core.Common.Pretty       as PP
import qualified Tct.Core.Common.Xml          as Xml
import qualified Tct.Core.Data                as T

import           Tct.Common.ProofCombinators
import           Tct.Common.Ring

import           Tct.Its.Data.Problem
import           Tct.Its.Data.Rule
import qualified Tct.Its.Data.Timebounds      as TB
import qualified Tct.Its.Data.Sizebounds      as SB
import qualified Tct.Its.Data.TransitionGraph as TG
import           Tct.Its.Data.Types
import           Tct.Its.Data.Complexity
import           Debug.Trace


data CombineProcessor = CombineProcessor [RuleId]
  deriving Show

data CombinationProof
  = CombinationProof
    { removedRules  :: [RuleId]
    , addedRule    :: RuleId }
  | NoCombinationProof
  deriving Show

instance PP.Pretty CombinationProof where
  pretty NoCombinationProof
    = PP.text "No rule found for the application."
  pretty pproof = PP.hsep
    [ PP.text "We combined rules"
    , PP.pretty (removedRules pproof)
    , PP.text "to obtain the rule"
    , PP.pretty (addedRule pproof)
    , PP.dot ]

instance Xml.Xml CombinationProof where
  toXml NoCombinationProof = Xml.elt "nocombine" []
  toXml _                  = Xml.elt "combine" []

instance T.Processor CombineProcessor where
  type ProofObject CombineProcessor = ApplicationProof CombinationProof
  type In  CombineProcessor         = Its
  type Out CombineProcessor         = Its
  type Forking CombineProcessor     = T.Optional T.Id

  execute (CombineProcessor choice) prob =
    case foldl (\acc r -> acc `mplus` combineOne prob r) Nothing choice of
      Nothing              -> progress NoProgress (Applicable NoCombinationProof)
      Just (nprob, pproof) -> progress (Progress nprob) (Applicable pproof)


combineOne :: Its -> RuleId -> Maybe (Its, CombinationProof)
combineOne prob ruleid = do
  let
    irules = irules_ prob
    rrule = irules IM.! ruleid
    (lt,rt,c) = (lhs rrule, rhs rrule, con rrule)
    rl r = irules IM.! r
    similars = [r | r <- IM.keys irules, rhs (rl r) == rt, lhs (rl r) == lt, con (rl r) /= c]
  case similars of
    [] -> Nothing
    ruleid2 : _ -> do
      let
        constr = [c1 ++ c2 | c1 <- con (rl ruleid2), c2 <- c]
        nrule = Rule {lhs = lt, rhs = rt, con = constr}
        nextid = maximum (IM.keys irules) + 1
        newrules = IM.insert nextid nrule (IM.delete ruleid2 (IM.delete ruleid irules))
        tb' = TB.combine ruleid ruleid2 nextid (timebounds_ prob)
        sb = sizebounds_ prob
        sb' = case sb of {Just sbm -> Just (SB.combine sbm ruleid ruleid2 nextid); _ -> Nothing}
        nprob = prob
          { irules_          = newrules
          , tgraph_          = TG.estimateGraph newrules
          , timebounds_      = tb'
          , localSizebounds_ = Nothing
          , rvgraph_         = Nothing
          , sizebounds_      = sb'}
      Just (nprob, CombinationProof [ruleid, ruleid2] nextid)


combine :: [RuleId] -> ItsStrategy
combine = T.Apply . CombineProcessor
