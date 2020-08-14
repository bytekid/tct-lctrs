module Tct.Its.Processor.LocationConstraints
  ( locationConstraints
  ) where

import           Control.Monad
import qualified Data.Map.Strict              as M
import qualified Data.IntMap.Strict           as IM
import           Data.Maybe                   (fromMaybe)

import qualified Tct.Core.Common.Pretty       as PP
import qualified Tct.Core.Common.Xml          as Xml
import qualified Tct.Core.Data                as T

import           Tct.Common.ProofCombinators

import           Tct.Its.Data.Types
import           Tct.Its.Data.Problem
import           Tct.Its.Data.Rule


data LocationConstraintProcessor = LocationConstraintsProc deriving Show

data LocationConstraintsProof =
  LocationConstraintsProof LocationConstraints
  | LocationConstraintsFail
  deriving Show

ppLocationConstraints :: LocationConstraints -> PP.Doc
ppLocationConstraints lcs = PP.hsep [ PP.hsep [PP.pretty r, PP.text ": ", PP.pretty c] | (r, c) <- M.assocs lcs]

instance PP.Pretty LocationConstraintsProof where
  pretty (LocationConstraintsProof lcs) = PP.hsep
    [ PP.text "We computed the location constraints "
    , ppLocationConstraints lcs
    , PP.dot ]
  pretty LocationConstraintsFail = PP.text "LocationConstraints: no progress."

instance Xml.Xml LocationConstraintsProof where
  toXml (LocationConstraintsProof _ ) = Xml.elt "location constraints" []
  toXml _                        = Xml.elt "no location constraints" []

instance T.Processor LocationConstraintProcessor where
  type ProofObject LocationConstraintProcessor = ApplicationProof LocationConstraintsProof
  type In  LocationConstraintProcessor         = Its
  type Out LocationConstraintProcessor         = Its
  type Forking LocationConstraintProcessor     = T.Optional T.Id

  execute LocationConstraintsProc prob | isClosed prob = closedProof prob
  execute LocationConstraintsProc prob = do
    nprob <- initialiseLocationConstraints prob
    let pproof = LocationConstraintsProof (error "proc locconstr" `fromMaybe` locConstraints_ nprob)
    if locConstraints_ prob /= locConstraints_ nprob
      then progress (Progress nprob) (Applicable pproof)
      else progress NoProgress (Applicable LocationConstraintsFail)

locationConstraints :: ItsStrategy
locationConstraints = T.Apply LocationConstraintsProc

initialiseLocationConstraints :: Its -> T.TctM Its
initialiseLocationConstraints prob = case locConstraints_ prob of
  Just _ ->  return prob
  Nothing -> return (prob { locConstraints_ = Just lcs })
  where
    lcs = foldl (\lcs r -> M.insert r (compute_lcs r) lcs) M.empty (rulesIds (irules_ prob))
    compute_lcs rid = []