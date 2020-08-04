module Tct.Its.Processor.Sizebounds
  (
  sizebounds
  , sizeboundsDeclaration
  ) where


--import qualified Data.Graph.Inductive.Dot         as Gr
import           Data.Maybe                       (fromMaybe)

import qualified Tct.Core.Common.Pretty           as PP
import qualified Tct.Core.Common.Xml              as Xml
import           Tct.Core (withProblem, (.>>>))
import qualified Tct.Core.Data as T

import           Tct.Common.ProofCombinators

import           Tct.Its.Data.Problem
import           Tct.Its.Data.LocalSizebounds     (LocalSizebounds)
import qualified Tct.Its.Data.LocalSizebounds     as LB (compute)
import           Tct.Its.Data.ResultVariableGraph (RVGraph)
import qualified Tct.Its.Data.ResultVariableGraph as RVG (compute)
import           Tct.Its.Data.Rule
import           Tct.Its.Data.Sizebounds          (Sizebounds)
import qualified Tct.Its.Data.Sizebounds          as SB (initialise, updateSizebounds)



-- | Computes local sizebounds; Initialises global sizebounds.
localSizebound :: ItsStrategy
localSizebound = T.Apply LocalSizeboundsProc

-- | Sets localSizebounds, rvgraph, sizebounds if not already defined.
initialiseSizebounds :: Its -> T.TctM Its
initialiseSizebounds prob = case localSizebounds_ prob of
  Just _ ->  return prob
  Nothing -> newprob
  where
    newprob = do
      lbounds <- LB.compute (domain prob) (irules_ prob)
      let
        rvgraph = RVG.compute (tgraph_ prob) lbounds
        sbounds = SB.initialise lbounds
      -- liftIO $ writeFile "/tmp/rvgraph.dot" $ maybe "Gr" (Gr.showDot . Gr.fglToDot) (rvgraph_ prob)
      return $ prob {localSizebounds_ = Just lbounds, rvgraph_ = Just rvgraph, sizebounds_ = Just sbounds}


data LocalSizeboundsProcessor = LocalSizeboundsProc deriving Show

data LocalSizeboundsProof
  = LocalSizeboundsProof (Vars, LocalSizebounds) RVGraph
  | LocalSizeboundsFail
  deriving Show

instance PP.Pretty LocalSizeboundsProof where
  pretty (LocalSizeboundsProof vlbounds _) =
    PP.text "LocalSizebounds generated; rvgraph"
    PP.<$$> PP.indent 2 (PP.pretty vlbounds)
  pretty LocalSizeboundsFail = PP.text "LocalSizebounds: no progress."

instance Xml.Xml LocalSizeboundsProof where
  toXml _ = Xml.elt "localsizebounds" []

instance T.Processor LocalSizeboundsProcessor where
  type ProofObject LocalSizeboundsProcessor = ApplicationProof LocalSizeboundsProof
  type In  LocalSizeboundsProcessor         = Its
  type Out LocalSizeboundsProcessor         = Its
  type Forking LocalSizeboundsProcessor     = T.Optional T.Id

  execute LocalSizeboundsProc prob | isClosed prob = closedProof prob
  execute LocalSizeboundsProc prob = do
    nprob <- initialiseSizebounds prob
    let pproof = LocalSizeboundsProof (domain prob, error "proc sizeb" `fromMaybe` localSizebounds_ nprob) (error "proc rv" `fromMaybe` rvgraph_ nprob)
    if localSizebounds_ prob /= localSizebounds_ nprob
      then progress (Progress nprob) (Applicable pproof)
      else progress NoProgress (Applicable LocalSizeboundsFail)


data SizeboundsProcessor = SizeboundsProc deriving Show

data SizeboundsProof
  = SizeboundsProof (Vars, Sizebounds)
  | SizeboundsFail
  deriving Show

instance PP.Pretty SizeboundsProof where
  pretty (SizeboundsProof vsbounds) =
    PP.text "Sizebounds computed:"
    PP.<$$> PP.indent 2 (PP.pretty vsbounds)
  pretty SizeboundsFail = PP.text "Sizebounds: no progress."

instance Xml.Xml SizeboundsProof where
  toXml _ = Xml.elt "sizebounds" []

instance T.Processor SizeboundsProcessor where
  type ProofObject SizeboundsProcessor = ApplicationProof SizeboundsProof
  type In  SizeboundsProcessor         = Its
  type Out SizeboundsProcessor         = Its
  type Forking SizeboundsProcessor     = T.Optional T.Id


  execute SizeboundsProc prob | isClosed prob = closedProof prob
  execute SizeboundsProc prob = 
    if sizebounds_ prob /= sizebounds_ nprob
      then progress (Progress nprob) (Applicable pproof)
      else progress NoProgress (Applicable SizeboundsFail)
    where
      nprob = updateSizebounds prob
      pproof = SizeboundsProof (domain prob, error "sizebound" `fromMaybe` sizebounds_ nprob)

updateSizebounds :: Its -> Its
updateSizebounds prob = prob {sizebounds_ = Just sbounds'} where
  sbounds' = SB.updateSizebounds
    (tgraph_ prob)
    (error "update rvgraph" `fromMaybe` rvgraph_ prob)
    (timebounds_ prob)
    (error "update sizebounds" `fromMaybe` sizebounds_ prob)
    (error "update localsizebounds" `fromMaybe` localSizebounds_ prob)

-- | Updates sizebounds.
sizebounds :: ItsStrategy
sizebounds = withProblem $
  \prob -> if sizeIsDefined prob then sb else localSizebound .>>> sb
  where sb = T.Apply SizeboundsProc

sizeboundsDeclaration :: T.Declaration ('[] T.:-> ItsStrategy)
sizeboundsDeclaration = T.declare "sizebounds" [desc] () sizebounds
  where desc = "Computes global sizebounds using timebounds."

