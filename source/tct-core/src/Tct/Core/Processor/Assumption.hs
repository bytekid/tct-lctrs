-- | This module provides the /Assumption/ processors.
-- Though generally not sound, it can be useful to close a node of a tree.
module Tct.Core.Processor.Assumption (assumeWith) where

import Tct.Core.Data

data Assumption i = Assumption { assumed :: Certificate }
  deriving Show

instance ProofData i => Processor (Assumption i) where
  type ProofObject (Assumption i) = ()
  type In (Assumption i)          = i
  type Out (Assumption i)         = i
  type Forking (Assumption i)     = Judgement

  execute p _ = succeedWith0 () (judgement (assumed p))

closeNode :: ProofData prob => Certificate -> prob -> ProofTree prob
closeNode cert prob = Success pn (judgement cert) Judgement where
  pn = ProofNode
    { appliedProcessor = assumption prob
    , problem          = prob
    , proof             = () }
  assumption :: ProofData prob => prob -> Assumption prob
  assumption _ = Assumption cert

assumeWith :: ProofData prob => Certificate -> ProofTree prob -> ProofTree prob
assumeWith cert = substitute (closeNode cert)

