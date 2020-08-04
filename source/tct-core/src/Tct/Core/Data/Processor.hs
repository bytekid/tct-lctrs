-- | This module provides the 'Processor' type.
-- 'Processor' instances define transformations from problems to a (possible empty) set of subproblems.
module Tct.Core.Data.Processor
  ( Processor (..)
  , Return (..)
  , Fork
  , ProofData
  , CertificateFn
  , apply
  -- * Proof node construction.
  , abortWith
  , succeedWith
  , succeedWith0
  , succeedWith1
  , succeedWithId
  ) where


import           Tct.Core.Common.Error  (catchError)
import qualified Tct.Core.Common.Pretty as PP
import qualified Tct.Core.Data.Forks    as F
import           Tct.Core.Data.Types


-- | Applies a processor. Transforms the result of an application, 'Return',  to a 'ProofTree'.
-- Always creates a node.
apply :: (Processor p) => p -> In p -> TctM (ProofTree (Out p))
apply p i = (toProofTree <$> execute p i) `catchError` handler
  where
    toProofTree (NoProgress (SomeReason r)) = (Failure $ Failed p i r)
    toProofTree (Progress pn cf ts)         = Success (ProofNode p i pn) cf ts
    handler = return . Failure . IOError

-- | Constructs a proof node with the given proof, certificate and output problems.
-- For a more general version, ie. if you want to provide proof trees instead of output problems, use 'Progress' and
-- 'NoProgress'.
succeedWith :: Processor p => ProofObject p -> CertificateFn p -> Forking p (Out p) -> TctM (Return p)
succeedWith pn cfn ts = return $ Progress pn cfn (Open <$> ts)

-- | Provide a judgement.
succeedWith0 :: (Processor p, Forking p ~ F.Judgement) => ProofObject p -> CertificateFn p -> TctM (Return p)
succeedWith0 pn cfn = return (Progress pn cfn F.Judgement)

-- | Succeed with a single child.
succeedWith1 :: (Processor p, Forking p ~ F.Id) => ProofObject p -> CertificateFn p -> Out p -> TctM (Return p)
succeedWith1 pn cfn p = return $ Progress pn cfn (F.toId $ Open p)

-- | Succeed with a single child such that the corresponding is complexity reflecting.
succeedWithId :: (Processor p, Forking p ~ F.Id) => ProofObject p -> Out p -> TctM (Return p)
succeedWithId pn p = return $ Progress pn F.fromId (F.toId $ Open p)

-- | Abort with a reason.
abortWith :: (Show r, PP.Pretty r) => r -> TctM (Return p)
abortWith = return . NoProgress . SomeReason

