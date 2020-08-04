-- | This module provides the /Failing/ processors.
-- Like 'abort' but allows to provide a reason. Often useful in combination
-- with choice operators. For example
--
-- > (s1 .>>> close) .<||> (s2 .>>> close)
module Tct.Core.Processor.Failing
  ( failing
  , close
  ) where

import Tct.Core.Data

data Failing i o = Failing String deriving Show

instance (ProofData i, Show o) => Processor (Failing i o) where
  type ProofObject (Failing i o) = ()
  type In (Failing i o)           = i
  type Out (Failing i o)          = o

  execute (Failing msg) _ = abortWith msg

-- | > failing s = abortWith s
failing :: (ProofData i, Show o) => String -> Strategy i o
failing s = processor (Failing s :: (ProofData i, Show o) => Failing i o)

-- | > close = failing "Open problems left."
close :: (ProofData i, Show o) => Strategy i o
close = failing "Open problems left."

