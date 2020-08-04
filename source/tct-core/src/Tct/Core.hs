{- | This module re-exports useful top-level definitions for

    * configuring Tct,
    * instantiating TcT, and
    * declaring customised Strategies,

and is intended to be imported unqualified.
-}
module Tct.Core
  (
  -- * Configuring Tct
  TctConfig (..)
  , runTct
  , runTctWithOptions
  , defaultTctConfig
  , withDefaultStrategy
  , appendGHCiScript
  , addRuntimeOption
  -- * Processor
  , ProofData
  , abortWith
  , succeedWith
  , succeedWith0
  , succeedWith1
  , succeedWithId
  -- ** Argument
  , Argument
  , ArgFlag (..)
  , nat
  , bool
  , strat
  , flag
  , string
  , some
  , optional
  , withDomain
  -- ** Declaration
  , Declaration
  , Declared (..)
  , (:->)
  , OneTuple (..)
  -- ** Argument and Declaration Modifyer
  , withName
  , withHelp
  -- ** Strategy and Combinators
  , module Tct.Core.Data.Strategy
  , close
  , failing
  ) where


import           Tct.Core.Data
import           Tct.Core.Data.Strategy
import           Tct.Core.Main
import           Tct.Core.Processor.Failing


-- | Sets the default Strategy.
withDefaultStrategy :: TctConfig i -> Strategy i i -> TctConfig i
withDefaultStrategy cfg st = cfg { defaultStrategy = st }

-- | Sets 'GHCiScript'; and appends the given script.
appendGHCiScript :: TctConfig i -> [String] -> TctConfig i
appendGHCiScript cfg ss = cfg { interactiveGHCi = k (interactiveGHCi cfg) ss}
  where
    k (GHCiCommand _) xs   = GHCiScript Nothing xs
    k (GHCiScript m s1) xs = GHCiScript m (s1 ++ xs)

-- | Adds a key-value pair to the runtime options.
addRuntimeOption :: TctConfig i -> String -> [String] -> TctConfig i
addRuntimeOption cfg s ss = cfg { runtimeOptions = (s,ss) :runtimeOptions cfg }

