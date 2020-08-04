-- | A Strategy for logging.
module Tct.Core.Processor.Log
  ( logging
  , logging'
  ) where


import           Control.Monad.Trans    (liftIO)
import           Debug.Trace            (traceIO)

import qualified Tct.Core.Common.Pretty as PP
import           Tct.Core.Data


data Logging = Logging | NoLogging
  deriving (Show, Enum, Bounded)

data Sink = StdOut | StdErr
  deriving (Show, Enum, Bounded)

data Log i where
  Log :: PP.Pretty msg => Logging -> Sink -> msg -> Log i

instance Show (Log i)      where show   (Log _ _ msg) = PP.display $ PP.pretty msg
instance PP.Pretty (Log i) where pretty (Log _ _ msg) = PP.pretty msg

instance ProofData i => Processor (Log i) where
  type ProofObject (Log i) = ()
  type In (Log i)          = i
  type Out (Log i)         = i

  execute (Log NoLogging _ _) _    = abortWith ""
  execute (Log Logging sink msg) _ = liftIO (put $ PP.display $ PP.pretty msg) >> abortWith ""
    where put = case sink of {StdOut -> putStrLn; StdErr -> traceIO}


-- | Log message to stderr. Behaves like identity.
logging :: ProofData i => PP.Pretty msg => msg -> Strategy i i
logging = try . logging' Logging StdErr

-- | Log message. Behaves like identity.
logging' :: ProofData i => PP.Pretty msg => Logging -> Sink -> msg -> Strategy i i
logging' l sink msg = try $ processor (Log l sink msg :: ProofData i => Log i)

