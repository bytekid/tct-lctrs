{-# LANGUAGE FlexibleContexts #-}
-- | This module re-exports SLogic.Smt and provides specialised commands for invoking the solver.
module Tct.Common.SMT
  (
  module SMT
  , smtSolveTctM
  , minismt, minismt'
  , yices, yices'
  , z3, z3'
  , minismtArgs, yicesArgs, z3Args
  -- encoding
  , encodePoly
  ) where


import           Control.Exception          (bracket)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.List                  ((\\))
import           Data.Maybe                 (fromMaybe)
import           System.IO                  (hClose, hFlush, hSetBinaryMode)
import           System.IO.Temp             (openTempFile)

import           SLogic.Smt                 as SMT hiding (minismt, minismt',
                                                    yices, yices', z3, z3')

import           Tct.Core.Common.Concurrent
import           Tct.Core.Common.Error      (throwError)
import qualified Tct.Core.Data              as T

import qualified Tct.Common.Polynomial      as P
import           Tct.Common.Ring


instance Additive (IExpr v) where
  zero = SMT.zero
  add  = (SMT..+)

instance Multiplicative (IExpr v) where
  one = SMT.one
  mul = (SMT..*)

instance AdditiveGroup (IExpr v) where
  neg = SMT.neg

-- | This is the preferred way to invoke a solver in 'TctM'.
-- Invokes the solver specified in the configuration. Currently "minismt", "z3" and "yices" are supported. If the
-- solver is undefined then "minismt" is used.
smtSolveTctM :: (Var v, Storing v) => prob -> SmtSolver T.TctM v
smtSolveTctM p st = do
  mso <- T.getKvPair "solver"
  tmp <- T.tempDirectory `fmap` T.askState
  mto <- T.remainingTime `fmap` T.askStatus p
  case mso of
    ("minismt":args)    -> minismt' mto (minismtArgs ++ (args \\ minismtArgs)) st
    ("yices":args)      -> yices'   args st
    ("yices-smt2":args) -> yices'   args st
    ("z3":args)         -> z3'      mto (z3Args ++ (args \\ z3Args)) st
    _                   -> defl mto
    where defl mto = minismt' mto ("-neg" : minismtArgs) st

-- | Default arguments. Needed to get things running.
minismtArgs, yicesArgs, z3Args :: Args
minismtArgs = ["-m", "-v2"]
yicesArgs   = ["-t", "10"]
z3Args      = ["-smt2"]

gSolver :: Cmd -> Args -> (t -> DiffFormat) -> (String -> Result v) -> t -> T.TctM (Result v)
gSolver cmd args formatter parser st = do
  tmp <- T.tempDirectory `fmap` T.askState
  let input = formatter st
  liftIO . withFile tmp $ \file hfile -> do
    hSetBinaryMode hfile True
    -- hSetBuffering hfile BlockBuffering
    hPutDiffFormat hfile input
    hFlush hfile
    hClose hfile
    either (throwError . userError) (return . parser) =<< spawn cmd (args ++ [file])
    where withFile tmp = bracket (openTempFile tmp "smt2x") (hClose . snd) . uncurry

-- | minismt solver instance
minismt' :: (Storing v, Var v)
  => Maybe Int      -- ^ optional timeout
  -> Args -> SmtSolver T.TctM v
minismt' mto args = gSolver "minismt" (args++to) minismtFormatter minismtParser
  where to = maybe [] (\i -> ["-t", show (max 1 i) ]) mto

-- | prop> minismt = minismt' Nothing Nothing minismtArgs
minismt :: (Storing v, Var v) => SmtSolver T.TctM v
minismt = minismt' Nothing minismtArgs

-- | yices solver instance
yices' :: (Storing v, Var v) => Args -> SmtSolver T.TctM v
yices' args = gSolver "yices-smt2" args yicesFormatter yicesParser

-- | prop> yices = yices' Nothing yicesArgs
yices :: (Storing v, Var v) => SmtSolver T.TctM v
yices = yices' yicesArgs

-- | z3 solver instance
z3' :: (Storing v, Var v)
  => Maybe Int      -- ^ optional timeout
  -> Args -> SmtSolver T.TctM v
z3' mto args = gSolver "z3" (args++to) z3Formatter z3Parser
  where to = maybe [] (\i -> ["-T:"++ show (max 1 i)]) mto

-- | prop> z3 = z3' Nothing Nothing z3Args
z3 :: (Storing v, Var v) =>  SmtSolver T.TctM v
z3 = z3' Nothing z3Args

-- | standard polynomial encoding
encodePoly :: Ord v => P.Polynomial Int v -> IExpr v
encodePoly ms = SMT.bigAdd (map encodeMono $ P.toView ms) where
  encodeMono (c,ps) = SMT.bigMul (SMT.num c: concatMap encodePower ps)
  encodePower (v,e) = replicate e (SMT.ivar v)

