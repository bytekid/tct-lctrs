-- | This module provides the /TcT/ monad.
module Tct.Core.Data.TctM
  (
  -- * Tct Monad
  TctM (..)
  , TctROState (..)
  , TctStatus (..)
  , askState
  , setState
  , setKvPair
  , getKvPair
  , askStatus
  -- * Lifted IO functions
  , async
  , wait
  , timed
  , paused
  , raceWith
  , concurrently
  ) where


import           Control.Applicative      ((<|>))
import           Control.Concurrent       (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad            (liftM)
import           Control.Monad.Reader     (ask, liftIO, local, runReaderT)
import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe)
import qualified System.Time              as Time

import           Tct.Core.Data.Types


-- | Returns the state of the Monad.
askState :: TctM TctROState
askState = ask

-- | Sets (locally) the state of the Monad.
setState :: (TctROState -> TctROState) -> TctM a -> TctM a
setState = local

-- | Sets (locally) a key-value pair.
setKvPair :: (String, [String]) -> TctM a -> TctM a
setKvPair (k,v) = local $ \st -> st { kvPairs = M.insert k v (kvPairs st) }

-- | Given a key; asks for a value. Returns [] if there is no value.
getKvPair :: String -> TctM [String]
getKvPair s = (get . kvPairs) <$> ask
  where get m = [] `fromMaybe` M.lookup s m

-- | Returns 'TctStatus' which is obtained from 'TctROState' during runtime.
--
-- > runningTime   = now - startTime
-- > remainingTime = max (0, now - stopTime)
askStatus :: prob -> TctM (TctStatus prob)
askStatus prob = do
  st <- askState
  now <- liftIO Time.getClockTime
  return TctStatus
    { currentProblem = prob
    , runningTime    = Time.tdSec (Time.diffClockTimes now (startTime st))
    , remainingTime  = (max 0 . Time.tdSec . flip Time.diffClockTimes now) `fmap` stopTime st }

toIO :: TctM a -> TctM (IO a)
toIO m = runReaderT (runTctM m) `fmap` askState

-- | Lifts 'Async.async'.
async :: TctM a -> TctM (Async.Async a)
async m = toIO m >>= liftIO . Async.async

-- | Lifts 'Async.wait'.
wait :: Async.Async a -> TctM a
wait = liftIO . Async.wait

--waitEither :: Async.Async a -> Async.Async b -> TctM (Either a b)
--waitEither a1 a2 = liftIO $ Async.waitEither a1 a2

-- | Lifts 'Async.concurrently'.
concurrently :: TctM a -> TctM b -> TctM (a,b)
concurrently m1 m2 = do
  io1 <- toIO m1
  io2 <- toIO m2
  liftIO $ Async.withAsync io1 $ \a1 ->
    liftIO $ Async.withAsync io2 $ \a2 ->
    liftIO $ Async.waitBoth a1 a2

-- | @'raceWith' p1 m1 m2@ runs @m1@ and @m2@ in parallel.
--
-- * Returns the first result that satisfies @p1@.
-- * Otherwise returns the latter result.
raceWith :: (a -> Bool) -> TctM a -> TctM a -> TctM a
raceWith p1 m1 m2 = do
  io1 <- toIO m1
  io2 <- toIO m2
  liftIO $ raceWithIO p1 io1 io2

raceWithIO :: (a -> Bool) -> IO a -> IO a -> IO a
raceWithIO p1 m1 m2 =
  Async.withAsync m1 $ \a1 ->
  Async.withAsync m2 $ \a2 -> do
    e <- Async.waitEither a1 a2
    case e of
      Left r1
        | p1 r1     -> Async.cancel a2 >> return r1
        | otherwise -> Async.wait a2
      Right r2
        | p1 r2     -> Async.cancel a1 >> return r2
        | otherwise -> Async.wait a1

-- | @'timed' seconds m@ wraps the Tct action in timeout, and locally sets 'stopTime'.
-- When @seconds@
--
--  * is negative, no timeout is set;
--  * is @0@, the computation aborts immediately, returning 'Nothing';
--  * is positive the computation runs at most @i@ seconds.
--
-- Returns 'Nothing' if @m@ does not end before the timeout.
--
-- Sets 'stopTime'.
--
-- > stopTime = min (now + timeout) stopTime
timed :: Int -> TctM a -> TctM (Maybe a)
timed n m
  | n < 0  = Just `liftM` m
  | n == 0 = return Nothing
  | otherwise = do
    e <- toIO m' >>= liftIO . Async.race (threadDelay $ toMicroSec n)
    return $ either (const Nothing) Just e
    where
      m' = do
        Time.TOD sec pico <- liftIO Time.getClockTime
        let newTime = Just $ Time.TOD (sec + toInteger n) pico
        local (\ r -> r { stopTime = min newTime (stopTime r) <|> newTime }) m

-- | @'wait' seconds m@ pauses seconds
paused :: Int -> TctM a -> TctM a
paused n m
  | n <= 0 = m
  | otherwise = liftIO (threadDelay (toMicroSec n)) >> m

toMicroSec :: Num a => a -> a
toMicroSec i = i*1000000

