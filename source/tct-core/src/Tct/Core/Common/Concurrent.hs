module Tct.Core.Common.Concurrent
  ( spawn
  ) where


import           Control.Concurrent
import           Control.Exception        (SomeException, mask, throwIO, try)
import qualified Control.Exception        as C
import           Control.Monad
import           Foreign.C
import           GHC.IO.Exception         (IOErrorType (..), IOException (..))
import           System.Exit              (ExitCode (..))
import           System.IO
import           System.Process           hiding (readCreateProcessWithExitCode, cleanupProcess)
import           System.Process.Internals


rnf :: String -> ()
rnf = foldr seq ()

-- | Replaces 'System.Process.readProcessWithExitCode' as this sometimes causes "waitForProcess: does not exist (No
-- child processes)" in our setting. Even when the computation is successful.
-- So we try to catch it here.
spawn :: String -> [String] -> IO (Either String String)
spawn cmd args = do
  (ex, out, err) <- readCreateProcessWithExitCode (proc cmd args) []
  return $ case ex of
    ExitFailure i -> Left ("Tct.Core.Common.Concurrent.spawn: Exitcode: " ++ show i ++ ": " ++ err)
    ExitSuccess   -> Right out

-- MS:
-- Taken from System.Process 1.2.3.0
-- 'System.Process.readCreateProcessWithExitCode' causes sometimes "waitForProcess: does not exist (No child process)"
-- We patch it.
readCreateProcessWithExitCode :: CreateProcess -> String -> IO (ExitCode,String,String)
readCreateProcessWithExitCode cp input = do
  let cp_opts = cp { std_in  = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  withCreateProcess_ "readCreateProcessWithExitCode" cp_opts $
    \(Just inh) (Just outh) (Just errh) ph -> do

      out <- hGetContents outh
      err <- hGetContents errh

      -- fork off threads to start consuming stdout & stderr
      withForkWait  (C.evaluate $ rnf out) $ \waitOut ->
        withForkWait (C.evaluate $ rnf err) $ \waitErr -> do

        -- now write any input
        unless (null input) $
          ignoreSigPipe $ hPutStr inh input
        -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
        ignoreSigPipe $ hClose inh

        -- wait on the output
        waitOut
        waitErr

        hClose outh
        hClose errh

      -- wait on the process
      ex <- waitForProcess ph

      return (ex, out, err)

withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `C.onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e ->
  case e of
    IOError { ioe_type  = ResourceVanished
            , ioe_errno = Just ioe }
      | Errno ioe == ePIPE -> return ()
    _ -> throwIO e

withCreateProcess_
  :: String
  -> CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ fun c action =
  C.bracketOnError (createProcess_ fun c) cleanupProcess
    (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr, ph@(ProcessHandle _ delegating_ctlc _)) = do
  terminateProcess ph
  maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
  maybe (return ()) hClose mb_stdout
  maybe (return ()) hClose mb_stderr
  when delegating_ctlc
    stopDelegateControlC
  -- _ <- forkIO (waitForProcess (resetCtlcDelegation ph) >> return ())
  -- MS: here is actually the only change
  _ <- forkIO $ void (waitForProcess (resetCtlcDelegation ph)) `C.catch` (\e -> void (return (e :: C.SomeException)))
  return ()
  where resetCtlcDelegation (ProcessHandle m _ waitPidLock) = ProcessHandle m False waitPidLock

