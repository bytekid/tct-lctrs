-- | This module re-exports "Control.Monad.Error" (<http://hackage.haskell.org/package/mtl>) and provides types for
-- custom error handling.
{-# LANGUAGE ScopedTypeVariables #-}
module Tct.Core.Common.Error
  ( module Control.Monad.Except
  , TctError (..)
  , ErroneousIO
  , runErroneousIO
  , tryIO
  , liftEither
  , liftMaybe
  , hush
  , note
  ) where


import           Control.Exception      (IOException, try)
import           Control.Monad.Except   hiding (liftEither)
import           Text.Parsec            (ParseError)

import qualified Tct.Core.Common.Pretty as PP

-- | Custom error type.
data TctError
  = TctProblemParseError String
  | TctStrategyParseError ParseError
  | TctIOError IOError
  | TctUnknownError String
  deriving Show

prettyError :: String -> PP.Doc -> PP.Doc
prettyError name reason = PP.align (PP.text name PP.<> PP.text ":" PP.</> PP.indent 2 reason)

instance PP.Pretty TctError where
  pretty (TctProblemParseError s)  = prettyError "Parsing of problem failed" (PP.text s)
  pretty (TctStrategyParseError s) = prettyError "Parsing of strategy failed" (PP.text (show s))
  pretty (TctIOError s)            = prettyError "IO error" (PP.text (show s))
  pretty (TctUnknownError s)       = prettyError "Unknown error" (PP.text (show s))

-- | Wraps 'IO' into an erroneous compuation with custom error handling.
type ErroneousIO e = ExceptT e IO

-- | Executes an erroneous computation.
--   Returns @'Left' e@ if if an error occured, and  @'Right' a@ if the computation was successfull.
runErroneousIO :: ErroneousIO e a -> IO (Either e a)
runErroneousIO = runExceptT

-- | Lifts an 'IO' computation to 'ErroneousIO'.
--   Returns 'TctIOError' if an error occured.
tryIO :: IO a -> ErroneousIO TctError a
tryIO io = ExceptT . liftIO $ do
  e :: Either IOException a <- try io
  return $ either (Left . TctIOError) Right e

-- | Lifts 'Either' to 'ErroneousIO'.
--   Indicates an error @e@ if the first argument is @'Left' e@.
liftEither :: Either e a -> ErroneousIO e a
liftEither = ExceptT . return

-- | Lifts 'Maybe' to 'ErroneousIO'.
--   Indicates an error @e@ if the second argument is 'Nothing'.
liftMaybe :: e -> Maybe a  -> ErroneousIO e a
liftMaybe e =  liftEither . note e

-- | Transforms 'Either' to 'Maybe'.
hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

-- | Transforms 'Maybe' to 'Either'.
note :: e -> Maybe a -> Either e a
note a = maybe (Left a) Right

