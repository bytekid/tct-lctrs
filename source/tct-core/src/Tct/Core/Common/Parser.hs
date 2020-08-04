-- | This module re-export "Text.Parsec" (<http://hackage.haskell.org/package/parsec>)
--   and provides common parser tokens.
module Tct.Core.Common.Parser
  (
  module Text.Parsec
  -- * Basic Parsers
  , symbol
  , int
  , nat
  , bool
  , enum

  -- * Parenthesis
  , braces
  , parens

  -- * Lexeme and Special Parsers
  , lexeme
  , reserved
  , identifier
  , whiteSpace

  -- * Stateful
  , changeState
  , withState
  ) where


import           Text.Parsec
import qualified Text.Parsec.Token             as PT
import           Text.ParserCombinators.Parsec (CharParser)

import           Tct.Core.Data.Types           (strategyTP)


lexeme :: CharParser s a -> CharParser s a
lexeme = PT.lexeme strategyTP

symbol :: String -> CharParser s String
symbol = PT.symbol strategyTP

reserved :: String -> CharParser s ()
reserved = PT.reservedOp strategyTP

braces :: CharParser s a -> CharParser s a
braces = PT.braces strategyTP

parens :: CharParser s a -> CharParser s a
parens = PT.parens strategyTP

nat :: CharParser s Int
nat = fromInteger `fmap` PT.natural strategyTP

bool :: CharParser s Bool
bool = try (symbol "True" >> return True) <|> (symbol "False" >> return False)

enum :: (Bounded a, Enum a, Show a) => CharParser s a
enum = choice $ k `fmap` [minBound ..]
  where k b = try $ symbol (show b) >> return b

int :: CharParser s Integer
int = PT.natural strategyTP

identifier :: CharParser s String
identifier = PT.identifier strategyTP

whiteSpace :: CharParser s ()
whiteSpace = PT.whiteSpace strategyTP

-- MS: found this gem on stackoverflow
-- http://stackoverflow.com/questions/17968784/an-easy-way-to-change-the-type-of-parsec-user-state by Roman Cheplyaka
-- | Change the state type of parsec.
changeState
  :: (Functor m, Monad m)
  => (u -> v)
  -> (v -> u)
  -> ParsecT s u m a
  -> ParsecT s v m a
changeState forward backward = mkPT . transform . runParsecT
  where
    -- mapState :: (u -> v) -> State s u -> State s v
    mapState f st = st { stateUser = f (stateUser st) }

    -- mapReply :: (u -> v) -> Reply s u a -> Reply s v a
    mapReply f (Ok a st err) = Ok a (mapState f st) err
    mapReply _ (Error e) = Error e

    fmap3 = fmap . fmap . fmap

    -- transform
    --   :: (State s u -> m (Consumed (m (Reply s u a))))
    --   -> (State s v -> m (Consumed (m (Reply s v a))))
    transform p st = fmap3 (mapReply forward) (p (mapState backward st))

-- | Apply a parser with a different state.
withState :: (Functor m, Monad m) => v -> ParsecT s v m a -> ParsecT s u m a
withState v p = getState >>= \u -> changeState (const u) (const v) p

