-- | This module provides a wrapper for "OptParse.Applicative" (<http://hackage.haskell.org/package/optparse-applicative>).
module Tct.Core.Common.Options
  (
  Options
  -- * Builder
  , eopt
  , withArgLong
  , withCropped
  , withDefault
  , withHelpDoc
  , withMetavar
  -- * Option Types
  , option
  , option'
  , alt
  , unit
  , switch
  , flag
  , argument
  , argument'
    -- * Finisher
  , mkArgParser
  ) where


import           Data.Monoid                          ((<>))
import qualified Options.Applicative                  as O

import           Tct.Core.Common.Pretty               (Doc)

-- | Type synonym for 'Options.Applicative.Type.Parser'.
type Options a = O.Parser a

-- | Identity Option.
eopt :: Show a => O.Mod f a
eopt = O.idm <> O.showDefault

-- | Sets the id for the argument. The id is parsed as "--id".
withArgLong :: O.HasName f => O.Mod f a -> String -> O.Mod f a
opt `withArgLong` str = opt <> O.long str

-- | The short version of 'withArgLong'.
withCropped :: O.HasName f => O.Mod f a -> Char -> O.Mod f a
opt `withCropped` c = opt <> O.short c

-- | Sets a default value for an option.
withDefault :: (O.HasValue f, Show a) => O.Mod f a -> a -> O.Mod f a
opt `withDefault` a = opt <> O.value a

-- | Sets the description for an option.
withHelpDoc :: O.Mod f a -> Doc -> O.Mod f a
opt `withHelpDoc` doc = opt <> O.helpDoc (Just doc)

-- | Sets the meta variable. Default meta variable is 'ARG'.
withMetavar :: O.HasMetavar f => O.Mod f a -> String -> O.Mod f a
opt `withMetavar` str = opt <> O.metavar str

-- | Constructs and Option given the empty option together with its modifiers.
option :: Read a => O.Mod O.OptionFields a -> O.Parser a
option = O.option O.auto

-- | Constructs and Option given the empty option together with its modifiers.
option' :: (String -> O.ReadM a) -> O.Mod O.OptionFields a -> O.Parser a
option' f = O.option (O.str >>= f)

-- | An optional argument. Returns 'Nothing' if the argument is not given.
alt :: O.Alternative f => f a -> f (Maybe a)
alt = O.optional

-- | Constructs a hidden unit option. This option is ignored during parsing.
unit :: O.Parser ()
unit = O.option O.auto $ O.idm `withDefault` () <> O.internal <> O.hidden

-- | A Flag option..
flag :: a -> a -> O.Mod O.FlagFields a -> O.Parser a
flag = O.flag

-- | A Boolesche Option, which is 'False' per default.
switch :: O.Mod O.FlagFields Bool -> O.Parser Bool
switch = O.switch

-- | An argument option without any id.
argument :: Read a => O.Mod O.ArgumentFields a -> O.Parser a
argument = O.argument O.auto

-- | An argument option without any id.
argument' :: (String -> O.ReadM a) -> O.Mod O.ArgumentFields a -> O.Parser a
argument' f = O.argument (O.str >>= f)

-- | Given a parser and a description this function constructs the actual parser.
mkArgParser :: Show a => O.Parser a -> Doc -> O.ParserInfo a
mkArgParser par doc = O.info par (O.fullDesc <> O.progDescDoc (Just doc))

