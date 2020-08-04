-- | A declaration associates meta-information such as name and description, to a function.
module Tct.Core.Data.Declaration
  (
  -- * Declaration
  Declaration (..)
  , declare
  , OneTuple (..)
  , declName
  , declHelp
  , declFun
  , declArgs
  , deflFun
  -- * Arguments
  , Argument (..)
  , ArgFlag (..)
  , ArgMeta (..)
  , setArgMeta
  , argMeta
  , argName
  , argDomain
  , argHelp
  , argDefault
  , ArgsInfo (..)
  -- ** Argument Constructors
  , arg
  , Nat
  , nat
  , bool
  , string
  , strat
  , flag
  -- ** Argument Modifiers
  , optional
  , some
  , withDomain
  ) where


import           Data.List              (intercalate)
import           Data.Typeable
import qualified Text.Parsec as Parsec
import qualified Tct.Core.Common.Parser as P
import qualified Tct.Core.Common.Pretty as PP
import           Tct.Core.Data.Types


declare ::
  (ToHList a, HListOf a ~ args, Uncurry (ArgsType args :-> Ret (ArgsType args) f) ~ f) =>
    String      -- ^ name/identifuer
    -> [String] -- ^ description
    -> a        -- ^ heterogenous list of arguments
    -> f        -- ^ function
    -> Declaration (args :-> Ret (ArgsType args) f)
declare n desc as p = Decl n desc p (toHList as)

-- | Returns the name of a 'Declaration'.
declName :: Declaration c -> String
declName (Decl n _ _  _) = n

-- | Returns the description of a 'Declaration'.
declHelp :: Declaration c -> [String]
declHelp (Decl _ h _  _) = h

-- | Returns the function of a 'Declaration'.
declFun :: Declaration (args :-> r) -> Uncurry (ArgsType args :-> r)
declFun (Decl _ _ f  _) = f

-- | Returns the arguments of a 'Declaration'.
declArgs :: Declaration (args :-> r) -> HList args
declArgs (Decl _ _ _ as) = as

instance WithName (Declaration c) where withName (Decl _ h f as) n' = Decl n' h f as
instance WithHelp (Declaration c) where withHelp (Decl n _ f as) h' = Decl n h' f as

-- | Specifies the default function of a 'Declaration'.
type family DeflFun c where
  DeflFun ('[] :-> r) = r
  DeflFun (Argument 'Optional a ': as :-> r) = DeflFun (as :-> r)
  DeflFun (Argument 'Required a ': as :-> r) = a -> DeflFun (as :-> r)

-- | Specifies the default function of a 'Declaration'.
-- The default function instantiates all optional arguments with their default value.
class DefF c where
  deflFun :: Declaration c -> DeflFun c

instance DefF ('[] :-> f) where
  deflFun (Decl _ _ f _) = f

instance (DefF (as :-> r)) => DefF (Argument 'Optional a ': as :-> r) where
  deflFun (Decl n h f (HCons a as)) = deflFun (Decl n h (f (argDefault a)) as)

instance DefF (as :-> r) => DefF (Argument 'Required a ': as :-> r) where
  deflFun (Decl n h f (HCons _ as)) = \ a' -> deflFun (Decl n h (f a') as)


--- * arguments ------------------------------------------------------------------------------------------------------

-- | Generic argument with name "arg" and domain "<arg>".
arg :: String -> String -> [String] -> SParser t -> Argument 'Required t
arg n d h = SimpleArg ArgMeta{argName_ = n, argDomain_ = d, argHelp_ = h}

-- | Transforms a required argument to an optional by providing a default value.
optional :: (Show t, Typeable t) => Argument 'Required t -> t -> Argument 'Optional t
optional = OptArg

-- | Wraps an argument into 'Maybe'.
some :: Argument 'Required a -> Argument 'Required (Maybe a)
some = SomeArg

-- * instances
type Nat = Int

-- | Specifies a natural number argument.
nat :: String -> [String] -> Argument 'Required Nat
nat n h = arg n "nat" h P.nat

-- | Specifies a bool argument.
bool :: String -> [String] -> Argument 'Required Bool
bool = flag

-- | Specifies a string argument.
string :: String -> [String] -> Argument 'Required String
string n h = arg n "string" h parser where
 parser = Parsec.try (quoted '\'') Parsec.<|> Parsec.try (quoted '"') Parsec.<|> P.identifier
 quoted q = P.lexeme (Parsec.between (Parsec.char q) (Parsec.char q) (Parsec.many (Parsec.noneOf [q])))

-- | Specifies a strategy argument with name "strategy" and domain "<strategy>".
strat :: Declared i o => String -> [String] -> Argument 'Required (Strategy i o)
strat n h = StrategyArg ArgMeta{ argName_ = n, argDomain_ = "strategy", argHelp_ = h } decls

-- | Specifies a flag argument.
flag :: (Show t, Bounded t, Enum t) => String -> [String] -> Argument 'Required t
flag n h = FlagArg ArgMeta{ argName_ = n, argDomain_ = ds, argHelp_ = h } es
  where
    es = [minBound..]
    ds = intercalate "|" (map show es)

withDomain :: Argument r a -> [String] -> Argument r a
withDomain ar ns = case ar of
  (SimpleArg a p)    -> SimpleArg (k a) p
  (FlagArg a fs)     -> FlagArg (k a) fs
  (StrategyArg a ds) -> StrategyArg (k a) ds
  (SomeArg a)        -> SomeArg (withDomain a ns)
  (OptArg a t)       -> OptArg (withDomain a ns) t
  where k a = a { argDomain_ = intercalate "|" ns }

instance WithName ArgMeta        where withName ar n = ar { argName_ = n }
instance WithName (Argument r a) where withName ar n = setArgMeta (`withName` n) ar

instance WithHelp ArgMeta        where withHelp ar n = ar { argHelp_ = n }
instance WithHelp (Argument r a) where withHelp ar n = setArgMeta (`withHelp` n) ar

setArgMeta :: (ArgMeta -> ArgMeta) -> Argument f t -> Argument f t
setArgMeta k (SimpleArg a p)    = SimpleArg (k a) p
setArgMeta k (FlagArg a fs)     = FlagArg (k a) fs
setArgMeta k (StrategyArg a ds) = StrategyArg (k a) ds
setArgMeta k (SomeArg a)        = SomeArg (setArgMeta k a)
setArgMeta k (OptArg a t)       = OptArg (setArgMeta k a) t

argMeta :: Argument f t -> ArgMeta
argMeta (SimpleArg m _)   = m
argMeta (FlagArg m _)     = m
argMeta (StrategyArg m _) = m
argMeta (SomeArg a)       = argMeta a
argMeta (OptArg a _)      = argMeta a

argName :: Argument f t -> String
argName = argName_ . argMeta

argDomain :: Argument f t -> String
argDomain (SomeArg a) = "<none|" ++ argDomain a ++ ">"
argDomain ar          = argDomain_ (argMeta ar)

argHelp :: Argument f t -> [String]
argHelp = argHelp_ . argMeta

argDefault :: Argument 'Optional t -> t
argDefault (OptArg _ t) = t


instance ArgsInfo '[] where
  argsInfo HNil = []
  toArgList HNil = []

instance (Show a, ArgsInfo as) => ArgsInfo (Argument r a ': as) where
  argsInfo (HCons a as) = argsInfo' a :argsInfo as where
    argsInfo' :: Show t => Argument f t -> (String, String, [String], Maybe String)
    argsInfo' (OptArg b t) = let m = argMeta b in (argName_ m, argDomain_ m, argHelp_ m, Just $ show t)
    argsInfo' b            = let m = argMeta b in (argName_ m, argDomain_ m, argHelp_ m, Nothing)
  toArgList (HCons a as) = SomeArgument a : toArgList as

--- * proof data -----------------------------------------------------------------------------------------------------

instance ArgsInfo args => PP.Pretty (Declaration (args :-> c)) where
  pretty (Decl n h _ as) = PP.vcat $
    [ theName
    , theLine
    , if null h then PP.empty else PP.indent 4 theHelp PP.<$$> PP.empty
    , PP.indent 4 theSynopsis
    , PP.empty ]
    ++ (if null opts then [] else [PP.indent 2 theOptArgs])
    ++ (if null reqs then [] else [PP.indent 2 theReqArgs])
    ++ [PP.empty]

    where
      theName = PP.text "Strategy" PP.<+> PP.text n
      theLine = PP.text $ replicate (length $ "Strategy " ++ n) '-'
      theHelp = PP.paragraph (unlines h)
      theSynopsis = PP.text "Synopsis: " PP.<+> PP.text n PP.<+> PP.hsep (map mkSynopsis info)
        where
          mkSynopsis (_ , ad, _, Nothing) = PP.angles (PP.text ad)
          mkSynopsis (an, ad, _, _)       = PP.brackets $ PP.char ':' PP.<> PP.text an PP.<+> PP.angles (PP.text ad)
      theOptArgs = PP.text "Optional:" PP.<$$> PP.indent 2 (PP.vcat (map mkArgsInfo opts))
      theReqArgs = PP.text "Required:" PP.<$$> PP.indent 2 (PP.vcat (map mkArgsInfo reqs))

      info = argsInfo as
      (opts,reqs) = foldr k ([],[]) info
        where
          k a@(_,_,_,Nothing) (os,rs) = (os,a:rs)
          k a                 (os,rs) = (a:os,rs)
      mkArgsInfo (an, _, ah, Nothing) = mkArgsInfo' an ah
      mkArgsInfo (an, _, ah, Just s)  = mkArgsInfo' an ah PP.<$$> PP.indent 2 (PP.text "default:" PP.<+> PP.text s)
      mkArgsInfo' an ah               = PP.text an PP.<> if null ah then PP.empty else PP.empty PP.<$$> PP.indent 2 (PP.paragraph $ unlines ah)

