-- | This module defines the most important types.
module Tct.Core.Data.Types where


import           Control.Monad.Except          (MonadError)
import           Control.Monad.Reader          (MonadIO, MonadReader, ReaderT)
import           Data.Dynamic                  (Dynamic)
import qualified Data.Map                      as M
import           Data.Typeable
import qualified System.Time                   as Time
import           Text.Parsec                   (alphaNum, letter, oneOf)
import qualified Text.Parsec                   as P ((<|>))
import qualified Text.Parsec.Language          as PL
import qualified Text.Parsec.Token             as PT
import           Text.ParserCombinators.Parsec (CharParser)

import qualified Tct.Core.Common.Pretty        as PP
import qualified Tct.Core.Common.Xml           as Xml
import qualified Tct.Core.Data.Certificate     as C
import           Tct.Core.Data.Forks           (Id (..))


--- * TctM Monad -----------------------------------------------------------------------------------------------------

-- | Provides /TcT/ runtime options. The State of 'Tct.Core.Data.TctM.TcTM' monad.
-- The state can be (locally) updated using 'Tct.Core.Data.TctM.setState'.
data TctROState = TctROState
  { startTime     :: Time.ClockTime        -- ^ Start time. Should be set in the start state.
  , stopTime      :: Maybe Time.ClockTime  -- ^ Stop time. Used to handle timeouts and is updated when 'Tct.Core.Data.TctM.timed' is used.
  , tempDirectory :: FilePath              -- ^ The temporary directory. Should be set in the start state.
  , kvPairs       :: M.Map String [String] -- ^ Key-Value pairs. This field is application dependent.
  }

-- | The Tct monad.
newtype TctM r = TctM { runTctM :: ReaderT TctROState IO r }
  deriving (Monad, Applicative, MonadIO, MonadReader TctROState, Functor, MonadError IOError)

-- | Defines the (read-only) runtime status of 'TctROState'.
data TctStatus prob = TctStatus
  { currentProblem :: prob      -- ^ Current Problem.
  , runningTime    :: Int       -- ^ Runing time in seconds.
  , remainingTime  :: Maybe Int -- ^ Remaining time in seconds.
  }


--- * Proof Trees ----------------------------------------------------------------------------------------------------

-- | Reason for failure of a 'Processor'
data Reason where
  IOError  :: IOError -> Reason
  Aborted  :: String -> Reason
  TimedOut :: Reason
  Failed   :: (Show proc, ProofData prob, Show reason, PP.Pretty reason) => proc -> prob -> reason -> Reason

deriving instance Show Reason

instance PP.Pretty Reason where
  pretty (IOError io)         = PP.text (show io)
  pretty (Aborted s)          = PP.text s
  pretty TimedOut             = PP.text "timed out"
  pretty (Failed proc prob r) = PP.text (show proc) PP.<$$> PP.pretty prob PP.<$$> PP.pretty r

-- | A 'ProofNode' stores the necessary information to construct a (formal) proof from the application of a 'Processor'.
data ProofNode p = ProofNode
  { appliedProcessor :: p
  , problem          :: In p
  , proof            :: ProofObject p }

-- | A 'ProofTree' is constructed by applying a 'Tct.Core.Strategy' to a problem.
data ProofTree o where
  Open     :: o -> ProofTree o
  Success  :: Processor p => ProofNode p -> CertificateFn p -> Forking p (ProofTree o) -> ProofTree o
  Failure  :: Reason -> ProofTree o


instance Functor ProofTree where
  f `fmap` Open l             = Open (f l)
  _ `fmap` (Failure r)        = Failure r
  f `fmap` Success pn cns pts = Success pn cns ((f `fmap`) `fmap` pts)

instance Foldable ProofTree where
  f `foldMap` Open l          = f l
  _ `foldMap` Failure{}       = mempty
  f `foldMap` Success _ _ pts = (f `foldMap`) `foldMap` pts

instance Traversable ProofTree where
  f `traverse` Open l  = Open <$> f l
  _ `traverse` Failure r = pure (Failure r)
  f `traverse` Success pn cfn pts = Success pn cfn <$> (f `traverse`) `traverse` pts

instance Show (ProofTree l) where
  show _ = "showTree"

--- * Processor  -----------------------------------------------------------------------------------------------------

-- | 'Fork' is an abstract type that provides the "Foldable", "Functor" and "Traversable" interface.
type Fork t = (Foldable t, Functor t, Traversable t)

-- | Provides the interface for the proof construction.
-- All types which occur in the proof construction have to implement 'ProofData'.
type ProofData d = (Show d, PP.Pretty d, Xml.Xml d, Typeable d)

-- | Type synonym for functions that defines how a 'C.Certificate' is computed from a collection of 'C.Certificate's.
type CertificateFn p = Forking p C.Certificate -> C.Certificate

-- | Wrapper for (pretty)printable reason.
data SomeReason where SomeReason :: (Show r, PP.Pretty r) => r -> SomeReason

-- | The return type of an application of a processors.
data Return p
  = NoProgress SomeReason
  | Progress (ProofObject p) (CertificateFn p) (Forking p (ProofTree (Out p)))

-- | Everything that is necessary for defining a processor.
class (Show p, ProofData (ProofObject p), ProofData (In p), Fork (Forking p)) => Processor p where
  type ProofObject p :: *                                           -- ^ The type of the proof.
  type In p          :: *                                           -- ^ The type of the input problem.
  type Out p         :: *                                           -- ^ The type of the output problem.
  type Forking p     :: * -> *                                      -- ^ The type of the (children) collection.
  execute            :: p -> In p -> TctM (Return p)

  type Forking p     =  Id


-- Strategy ----------------------------------------------------------------------------------------------------------

-- | A 'Strategy' composes instances of 'Processor' and specifies in which order they are applied.
-- For a detailed description of the combinators see "Tct.Combinators".
data Strategy i o where
  Apply       :: (Processor p) => p -> Strategy (In p) (Out p)

  Seq         :: Strategy i q -> Strategy q o -> Strategy i o
  IdStrategy  :: Strategy i i

  Alt         :: Strategy i o -> Strategy i o -> Strategy i o
  Abort       :: Strategy i o

  Force       :: Strategy i o -> Strategy i o
  Ite         :: Strategy i q -> Strategy q o -> Strategy i o -> Strategy i o

  -- | Parallel Application
  Par         :: Strategy i o -> Strategy i o
  Race        :: Strategy i o -> Strategy i o -> Strategy i o
  Better      :: (ProofTree o -> ProofTree o -> Ordering) -> Strategy i o -> Strategy i o -> Strategy i o
  -- | Misc
  Timeout     :: RelTimeout -> Strategy i o -> Strategy i o
  Wait        :: RelTimeout -> Strategy i o -> Strategy i o
  WithStatus  :: (TctStatus i -> Strategy i o) -> Strategy i o
  WithState   :: (TctROState -> TctROState) -> Strategy i o -> Strategy i o

data RelTimeout = TimeoutIn Int | TimeoutUntil Int


-- Heterogenous List -------------------------------------------------------------------------------------------------

-- | A heterogenous list.
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList t -> HList (a ': t)

type family HListOf a :: [*] where
  HListOf ()                                             = '[]
  HListOf (a1,a2)                                        = '[a1,a2]
  HListOf (a1,a2,a3)                                     = '[a1,a2,a3]
  HListOf (a1,a2,a3,a4)                                  = '[a1,a2,a3,a4]
  HListOf (a1,a2,a3,a4,a5)                               = '[a1,a2,a3,a4,a5]
  HListOf (a1,a2,a3,a4,a5,a6)                            = '[a1,a2,a3,a4,a5,a6]
  HListOf (a1,a2,a3,a4,a5,a6,a7)                         = '[a1,a2,a3,a4,a5,a6,a7]
  HListOf (a1,a2,a3,a4,a5,a6,a7,a8)                      = '[a1,a2,a3,a4,a5,a6,a7,a8]
  HListOf (a1,a2,a3,a4,a5,a6,a7,a8,a9)                   = '[a1,a2,a3,a4,a5,a6,a7,a8,a9]
  HListOf (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0)                = '[a1,a2,a3,a4,a5,a6,a7,a8,a9,b0]
  HListOf (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1)             = '[a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1]
  HListOf (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2)          = '[a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2]
  HListOf (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3)       = '[a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3]
  HListOf (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4)    = '[a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4]
  HListOf (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4,b5) = '[a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4,b5]
  HListOf (OneTuple a)                                   = '[a]

class ToHList a                                                 where toHList :: a -> HList (HListOf a)
instance ToHList ()                                             where toHList ()                                             = HNil
instance ToHList (a1,a2)                                        where toHList (a1,a2)                                        = HCons a1 (HCons a2 HNil)
instance ToHList (a1,a2,a3)                                     where toHList (a1,a2,a3)                                     = HCons a1 (toHList (a2,a3))
instance ToHList (a1,a2,a3,a4)                                  where toHList (a1,a2,a3,a4)                                  = HCons a1 (toHList (a2,a3,a4))
instance ToHList (a1,a2,a3,a4,a5)                               where toHList (a1,a2,a3,a4,a5)                               = HCons a1 (toHList (a2,a3,a4,a5))
instance ToHList (a1,a2,a3,a4,a5,a6)                            where toHList (a1,a2,a3,a4,a5,a6)                            = HCons a1 (toHList (a2,a3,a4,a5,a6))
instance ToHList (a1,a2,a3,a4,a5,a6,a7)                         where toHList (a1,a2,a3,a4,a5,a6,a7)                         = HCons a1 (toHList (a2,a3,a4,a5,a6,a7))
instance ToHList (a1,a2,a3,a4,a5,a6,a7,a8)                      where toHList (a1,a2,a3,a4,a5,a6,a7,a8)                      = HCons a1 (toHList (a2,a3,a4,a5,a6,a7,a8))
instance ToHList (a1,a2,a3,a4,a5,a6,a7,a8,a9)                   where toHList (a1,a2,a3,a4,a5,a6,a7,a8,a9)                   = HCons a1 (toHList (a2,a3,a4,a5,a6,a7,a8,a9))
instance ToHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0)                where toHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0)                = HCons a1 (toHList (a2,a3,a4,a5,a6,a7,a8,a9,b0))
instance ToHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1)             where toHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1)             = HCons a1 (toHList (a2,a3,a4,a5,a6,a7,a8,a9,b0,b1))
instance ToHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2)          where toHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2)          = HCons a1 (toHList (a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2))
instance ToHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3)       where toHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3)       = HCons a1 (toHList (a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3))
instance ToHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4)    where toHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4)    = HCons a1 (toHList (a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4))
instance ToHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4,b5) where toHList (a1,a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4,b5) = HCons a1 (toHList (a2,a3,a4,a5,a6,a7,a8,a9,b0,b1,b2,b3,b4,b5))
instance ToHList (OneTuple a)                                   where toHList (OneTuple a)                                   = HCons a HNil

-- | Should be used in 'strategy' ('declareProcessor') if the Strategy (Processor) has a single argument.
newtype OneTuple a = OneTuple a


-- Currying / Uncurrying ---------------------------------------------------------------------------------------------

data as :-> b = HList as :-> b
infix 4 :->

-- | Uncurried version of a function.
type family Uncurry a where
  Uncurry ('[] :-> r) = r
  Uncurry (a ': as :-> r) = a -> Uncurry (as :-> r)


-- | Return type of function wrt to its argument list.
type family Ret as f where
  Ret '[] b = b
  Ret (a ': as) (a -> b) = Ret as b


--- * Declarations ---------------------------------------------------------------------------------------------------

-- | Specifies if the Argument is optional or required.
-- This mainly affects parsing of strategies and the the default function ('defaultFun') of declarations.
data ArgFlag = Optional | Required

-- | Meta information of arguments.
data ArgMeta = ArgMeta {argName_ :: String, argDomain_ :: String, argHelp_ :: [String]} deriving Show

data Argument (f :: ArgFlag) t where
  SimpleArg   :: ArgMeta -> SParser t -> Argument 'Required t
  FlagArg     :: Show t  => ArgMeta -> [t] -> Argument 'Required t
  StrategyArg :: ArgMeta -> [StrategyDeclaration i o] -> Argument 'Required (Strategy i o)

  SomeArg     :: Argument 'Required t -> Argument 'Required (Maybe t)
  OptArg      :: (Show t, Typeable t) => Argument 'Required t -> t -> Argument 'Optional t

-- | Associates the types to a list of arguments.
type family ArgsType a where
  ArgsType (Argument r a ': as) = a ': ArgsType as
  ArgsType '[]                  = '[]

-- | A declaration associates a function with name, description, arguments.
data Declaration :: * -> * where
  Decl :: (f ~ Uncurry (ArgsType args :-> Ret (ArgsType args) f)) =>
    String -> [String] -> f -> HList args -> Declaration (args :-> Ret (ArgsType args) f)

-- | Specifies the construction of a argument parser.
class ParsableArgs ats where
  mkOptParser :: HList ats -> [SParser (String,Dynamic)]
  mkArgParser :: HList ats -> [(String, Dynamic)] -> SParser (HList (ArgsType ats))

data SomeArgument where
  SomeArgument :: Argument f t -> SomeArgument

-- | Collects the meta information of a list of arguments.
class ArgsInfo as where
  argsInfo ::
    HList as ->                                -- A heterogenous list of arguments.
    [(String, String, [String], Maybe String)] -- A list of (name, domain, description, default value)
  toArgList :: HList as -> [SomeArgument]

-- | Open type for declarations. Allows to provide problem specific declarations in executables.
class Declared i o where
  decls :: [StrategyDeclaration i o]

-- | Existential type for declarations specifying a Strategy.
-- Mainly used for parsing and description.
data StrategyDeclaration i o where
  SD :: (ParsableArgs args, ArgsInfo args) => Declaration (args :-> Strategy i o) -> StrategyDeclaration i o


--- * Parsing --------------------------------------------------------------------------------------------------------

type SPState = ()
type SParser = CharParser SPState

-- | Specified Tokenparser.
strategyTP :: PT.TokenParser st
strategyTP = PT.makeTokenParser style
  where
    style = PL.emptyDef
      { PT.commentStart   = "{-"
      , PT.commentEnd     = "-}"
      , PT.commentLine    = "--"
      , PT.nestedComments = True
      , PT.identStart     = letter
      , PT.identLetter    = alphaNum P.<|> oneOf "_'"
      , PT.reservedOpNames= ["try", "force"]
      , PT.reservedNames  = [">>>", ">||", "<>", "<||>" ]
      , PT.caseSensitive  = True }


--- * Modifyers ------------------------------------------------------------------------------------------------------

-- | Update of meta information.
class WithName a where  withName :: a -> String -> a

-- | Update of meta information.
class WithHelp a where  withHelp :: a -> [String] -> a

