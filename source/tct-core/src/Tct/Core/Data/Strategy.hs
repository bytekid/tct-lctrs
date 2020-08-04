-- | This module provides the 'Strategy' type.
module Tct.Core.Data.Strategy
  (
  Strategy (..)
  -- * Strategies
  , identity
  , abort
  , processor
  , ite
  , (.>>>)
  , (.<|>)
  , try
  , force
  , exhaustively , es, te
  , exhaustivelyN
  , chain
  , chainWith
  , alternative
  , when
  , timeoutIn
  , timeoutUntil
  , timeoutRelative
  , timeoutRemaining
  , wait
  , waitUntil
  -- ** Parallel application
  , inParallel
  , (.>||>)
  , (.<||>)
  , (.<?>)
  , fastest
  , fastestN
  , best
  , cmpTimeUB
  -- ** Inspecting the Status
  , withState
  , withProblem
  , withKvPair
  -- * Strategy evaluation
  , evaluate
  , evaluate1
  -- * Declaration
  , StrategyDeclaration (..)
  , strategy
  ) where


import qualified Data.Map                  as M (insert)
import           Data.Maybe                (fromMaybe)
import qualified Data.Traversable          as F (traverse)

import qualified Tct.Core.Common.Pretty    as PP
import           Tct.Core.Data.Certificate (timeUB)
import           Tct.Core.Data.Processor
import           Tct.Core.Data.ProofTree
import           Tct.Core.Data.TctM        hiding (wait)
import qualified Tct.Core.Data.TctM        as TctM
import           Tct.Core.Data.Types
import           Tct.Core.Data.Declaration (declare)


instance Show (Strategy i o) where
  show _ = "someStrategy"

instance PP.Pretty (Strategy i o) where
  pretty _ = PP.text "someStrategy"

reltimeToTimeout :: RelTimeout -> TctM Int
reltimeToTimeout t = do
  running <- runningTime `fmap` askStatus undefined
  let to = max 0 (case t of { TimeoutIn secs -> secs; TimeoutUntil secs -> secs - running })
  remains <- (fromMaybe to . remainingTime) `fmap` askStatus undefined
  return (max 0 (min to (remains - 1)))

-- MS: error report is far from optimal.
-- * generating information from evaluate requires additional ProofData constraints
-- in principle they are always fullfilled since as basic blocks are processors, but ugly
-- * alternatively some logging functions when applying processors
-- * there are also some strange cases one may want to consider
-- eg assume that s fails; force (try s) vs force s, force (s >>> t) vs force (t >>> s)

-- | @'evaluate1' s prob@ defines the application of @s@ to a problem.
-- See "Combinators" for a detailed description.
evaluate1 :: Strategy i o -> i -> TctM (ProofTree o)
evaluate1 (Apply p)          prob = apply p prob

evaluate1 (Seq s1 s2)        prob = evaluate1 s1 prob >>= continue where
  continue pt
    | isFailing pt = evaluate Abort pt
    | otherwise    = evaluate s2 pt
evaluate1 IdStrategy         prob = return (Open prob)

evaluate1 (Alt s1 s2)        prob = evaluate1 s1 prob >>= continue where
  continue pt
    | isFailing pt = evaluate1 s2 prob
    | otherwise    = return pt
evaluate1 Abort              _    = return (Failure $ Aborted "aborted")

evaluate1 (Force s)          prob = evaluate1 s prob >>= continue where
  continue (Open _) = return (Failure $ Aborted "no progress")
  continue pt       = return pt
evaluate1 (Ite sb st se)     prob = evaluate1 sb prob >>= continue where
  continue pt
    | isProgressing pt = evaluate st pt
    | otherwise        = evaluate1 se prob
evaluate1 (Par s)            prob = evaluate1 s prob
evaluate1 (Race s1 s2)       prob =
  raceWith (not . isFailing) (evaluate1 s1 prob) (evaluate1 s2 prob)
evaluate1 (Better cmp s1 s2) prob =
  uncurry pick <$> concurrently (evaluate1 (timeoutRemaining s1) prob) (evaluate1 (timeoutRemaining s2) prob) where
    pick r1 r2 | cmp r2 r1 == GT = r2
               | otherwise       = r1
evaluate1 (Timeout t s) prob = do
  timeout <- reltimeToTimeout t
  fromMaybe (Failure TimedOut) <$> timed timeout (evaluate1 s prob)
evaluate1 (Wait t s) prob = do
  timeout <- reltimeToTimeout t
  paused timeout (evaluate1 s prob)
evaluate1 (WithStatus f)     prob = do
  st <- askStatus prob
  evaluate1 (f st) prob
evaluate1 (WithState f s)    prob =
  setState f (evaluate1 s prob)

-- | 'evaluate' on a 'ProofTree'.
evaluate, evaluateSeq,evaluatePar :: Strategy i o -> ProofTree i -> TctM (ProofTree o)
evaluate IdStrategy t = return t
evaluate (Par s)    t = evaluatePar s t
evaluate s          t = evaluateSeq s t

evaluateSeq s = substituteM (evaluate1 s)
evaluatePar s t = spawnTree t >>= collect where
  spawnTree = F.traverse (async . evaluate1 s)
  collect = substituteM TctM.wait


-- * Strategy Declaration ---------------------------------------------------------------------------------------------

-- |  Constructs a strategy declaration. For example: Assume that @st :: Int -> Maybe Int -> Strategy prob@.
--
-- > strategy "name" (nat, some nat) st
-- Similar to 'Declaration.declare' but specified to Strategies and with no description.
strategy ::
  ( ToHList as, HListOf as ~ args
  , f ~ Uncurry (ArgsType args :-> Ret (ArgsType args) f)
  , Ret (ArgsType args) f ~ Strategy prob prob)
  => String         -- ^ The name of the strategy.
  -> as             -- ^ The arguments as tuples: (), (OneTuple a1), (a1,a2) ...
  -> f              -- ^ The strategy.
  -> Declaration (args :-> Strategy prob prob)
strategy n = declare n []


-- * Strategies

infixr 5 .>>>, .>||>
infixr 6 .<|>, .<||>

identity :: Strategy i i
identity = IdStrategy

abort :: Strategy i o
abort = Abort

-- | lift a processor to a strategy
processor :: (Processor p) => p -> Strategy (In p) (Out p)
processor = Apply

-- | conditional
-- ite :: Strategy i q -> Strategy q o -> Strategy i o -> Strategy i o
-- ite = Ite Cond (not . isFailure)

ite :: Strategy i q -> Strategy q o -> Strategy i o -> Strategy i o
ite = Ite

-- | sequencing
(.>>>) :: Strategy i q -> Strategy q o -> Strategy i o
s1 .>>> s2 = Seq s1 s2 -- ite s1 s2 abort

-- | choice
(.<|>) :: Strategy i o -> Strategy i o -> Strategy i o
s1 .<|> s2 = Alt s1 s2 -- ite s1 identity s2

-- | @try s@ behaves like @s@, except in case of failure of @s@, @try s@ behaves like @identity@
try :: Strategy i i -> Strategy i i
try s = s .<|> identity

force :: Strategy i o -> Strategy i o
force = Force

-- | @'exhaustively' s@ repeatedly applies @s@ until @s@ fails.
-- Fails if the first application of @s@ fails.
exhaustively :: Strategy i i -> Strategy i i
exhaustively s =  force s .>>> try (exhaustively s)

-- | Like 'exhaustively'. But maximal @n@ times.
exhaustivelyN :: Int -> Strategy i i -> Strategy i i
exhaustivelyN n s
  | n > 1     = force s .>>> try (exhaustivelyN (n-1) s)
  | n == 1    = s
  | otherwise = identity

-- | Short for 'exhaustively'.
es :: Strategy i i -> Strategy i i
es = exhaustively

-- | prop> te st = try (exhaustively st)
te :: Strategy i i -> Strategy i i
te = try . exhaustively

-- | List version of ('.>>>').
--
-- prop> chain [] = identity
chain :: ProofData i => [Strategy i i] -> Strategy i i
chain [] = identity
chain ss = foldr1 (.>>>) ss

-- | Like 'chain' but additionally executes the provided strategy after each strategy of the list.
--
-- > chainWith [] (try empty)      == try empty
-- > chainWith [s1,s2] (try empty) == s1 .>>> try empty .>>> s2 .>>> try empty
chainWith :: ProofData i => Strategy i i -> [Strategy i i] -> Strategy i i
chainWith s [] = s
chainWith s ss = foldr1 (\t ts -> t .>>> s .>>> ts) ss .>>> s

-- | List version of ('.<|>').
--
-- prop> alternative [] = failing
alternative :: (ProofData i, Show o) => [Strategy i o] -> Strategy i o
alternative [] = abort
alternative ss = foldr1 (.<|>) ss


-- | @'when' b st@ applies @st@ if @b@ is true.
when :: Bool -> Strategy i i -> Strategy i i
when b st = if b then st else identity

timeoutIn,timeoutUntil :: Int -> Strategy i o -> Strategy i o
timeoutIn secs    = Timeout (TimeoutIn secs)
timeoutUntil secs = Timeout (TimeoutUntil secs)

-- | Sets timeout relative to the given percentage.
-- Useful together with total timeout.
--
-- > timeoutRelative (Just 60) 50 st = timeoutIn 30 st
-- > timeoutRelative Nothing   50 st = st
timeoutRelative :: Maybe Int -> Int -> Strategy i o -> Strategy i o
timeoutRelative mtotal percent st = maybe st timeout mtotal
  where timeout total = timeoutIn (floor (fromIntegral (total*percent :: Int) / 100 :: Double)) st

-- | Sets the timeout to the remaining time if set.
-- Useful for lifting timeout to processors.
timeoutRemaining :: Strategy i o -> Strategy i o
timeoutRemaining st = WithStatus $ \ state -> maybe st (`timeoutIn` st) (remainingTime state)

wait,waitUntil :: Int -> Strategy i o -> Strategy i o
wait secs      = Wait (TimeoutIn secs)
waitUntil secs = Wait (TimeoutUntil secs)

-- | apply given strategy in parallel to all open problems
inParallel :: Strategy i o -> Strategy i o
inParallel = Par

-- | parallel sequencing
(.>||>) :: Strategy i q -> Strategy q o -> Strategy i o
s1 .>||> s2 = s1 .>>> inParallel s2

-- | parallel choice
(.<||>) :: Strategy i o -> Strategy i o -> Strategy i o
(.<||>) = Race


-- | List version of ('<||>').
fastest :: (ProofData i, Show o) => [Strategy i o] -> Strategy i o
fastest [] = abort
fastest ss = foldr1 (.<||>) ss

-- | Like 'fastest'. But only runs @n@ strategies in parallel.
fastestN :: (ProofData i, Show o) => Int -> [Strategy i o] -> Strategy i o
fastestN _ [] = abort
fastestN n ss = fastest ss1 .<|> fastestN n ss2
  where (ss1,ss2) = splitAt n ss

-- | List version of ('<?>').
best :: (ProofData i, ProofData o) => (ProofTree o -> ProofTree o -> Ordering) -> [Strategy i o] -> Strategy i o
best _   [] = abort
best cmp ss = foldr1 (cmp .<?>) ss

-- | @('<?>') cmp s1 s2@ applies @ s1@ and @ s2@ in parallel, returning
-- the (successful) result @r1@ of strategy @s1@ iff @comp r1 r2 == GT@,
-- otherwise @r2@ is returned. An example for @comp@ is
--
-- > comp pt1 pt2 = flip compare (timeUB $ certificate pt1) (timeUB $ certificate pt2)
(.<?>) :: (ProofTree o -> ProofTree o -> Ordering) -> Strategy i o -> Strategy i o -> Strategy i o
(.<?>) = Better

-- -- | Compares time upperbounds. Useful with 'best'.
cmpTimeUB :: ProofTree i -> ProofTree i -> Ordering
cmpTimeUB pt1 pt2 = compare (tu pt2) (tu pt1)
  where tu = timeUB . certificate


-- | Applied strategy depends on run time status.
withState :: (TctStatus i -> Strategy i o) -> Strategy i o
withState = WithStatus

-- | Specialised version of 'withState'.
withProblem :: (i -> Strategy i o) -> Strategy i o
withProblem g = WithStatus (g . currentProblem)

-- | Sets a key-value pair for a strategy.
withKvPair :: (String, [String]) -> Strategy i o -> Strategy i o
withKvPair (k,v) = WithState $ \st -> st { kvPairs = M.insert k v (kvPairs st) }


--- * proof data -----------------------------------------------------------------------------------------------------

instance PP.Pretty (StrategyDeclaration i o) where
  pretty (SD s) = PP.pretty s

