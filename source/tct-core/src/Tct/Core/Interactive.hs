{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeOperators             #-}
-- | This module provides a basic interactive functionality via \ghci\.
--
-- __WARNING:__ all operations that are directly applied to a problem are __unsafe__. More precisely, the application
-- of a function to a problem is not type-safe: Given problems of type @i@. We can apply a strategy of type @Strategy
-- o o@ (where @o@ different from @i@). This can lead to undefined behaviour.
--
-- \At the momement user configurations and user options are ignored.\
module Tct.Core.Interactive
  (
  module M
  -- * Load and Modify Problems
  , load
  , load'
  , modifyProblems
  , onProblems
  , onTree

  -- * Select (Sub-)Problems
  -- | We can select sub-problems individually and use 'applySelected' to apply strategies only on selected
  -- sub-problems. Use 'printState' to  get a list of open sub-problems.
  , select
  , selectAll

  -- * Apply Strategies
  , apply
  , applySelected

  -- * Undo History
  , undo
  , reset

  -- * Print State
  -- | For simplicity the output is fixed; And unrelated to user-defined options and configurations. Use 'onProblems',
  -- 'onTree' to perform specific IO actions.
  , welcome
  , proof
  , state
  , complexity
  , describe
  , help
  , parse
  , list
  ) where


import           Tct.Core.Common.Pretty     as M (putPretty)
import           Tct.Core.Data.Strategy     as M hiding (Strategy, StrategyDeclaration, evaluate, strategy)

import           Control.Monad
import qualified Control.Monad              as W (when)
import qualified Control.Monad.State.Strict as S
import           Data.Either                (rights)
import qualified Data.Foldable              as F
import           Data.IORef
import qualified Data.Traversable           as F
import           System.IO.Unsafe

import           Tct.Core.Common.Error      (TctError (..), liftEither, runErroneousIO, tryIO)
import qualified Tct.Core.Common.Pretty     as PP
import qualified Tct.Core.Common.Xml        as Xml
import           Tct.Core.Data              hiding (apply, proof)
import           Tct.Core.Main
import           Tct.Core.Parse             (strategyFromString)
import           Tct.Core.Processor.Failing (failing)


--- * selection ------------------------------------------------------------------------------------------------------

type Selected l = Either l l

instance PP.Pretty l => PP.Pretty (Selected l) where
  pretty (Left _)  = PP.empty
  pretty (Right l) = PP.pretty l

instance Xml.Xml l => Xml.Xml (Selected l) where
  toXml = const Xml.empty

selectLeafs :: ProofData l => [Int] -> ProofTree (Selected l) -> ProofTree (Selected l)
selectLeafs ns pt = S.evalState (F.mapM k pt) 0
  where
    k p = S.modify succ >> S.get >>= \i -> return (either (f i) (f i) p)
    f i = if i `elem` ns then Right else Left

selectAllLeafs :: ProofTree (Selected l) -> ProofTree (Selected l)
selectAllLeafs = fmap (either Right Right)

removeSelection :: ProofTree (Selected l) -> ProofTree l
removeSelection = fmap (either id id)

-- MS: Strategies with (possible) different input output type have to be applied to all problems.
evaluateAll :: ProofData o => Strategy i o -> ProofTree (Selected i) -> TctM (ProofTree (Selected o))
evaluateAll s (Open (Left p))             = fmap Left `fmap` evaluate s (Open p)
evaluateAll s (Open (Right p))            = fmap Right `fmap` evaluate s (Open p)
evaluateAll _ (Failure r)                 = return (Failure r)
evaluateAll s (Success n certfn subtrees) = Success n certfn <$> (evaluateAll s `F.mapM` subtrees)

-- MA:TODO please check
evaluateSelected :: ProofData i => Strategy i i -> ProofTree (Selected i) -> TctM (ProofTree (Selected i))
evaluateSelected _ pt@(Open (Left _))          = return pt
evaluateSelected s (Open (Right p))            = fmap Right `fmap` evaluate s (Open p)
evaluateSelected _ (Failure r)                 = return (Failure r)
evaluateSelected s (Success n certfn subtrees) = Success n certfn <$> (evaluateSelected s `F.mapM` subtrees)


--- * state ----------------------------------------------------------------------------------------------------------

data St l where
  St :: ProofData l => ProofTree (Selected l) -> St l

unSt :: St l -> ProofTree (Selected l)
unSt (St pt) = pt

data a :+: b = a :+: b | Nil

null' :: (a :+: b) -> Bool
null' Nil = True
null' _   = False

head' :: (a :+: b) -> a
head' Nil       = error "Tct.Core.Interactive.head': empty list"
head' (a :+: _) = a

tail' :: (t :+: (a :+: b)) -> a :+: b
tail' Nil       = Nil
tail' (_ :+: b) = b

newtype State l = State { history_ :: l }

stateRef :: IORef (State (a :+: b))
stateRef = unsafePerformIO $ newIORef st
  where st = State { history_ = Nil }
{-# NOINLINE stateRef #-}

getState :: IO (State (a :+: b))
getState = readIORef stateRef

putState :: State (a :+: b) -> IO ()
putState = writeIORef stateRef

modifyState :: (State (a :+: b) -> State (c :+: d)) -> IO ()
modifyState f = getState >>= putState . f

initSt :: ProofData i => i -> IO ()
initSt st' = modifyState (\st -> st{ history_ = St (Open (Right st')) :+: Nil })

getSt :: IO (Maybe (St i))
getSt = do
  hst <- history_ `fmap` getState
  return $ if null' hst then Nothing else Just (head' hst)

maybeSt :: IO a -> (St i -> IO a) -> IO a
maybeSt a f = getSt >>= maybe a f

onSt :: (St i -> IO ()) -> IO ()
onSt = maybeSt (print "no problem specified")

putSt :: St i -> IO ()
putSt st' = do
  st <- readIORef stateRef
  writeIORef stateRef $ st { history_ = st' :+: history_ st }

modifySt :: ProofData o => (ProofTree (Selected i) -> ProofTree (Selected o)) -> IO ()
modifySt f = onSt (putSt . St . f . unSt)


--- * interface ------------------------------------------------------------------------------------------------------

-- | Load a problem.
load :: ProofData i => (FilePath -> IO (Either String i)) -> FilePath -> IO ()
load p  fp = do
  ret <- runErroneousIO $ tryIO (p fp) >>= liftEither . either (Left . TctProblemParseError) Right
  either print (\prob -> initSt prob >> print "Problem loaded." >> printState) ret

-- | Like 'load' but extracts the parser from a configuration.
--
-- > load' = load . parseProblem
load' :: ProofData i => TctConfig i -> FilePath -> IO ()
load' = load . parseProblem

-- | Modifies all sub-problems.
modifyProblems :: ProofData i => (i -> i) -> IO ()
modifyProblems = modifySt . fmap . fmap

-- | Performs an IO action on selected sub-problems.
onProblems :: (i -> IO()) -> IO ()
onProblems f = onSt $ F.mapM_ f . rights . F.toList . unSt

-- | Performs an IO action on the proof tree.
onTree :: (ProofTree i -> IO()) -> IO ()
onTree f = onSt $ f . removeSelection . unSt


-- | Select a list of sub-problems.
select :: [Int] -> IO ()
select is = onSt $ \(St l) -> putSt (St (selectLeafs is l)) >> printState

-- | Select all sub-problems.
selectAll :: IO ()
selectAll = onSt $ \(St l) -> putSt (St (selectAllLeafs l)) >> printState

apply' :: ProofData o => (Strategy i o -> ProofTree (Selected i) -> TctM (ProofTree (Selected o))) -> Strategy i o -> IO ()
apply' eval str = onSt $ \st -> do
  let t1 = unSt st
  t2 <- runInteractive (eval str t1)
  if not (isFailing t2) && norm t2 > norm t1
    then do
      putSt (St t2)
      printState
      putStrLn "progress :)"
      W.when (isClosed t2) $ do
        putStrLn "solved :D"
        printComplexity
    else do
      putStrLn "no progress :/"
      W.when (isFailing t2) $ do
        putStrLn "the reason is:"
        putStrLn (reason t2)
  where
    norm :: ProofTree l -> Int
    norm (Open _)          = 0
    norm (Failure _)       = 0
    norm (Success _ _ pts) = 1 + sum (norm <$> pts)

    reason :: ProofTree t -> String
    reason (Open _)          = mempty
    reason (Failure e)       = show e ++ "\n"
    reason (Success _ _ pts) = concatMap reason (F.toList pts)

-- | Applies a strategy on all sub-problems.
--
-- > apply a >> apply b == apply $ try a .>>> b
apply :: ProofData o => Strategy i o -> IO ()
apply = apply' evaluateAll

-- | Applies a strategy on \selected\ sub-problems. Requires the strategy to have the same input-output type.
applySelected :: ProofData i => Strategy i i -> IO ()
applySelected = apply' evaluateSelected


-- | Undos state. Restores the prooftree before the last application of 'apply' and 'applySelected'.
undo :: IO ()
undo = do
  hst <- history_ `fmap` getState
  unless (null' hst) $ modifyState (\st -> st { history_ = tail' hst })
  printState

-- | Returns the the initial problem.
reset :: IO ()
reset = reset' >> printState where
  reset' = do
    hst <- history_ `fmap` getState
    case hst of
      Nil           -> return ()
      p@(_ :+: Nil) -> modifyState (\st -> st { history_ = p })
      _             -> modifyState (\st -> st { history_ = tail' hst }) >> reset'


ppSt :: PP.Pretty a => (ProofTree (Selected i) -> a) -> IO ()
ppSt pp = onSt $ \(St l) -> PP.putPretty (pp l)

-- | Print the proof.
printProof :: IO ()
printProof = onSt (PP.putPretty . pp)
  where pp (St l) = ppProofTree PP.pretty l

-- | Print state, ie. a list of selected sub-problems.
printState :: IO ()
printState = onSt (PP.putPretty . pp)
  where pp (St l) = ppProofTreeLeafs PP.pretty l

-- | Prints the certificate in termcomp format.
printComplexity :: IO ()
printComplexity = ppSt (termcomp . certificate)


-- | Print proof, state, certificate.
proof, state, complexity :: IO()
proof      = printProof
state      = printState
complexity = printComplexity

-- | Print description of given declaration.
describe :: ArgsInfo args => Declaration (args :-> c) -> IO ()
describe = PP.putPretty

-- | Parses strategy from  the given string.
parse :: ProofData i => [StrategyDeclaration i i] -> String -> Strategy i i
parse ds s = case strategyFromString ds s of
  Left e   -> failing $ show e
  Right st -> st

-- | List descriptions of declarations.
list :: [StrategyDeclaration i o] -> IO ()
list = PP.putPretty . PP.vcat . map PP.pretty

-- | Welcome.
welcome :: IO ()
welcome = putStrLn $ unlines
  ["  .:                                     "
  ,"  XWd.                                   "
  ,"  lMkX0:                            ,d   "
  ,"  .Md lMWd.                      ;kWMl   "
  ,"   OW0Nl.lX;'cdO0KK0Oxl      .:ONx:NK    "
  ,"   ;Md   ;ONkc'.    ..,   .c0XNN' dM.    "
  ,"    N0 :NO,                c.  cNOMd     "
  ,"    ;d0N,                       .WX      "
  ,"     KN.                        lM,      "
  ,"    cM,                         xd       "
  ,"    OW                                   "
  ,"    OW       ..                          "
  ,"    :M:     .xOXN0kdc                    "
  ,"     OW.       kW..':        '           "
  ,"      xWc     .Mo          .KK.          "
  ,"       ,KXc   oN.        ,ONl            "
  ,"         'dX0o:'.....;lONk;              "
  ,"            .,cdxkkkdl;.                 "
  ]

-- | Prints help
help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "load <parser> <filepath>      -- load a problem, initialise state"
  , "load' <tctconfig> <filepath>  -- load a problem, initialise state"
  , "select <leafs>                -- select <leafs>"
  , "selectAll                     -- select all problems"
  , "apply <strategy>              -- apply <strategy> on all problems"
  , "applySelected <strategy>      -- apply <strategy> on selected problems"
  , "undo                          -- undo last application"
  , "reset                         -- reset the history"
  , "state                         -- print state, lists open problems"
  , "proof                         -- print proof"
  , "complexity                    -- print complexity"
  , "describe <declaration>        -- print description of declaration"
  , "parse <declarations> <string> -- parses strategy from <string> from <declarations>"
  , "list <declarations>           -- print description of <declarations>"
  , "modifyProblems <f>            -- applies <f> on all problems"
  , "onProblems <ioaction>         -- applies <ioaction> on all problems"
  , "onTree <ioaction>             -- applies <ioaction> on the current proof tree"
  , "help                          -- print this help"
  , "welcome                       -- print welcome message"
  , ""
  , "WARNING: The interactive mode is not type-safe. In particular, we can apply"
  , "strategies that have a different input problem type than the problem type of"
  , "the current proof state. This can lead to undefined behaviour."
  , ""
  , "INFO: *complexity*, *proof* and *state* provide generic (configuration"
  , "independent) pretty print functions. For special output consider using"
  , "*onProblems* and *onTree*."
  , ""
  , "For detailed information we refer to the documentation."
  ]

