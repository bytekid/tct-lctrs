-- | This module provides the main function and the command-line interface.
module Tct.Core.Main
  (
  -- * Tct Configuration
  TctConfig (..)
  , defaultTctConfig
  , InteractiveGHCi (..)
  -- * Tct Initialisation
  , run
  , runInteractive
  , runTct
  , runTctWithOptions
  -- * Pretty Print
  , AnswerFormat (..)
  , ProofFormat (..)
  ) where


import           Control.Applicative     ((<|>))
import           Control.Monad.Reader    (runReaderT)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import qualified Options.Applicative     as O
import           System.Exit             (exitFailure, exitSuccess)
import           System.IO               (hClose, hPutStrLn, stderr)
import           System.IO.Temp          (withSystemTempFile, withTempDirectory)
import           System.Process          (system)
import qualified System.Time             as Time
import System.Directory (getTemporaryDirectory)

import           Tct.Core.Common.Error
import           Tct.Core.Common.Options
import qualified Tct.Core.Common.Pretty  as PP
import           Tct.Core.Data
import           Tct.Core.Parse          (strategyFromString)


synopsis :: String
synopsis = "TcT is a transformer framework for automated complexity analysis."


-- TctConfig ---------------------------------------------------------------------------------------------------------

-- | The Tct configuration defines global properties.
-- The configuration affects the execution of ('runTct') and sets initial properties ('run') when eva
data TctConfig i = TctConfig
  { parseProblem    :: FilePath -> IO (Either String i)
  , putAnswer       :: Either AnswerFormat (ProofTree i -> IO ())
  , putProof        :: Either ProofFormat (ProofTree i -> IO ())

  , defaultStrategy :: Strategy i i

  , runtimeOptions  :: [(String, [String])]
  , interactiveGHCi :: InteractiveGHCi
  , version         :: String
  }

-- | Specifies how interactive mode is executed. See 'startInteractive'.
data InteractiveGHCi = GHCiScript (Maybe (FilePath -> String)) [String] | GHCiCommand String

-- | Default configuration. Minimal requirement 'parseProblem'.
defaultTctConfig :: ProofData i => (FilePath -> IO (Either String i)) -> TctConfig i
defaultTctConfig p = TctConfig
  { parseProblem    = p
  , putAnswer       = Left DefaultAnswerFormat
  , putProof        = Left DefaultProofFormat
  , defaultStrategy = abort
  , runtimeOptions  = []
  , interactiveGHCi = GHCiScript Nothing
      [ ":set prompt \"tct>\""
      , ":module +Tct.Core.Interactive" ]
  , version         = "3.2.0"
  }


prettySilentAnswer, prettyDefaultAnswer, prettyTTTacAnswer :: ProofTree i -> PP.Doc
prettySilentAnswer  _ = PP.empty
prettyDefaultAnswer r = PP.pretty (termcomp (certificate r))
prettyTTTacAnswer   r = PP.pretty (tttac (certificate r))

prettySilentProof, prettyDefaultProof :: PP.Pretty i => ProofTree i -> PP.Doc
prettySilentProof _  = PP.empty
prettyDefaultProof r = PP.pretty (ppProofTree PP.pretty r)
-- prettyXmlProof r             = error "missing: toXml proofTree" -- TODO

putAnswerFormat :: AnswerFormat -> ProofTree i -> IO ()
putAnswerFormat SilentAnswerFormat  = PP.putPretty . prettySilentAnswer
putAnswerFormat DefaultAnswerFormat = PP.putPretty . prettyDefaultAnswer
putAnswerFormat TTTACAnswerFormat   = PP.putPretty . prettyTTTacAnswer

putProofFormat :: PP.Pretty i => ProofFormat -> ProofTree i -> IO ()
putProofFormat SilentProofFormat  = PP.putPretty . prettySilentProof
putProofFormat DefaultProofFormat = PP.putPretty . prettyDefaultProof
-- putXmlFormat XmlProofFormat = PP.putPretty prettyXmlProof

-- | Format of answer output.
data AnswerFormat = SilentAnswerFormat | DefaultAnswerFormat | TTTACAnswerFormat

-- | Format of proof output. Printed after answer in main.
data ProofFormat = SilentProofFormat | DefaultProofFormat
  -- | XmlProofFormat

writeAnswerFormat :: AnswerFormat -> String
writeAnswerFormat SilentAnswerFormat  = "s"
writeAnswerFormat DefaultAnswerFormat = "d"
writeAnswerFormat TTTACAnswerFormat   = "t"

readAnswerFormat :: Monad m => String -> m AnswerFormat
readAnswerFormat s
  | s == writeAnswerFormat SilentAnswerFormat  = return SilentAnswerFormat
  | s == writeAnswerFormat DefaultAnswerFormat = return DefaultAnswerFormat
  | s == writeAnswerFormat TTTACAnswerFormat   = return TTTACAnswerFormat
  | otherwise = fail $ "Tct.readOutputMode: " ++ s

writeProofFormat :: ProofFormat -> String
writeProofFormat SilentProofFormat  = "s"
writeProofFormat DefaultProofFormat = "d"
-- writeProofFormat XmlProofFormat     = "x"

readProofFormat :: Monad m => String -> m ProofFormat
readProofFormat s
  | s == writeProofFormat SilentProofFormat  = return SilentProofFormat
  | s == writeProofFormat DefaultProofFormat = return DefaultProofFormat
  -- | s == writeProofFormat XmlProofFormat     = return XmlProofFormat
  | otherwise = fail $ "Tct.readOutputMode: " ++ s


--- * Command-Line Options -------------------------------------------------------------------------------------------

-- | Tct command line options.
data TctOptions m = TctOptions
  { answerFormat_ :: Maybe AnswerFormat
  , proofFormat_  :: Maybe ProofFormat
  , timeout_      :: Maybe Int
  , modeOptions_  :: m
  , strategyName_ :: Maybe String
  , problemFile_  :: FilePath }


data TctAction m
  = Run (TctOptions m)
  | RunInteractive

mkParser :: [StrategyDeclaration i o] -> String -> O.Parser m -> O.ParserInfo (TctAction m)
mkParser ps vers mparser = O.info (versioned <*> listed <*> O.helper <*> interactive <|> tctp) desc
  where
    listed = O.infoOption (PP.display . PP.vcat $ map PP.pretty ps) $ mconcat
      [ O.long "list"
      , O.help "Display list of strategies."]
    versioned = O.infoOption vers  $ mconcat
      [ O.long "version"
      , O.short 'v'
      , O.help "Display Version."
      , O.hidden]
    interactive = O.flag' RunInteractive $ mconcat
      [ O.long "interactive"
      , O.short 'i'
      , O.help "Interactive mode (experimental)." ]
    tctp = fmap Run $ TctOptions
      <$> O.optional (O.option (O.str >>= readAnswerFormat) (mconcat
        [ O.short 'a'
        , O.long "answer"
        , O.helpDoc . Just $ PP.vcat
          [ PP.hsep [PP.text (writeAnswerFormat SilentAnswerFormat)  , PP.text "- silent"]
          , PP.hsep [PP.text (writeAnswerFormat DefaultAnswerFormat) , PP.text "- default answer (termcomp 2015)"]
          , PP.hsep [PP.text (writeAnswerFormat TTTACAnswerFormat )  , PP.text "- competition answer"] ]]))
      <*> O.optional (O.option (O.str >>= readProofFormat) (mconcat
        [ O.short 'p'
        , O.long "proof"
        , O.helpDoc . Just $ PP.vcat
          [ PP.hsep [PP.text (writeProofFormat SilentProofFormat) , PP.text "- silent"]
          , PP.hsep [PP.text (writeProofFormat DefaultProofFormat), PP.text "- default proof"] ]]))
          -- , PP.hsep [PP.text (writeProofFormat XmlProofFormat)    , PP.text "- xml proof"] ]]))
      <*> O.optional (O.option O.auto (mconcat
        [ O.short 't'
        , O.long "timeout"
        , O.metavar "Sec"
        , O.help "Sets timeout in seconds."]))
      <*> mparser
      <*> O.optional (O.strOption (mconcat
        [O.long "strategy"
        , O.short 's'
        , O.help "The strategy to apply."]))
      <*> O.argument O.str (O.metavar "File")
    desc = mconcat
      [ O.headerDoc   . Just $ PP.string "TcT -- Tyrolean Complexity Tool"
      , O.progDescDoc . Just $ PP.string synopsis ]


--- * Main -----------------------------------------------------------------------------------------------------------

run :: TctConfig i -> TctM a -> IO a
run cfg m = run' m $ \st -> st { kvPairs = M.fromList (runtimeOptions cfg) }

-- MS: the interactive mode is not type-safe so we ignore all problem specfic options
runInteractive :: TctM a -> IO a
runInteractive m = run' m id

run' :: TctM a -> (TctROState -> TctROState) -> IO a
run' m k = do
  time <- Time.getClockTime
  tdir <- getTemporaryDirectory
  let
    state tmp = TctROState
      { startTime     = time
      , stopTime      = Nothing
      , tempDirectory = tmp
      , kvPairs       = M.empty }
  withTempDirectory tdir "tctx" (runReaderT (runTctM m) . k . state)

startInteractive :: InteractiveGHCi -> IO ()
startInteractive ig = void $ case ig of
  GHCiCommand cmd     -> system cmd
  GHCiScript mcmd scr -> withSystemTempFile "ghcix" $ \fp hf -> do
    hPutStrLn hf (unlines scr) >> hClose hf
    system $ fromMaybe ("ghci -ingore-dot-ghci -ghci-script " ++) mcmd fp

-- > runTct = runTctWith const unit
runTct :: (ProofData i, Declared i i) => TctConfig i -> IO ()
runTct = runTctWithOptions const unit

-- | Default main function.
--
-- @runTctWithOptions update options config@
--
-- 1. builds a strategy parser from the strategies defined in @config@
-- 2. parses the command line arguments including arguments defined in @options@
-- 3. updates @config@ using the @update@ and the parsed problem options from @option@
-- 4. updates @config@ from standard arguments; in particular answer output and proof output overrides @config@ if given
-- 5. evaluates strategy with the given timeout
-- 6. output answer and proof as given in (updated) @config@
runTctWithOptions :: (ProofData i, Declared i i) => (TctConfig i -> opt -> TctConfig i) -> Options opt -> TctConfig i -> IO ()
runTctWithOptions = runTctWithOptions' decls

runTctWithOptions' :: ProofData i => [StrategyDeclaration i i] -> (TctConfig i -> opt -> TctConfig i) -> Options opt -> TctConfig i -> IO ()
runTctWithOptions' theStrategies theUpdate theOptions cfg = do
  r <- runErroneousIO $ do
    action <- liftIO $ O.execParser $ mkParser theStrategies (version cfg) theOptions
    case action of
      RunInteractive -> tryIO $ startInteractive (interactiveGHCi cfg)
      Run opts       -> do
        let
          TctOptions
            { strategyName_ = theStrategyName
            , timeout_      = theTimeout
            , problemFile_  = theProblemFile
            } = opts
          ucfg = updateTctConfig theUpdate opts cfg
        let
          TctConfig
            { parseProblem    = theParser
            , putAnswer       = theAnswer
            , putProof        = theProof
            , defaultStrategy = theDefaultStrategy
            } = ucfg

        prob <- do
          f <- tryIO $ theParser theProblemFile
          liftEither $ either (Left . TctProblemParseError) Right f

        st <- maybe (return theDefaultStrategy) (liftEither . parseStrategy) theStrategyName

        let stt = maybe st (`timeoutIn` st) theTimeout
        r <- liftIO $ run ucfg (evaluate stt (Open prob))

        liftIO $ ppAnswer theAnswer r
        liftIO $ ppProof theProof theAnswer r
  case r of
    Left err -> PP.putPretty (PP.text "ERROR") >> PP.hPutDoc stderr (PP.pretty err PP.</> PP.text "") >> exitFailure
    Right _  -> exitSuccess
  where
    updateTctConfig f opt cf = cfg'
      { putAnswer = maybe (putAnswer cfg') Left (answerFormat_ opt)
      , putProof  = maybe (putProof cfg')  Left (proofFormat_ opt) }
      where cfg' = f cf (modeOptions_ opt)

    parseStrategy s = case strategyFromString theStrategies s of
      Left err -> Left $ TctStrategyParseError err
      Right st -> Right st

    ppAnswer (Left f)                     = putAnswerFormat f
    ppAnswer (Right f)                    = f
    ppProof (Left f@DefaultProofFormat) a = \pt -> putProofFormat f pt >> ppAnswer a pt
    ppProof (Left f) _                    = putProofFormat f
    ppProof (Right f) _                   = f

