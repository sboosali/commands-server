{-# LANGUAGE FlexibleContexts, LambdaCase, LiberalTypeSynonyms, RankNTypes, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns, ViewPatterns, OverloadedStrings, TupleSections#-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-| (very hacky for now)

-}
module Commands.Plugins.Spiros.Server where
-- import           Commands.Plugins.Spiros.Module
-- import           Commands.Plugins.Spiros.Config
-- -- import           Commands.Plugins.Spiros.Types
-- import           Commands.Plugins.Spiros.Root
-- import           Commands.Plugins.Spiros.Phrase.Types
import Commands.Plugins.Spiros.Server.Setup

import qualified Commands.Servers.Servant as Server
import  Commands.Frontends.Dictation
import Commands.Server.Backend.Types

--import           Commands.Plugins.Spiros.Extra
import           Commands.Plugins.Spiros.Server.Settings
import           Commands.Plugins.Spiros.Server.Types
--import           Commands.Plugins.Spiros.Correct

import System.Clock (TimeSpec,timeSpecAsNanoSecs,diffTimeSpec)
import           Commands.Parsers.Earley (EarleyParser(..), bestParse, eachParse)
import  Workflow.Core         as W
import           Commands.Frontends.Dragon13
import           Commands.Mixins.DNS13OSX9 -- (unsafeDNSGrammar, unsafeEarleyProd)
import           Commands.Servers.Servant
import Commands.Server.Types

import           Control.Lens
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import System.Clock
import           Data.List.NonEmpty              (NonEmpty (..))
-- TODO import Control.Monad.Free.Church (fromF)
import Control.DeepSeq(force)

-- import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad
import System.IO
import System.Mem
import qualified Data.List as List
import System.Exit
import Control.Concurrent.STM
import Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Exception(bracket_)

import Prelude.Spiros
import Prelude()

type CMD c = DNSEarleyCommand c --TODO

--------------------------------------------------------------------------------

parseMain :: (Show a) => CMD c (m()) a -> [String] -> IO ()
parseMain c a (fmap T.pack -> ws) = do
  let p = command2earley c
  print ws

  -- p `eachParse` ws
  (p&pProd) `eachParse` ws & \case
    Left e -> print e
    Right as -> traverse_ print as

--------------------------------------------------------------------------------

spirosServer :: (Show a) => CMD c (m()) a -> IO ()
spirosServer c = do
 (settings, globals, _reference) <- newSpirosSettings c

 _theContextThread <- forkContextWorker globals  -- TODO manage threads
 _theModeThread <- forkModeWorker globals        -- TODO pipes-mvc?

 natlinkIO settings

-- spirosServer = spirosServe rootCommand
-- spirosServer = spirosServe rootPlugin

-- rootPlugin :: VPlugin_ r Root
-- rootPlugin = VPlugin rootCommand

-- de'serve :: (Show a) => (VPlugin_ r a) -> IO ()
 -- Couldn't match type ‘VSettings (E.Rule r a) a’ with ‘forall r1. VSettings_ r1 a0’
-- de'serve :: (Show a) => (forall r. VPlugin_ r a) -> IO ()
 -- Couldn't match type ‘VSettings (E.Rule r0 a) a’ with ‘forall r. VSettings_ r a0’
-- de'serve plugin = de'Settings plugin >>= serveNatlink

-- spirosServe :: (Show a) => (forall r. RULED DNSEarleyCommand r a) -> IO ()
-- -- de'serve plugin = unsafePerformIO(de'Settings plugin) & serveNatlink
-- spirosServe plugin = serveNatlink (spirosSettings plugin)

spirosTest :: (Show a, MonadIO m) => Backend m -> Settings -> CMD c (m()) a -> IO ()
spirosTest Backend{..} Settings{..} c = do
 (settings, globals, _reference) <- newSpirosSettings c

 _theContextThread <- forkContextWorker globals  -- TODO manage threads

 theEnvironment <- getVEnvironment settings
 setupStatus     <- either2bool <$> spirosSetup (NatlinkSetupConfig _bExecuteWorkflow nofindErrorBySearch (_sNatLinkConfig)) theEnvironment

 -- TODO printHeader
 -- let _runInterpret = runEitherT (spirosInterpret spirosMagic rankRoots (newSpirosSettings  globals ootsCommand) (RecognitionRequest ["test"]))   -- NOTE it's a NULLOP.  TODO give name to"test" testInterpret
 -- let _handleInterpret = \case; Left e -> (do print e >> return (Left e)); Right x -> (do print x >> return (Right x))
 -- interpretStatus <- either2bool <$> (_runInterpret >>= _handleInterpret)
 let interpretStatus = True

 let theStatus = (setupStatus && interpretStatus)
 printHeader
 if theStatus then putStrLn "spirosTest: Success" else putStrLn "spirosTest: FAILURE"
 exitWith (bool2exitcode theStatus)


-- TODO remove all these commented out expressions spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
newSpirosSettings
 :: (Default c)
 => (CMD c (m()) a)
 -> IO ((Server.VSettings m c a), (Server.VGlobals c), TVar (Server.VPlugin m c a))
newSpirosSettings spirosCommand = do

 spirosGlobals <- newVGlobals def
 spirosPlugin <- newVPlugin $ spirosUpdate spirosDnsOptimizationSettings spirosCommand

 return$ (makeSpirosSettings spirosGlobals spirosPlugin, spirosGlobals, spirosPlugin)

makeSpirosSettings :: (MonadIO m) => Backend m -> Settings -> _ -> _ -> (Server.VSettings m c a)
makeSpirosSettings b@Backend{..} Settings{..} spirosGlobals spirosPluginReference  = VSettings
 (spirosSetup (NatlinkSetupConfig _bExecuteWorkflow nofindErrorBySearch (_sNatLinkConfig)))
 (VConfig spirosSettings_ spirosBackend spirosGlobals)
 spirosPluginReference

 (spirosInterpret spirosInterpreterSettings)
 (spirosHypotheses )
 (spirosCorrection )
 (spirosReload  )
 (spirosContext  )

spirosInterpreterSettings :: Backend m -> InterpreterSettings m a
spirosInterpreterSettings Backend{..} = InterpreterSettings{..}
 where
 iExecute = (_bExecuteWorkflow)  -- TODO ecuteWorkflow def{W.osxStepDelay = fromIntegral workflowDelay}
 -- iExecute = W.runWorkflowWithDelay 5
 -- iRanking = rankRoots



-- iMagic   = spirosMagic

spirosBackend = VBackend{..}    -- TODO rm
 where
 -- vExecute = fromF >>> W.runWorkflowWithDelay 5
 vExecute = (spirosInterpreterSettings&iExecute )

-- spirosSettings :: (Show a) => RULED DNSEarleyCommand r a -> RULED VSettings r a
-- spirosSettings command = VSettings 8888 pirosSetup (spirosInterpret (\_ _ _ -> return())) (spirosUpdate command)

-- spirosSettings :: forall r a. VPlugin_ r a -> (VSettings_ r a)
-- spirosSettings plugin = (defSettings runWorkflow spirosUpdate plugin)
--   { vSetup = pirosSetup
--   }

-- spirosSettings :: forall r. VPlugin (E.Rule r Root) Root -> IO (VSettings (E.Rule r Root) Root)
-- spirosSettings plugin = do
--  settings :: (VSettings (E.Rule r Root) Root) <- (defSettings runWorkflow spirosUpdate plugin)
--  return$ settings
--   { vSetup = setupCopyGrammar :: (VSettings (E.Rule r Root) Root -> IO (Either VError ()))
--   }


-- spirosUpdate :: VPlugin (E.Rule r Root) Root -> IO (VConfig (E.Rule r Root) Root)
spirosUpdate
 :: DnsOptimizationSettings
 -- -> RULED ((Server.VSettings m c a)) r a
 -> (CMD c (m()) a)
 -> (Server.VPlugin m c a)
spirosUpdate dnsSettings command = VPlugin g p d
 where
 g = (unsafeDNSGrammar dnsSettings (command&_cRHS))
 p = (EarleyParser (unsafeEarleyProd (command&_cRHS)) (command&_cBest))
 d = (command&_cDesugar)
{-# NOINLINE spirosUpdate #-}

--TODO
command2earley :: CMD c b a -> EarleyParser s r String Text a
command2earley Command{..} = EarleyParser (unsafeEarleyProd _cRHS) _cBest

{- | this handler:

* supports short-circuiting (in 'EitherT') on parser error, returning an HTTP error status.
* executes the compiled actions (in 'IO').

-}
spirosInterpret :: forall m c a. InterpreterSettings m a -> _ -> (Server.V Server.DNSResponse m c a)
spirosInterpret InterpreterSettings{..} = \(RecognitionRequest ws) -> do

 VEnvironment{..} :: (Server.VEnvironment m c a) <- ask

 t0<- getTime_

 !(force -> value) <- case bestParse (ePlugin&vParser) ws of
  -- {force} turns WHNF (the bang pattern) into NF
  Right x -> return x
  Left e -> do
   liftIO$ do
    replicateM_ 3 (putStrLn"")
    putStrLn$ "ERROR:"
    print e
    putStrLn$ "WORDS:"
    putStrLn$ showWords ws
    hFlush stdout
   throwError$ VError (show e)

 context <- liftIO$ readSpirosContext <$> iExecute W.currentApplication

 let hParse = either2maybe . (bestParse (ePlugin&vParser))
 let hDesugar = ((ePlugin&vDesugar) context) -- TODO fromF for speed?
 let theHandlers = CommandsHandlers{..}

 let theAmbiguousParser = makeAmbiguousParser (ePlugin&vParser)

 let workflow = hDesugar value  -- TODO church encoding doesn't accelerate construction
 let workflowIO = iExecute workflow -- TODO any executor

 t2<- getTime_

 liftIO$ (atomically$ getMode (eConfig&vGlobals)) >>= \case
   NormalMode -> workflowIO
   CorrectingMode -> workflowIO
   m -> print m
  -- delay in milliseconds
  -- the Objective-C bindings print out which functions are called

 -- magic actions, TODO replace with a free monad
 shouldPrint <- liftIO$ (atomically$ getMode (eConfig&vGlobals)) >>= \case
   NormalMode -> return True --TODO iMagic theHandlers theAmbiguousParser iRanking ws value
   CorrectingMode  -> return False
   _ -> return True

 let d3 = diffTimeSpecAsMilliseconds t2 t0

 when shouldPrint $ liftIO$ do  -- TODO don't print but still log?
     printHeader
     putStrLn$ "RESPONSE:"
     print =<< do atomically$ readTVar (eConfig&vGlobals&vResponse) -- TODO not exactly the same as dnsRespond. should be constant throughout request?
     putStrLn ""
     putStrLn$ "MODE:"
     print =<< do atomically$ getMode (eConfig&vGlobals)
     putStrLn ""
     putStrLn$ "WORKFLOW:"
     putStr  $ "" -- TODO W.showWorkflow workflow -- arrows?
     putStrLn ""
     putStrLn$ "TIMES:"
     putStrLn$ show d3 ++ "ms"
     putStrLn ""
     putStrLn$ "CONTEXT:"
     print =<< do atomically$ getContext (eConfig&vGlobals)
     putStrLn ""
     putStrLn$ "APPLICATION:"
     print context
     putStrLn ""
     putStrLn$ "VALUE:"
     print value
     putStrLn ""
     putStrLn$ "WORDS:"
     putStrLn$ showWords ws

 liftIO$ performMajorGC                -- TODO other thread and delayed ?

 dnsRespond

 where
 getTime_ = liftIO$ getTime Realtime


-- spirosHypotheses
--  :: VSettings_
--  -> (Server.VGlobals c)
--  -> HypothesesRequest
--  -> (Server.V Server.DNSResponse m c a)
-- spirosHypotheses VSettings_{..} vGlobals = \hypotheses -> do
 -- liftIO$ handleHypotheses (vUIAddress) vGlobals hypotheses
spirosHypotheses :: (Server.VHandler m c a) HypothesesRequest
spirosHypotheses = \hypotheses -> do
 globals <- asks (eConfig>>>vGlobals)
 liftIO$ handleHypotheses globals hypotheses
 dnsRespond


-- spirosCorrection
--  :: CorrectionRequest
--  -> (Server.V Server.DNSResponse m c a)
spirosCorrection = \(CorrectionRequest correction) -> do
 liftIO$ handleCorrection' correction
 dnsRespond


-- spirosReload
--  :: ReloadRequest
--  -> (Server.V Server.DNSResponse m c a)
spirosReload = \() -> do
 liftIO$ handleReload
 dnsRespond


-- spirosContext
--  :: ContextRequest
--  -> (Server.V Server.DNSResponse m c a)
spirosContext = \() -> do
 liftIO$ handleContext
 dnsRespond

--
-- -- | on 'Ambiguous', print all parse results.
-- spirosMagic :: ServerMagic SpirosType
-- spirosMagic theHandlers theAmbiguousParser theRanking theWords = \case
--
--   Frozen (List.nub -> List.sort -> stages) _r -> do
--    replicateM_ 2 (putStrLn"")
--
--    case theWords of
--     ((T.unpack -> RootsFrozenPrefix):ws) -> do -- TODO grammatical symbol is hardcoded
--         let theResponse = handleRequest theHandlers ws
--         traverse_ (handleStage theResponse) stages
--         return False
--
--     _ -> return True
--
--   Ambiguous _ -> case theWords of
--    ((T.unpack -> RootsAmbiguousPrefix):ws) -> do -- TODO grammatical symbol is hardcoded
--     liftIO$ handleParses theAmbiguousParser theRanking ws
--     return False
--    _ -> return True
--
--   _ -> return True


{-| the fields in the output are constructed lazily, with intermediary computations shared.

-}
handleRequest :: CommandsHandlers a b -> CommandsRequest -> CommandsResponse a b
handleRequest CommandsHandlers{..} ws = CommandsResponse{..}
 where
 rRaw       = ws
 rParsed    = hParse rRaw
 rDesugared = hDesugar <$> rParsed


printStage :: (Show a, MonadIO m) => CommandsResponse a (m()) -> Stage -> IO()
printStage CommandsResponse{..} = \case
 RawStage   -> liftIO$do
  putStrLn ""
  putStrLn "WORDS:"
  putStrLn$ showWords rRaw
 ParseStage -> liftIO$do
  putStrLn ""
  putStrLn "VALUE:"
  traverse_ print rParsed
 RunStage   -> liftIO$do
  putStrLn ""
  putStrLn "WORKFLOW:"
  putStrLn "" -- TODO traverse_ printWorkflow rDesugared


handleStage :: (Show a, MonadIO m) => CommandsResponse a (m()) -> Stage -> IO ()
handleStage CommandsResponse{..}= \case

         RawStage   -> liftIO$do
             putStrLn ""
             putStrLn "WORDS:"
             printAndPaste (showWords rRaw)

         ParseStage -> liftIO$do
             putStrLn ""
             putStrLn "VALUE:"
             traverse_ (printAndPaste . show) rParsed

         RunStage   -> liftIO$do
             putStrLn ""
             putStrLn "WORKFLOW:"
             putStrLn "" -- TODO traverse_ (rintAndPaste . W.showWorkflow) rDesugared
--
-- handleParses
--  :: (Show a)
--  => AmbiguousParser a -> Ranking a -> [Text] -> IO ()
-- handleParses theParser theRanking ws = do
--  let (value,values) = theParser ws
--  let message = [ ""
--                , ""
--                , ""
--
--                , "LENGTH:"
--                , show (length values)
--                , ""
--
--                , "WORDS:"
--                , showWords ws
--                , ""
--
--                , "BEST:"
--                , show value
--                , ""
--
--                , "VALUES:"
--                ] <> concat (imap showValue values)
--
--  printAndPaste (List.intercalate "\n" message)
--
--  where
--  showValue ((+1) -> index_) value =
--   [ ""
--   , (show index_ <> ".")
--   , ("(" <> show (theRanking value) <> ")")
--   , show value
--   ]


-- handleHypotheses :: Address -> VGlobals c -> HypothesesRequest -> IO ()
-- handleHypotheses _address globals hypotheses@(HypothesesRequest hs) = do
handleHypotheses b@Backend{..} globals hypotheses@(HypothesesRequest hs) = do

 printHeader
 printMessage $ hypothesesMessage

 _ <- forkIO$ do                                -- TODO should be singleton. use some thread manager?
     bracket_ openCorrection closeCorrection useCorrection

 return()

 where

 openCorrection = do
  atomically$ setMode globals CorrectingMode
  (_bExecuteWorkflow&getExecuteWorkflow)$ b^.bCorrectionSettings.reachCorrectionUi

 closeCorrection = do
  atomically$ setMode globals NormalMode
  (_bExecuteWorkflow&getExecuteWorkflow)$ b^.bCorrectionSettings.leaveCorrectionUi

 useCorrection = do
  (b^.bCorrectionSettings.promptCorrection) hypotheses >>= handleCorrection globals -- ignoring control C ? ask

 hypothesesMessage =
  [ "HYPOTHESES:"
  , ""
  ] <> showHypotheses hs

 showHypotheses = \case
  [] -> []
  (theRecognition:theHypotheses) -> concat
   [ fmap (" " <>) (imap showHypothesis theHypotheses)
   , ["(" <> showHypothesis (-1::Int) theRecognition <> ")"]
   ]

 showHypothesis ((+1) -> index_) hypothesis =
  show index_ <> ". " <> T.unpack (T.intercalate " " hypothesis)


handleCorrection' :: [Text] -> IO ()
handleCorrection' _correction = do    -- TODO
 return()


handleCorrection :: VGlobals c -> Dictation -> IO ()
handleCorrection globals theCorrection = do
 let theResponse = (ForeignResultsObject 0 , (\(Dictation ws) -> fmap T.pack ws) theCorrection) -- TODO ForeignResultsObject
 atomically$ writeCorrection globals (CorrectionResponse theResponse)
 printHeader
 printMessage$ correctionMessage

 where

 correctionMessage =
  [ "CORRECTION:"
  ] <> [displayDictation theCorrection]


{-|
an invasive but simple way to signal to the user that the client has been reloaded.
assumes that the "/context" endpoint is only called on module reloading.

-}
handleReload :: (MonadIO m) => Backend m -> IO ()
handleReload b = do    -- TODO
 ((b^.bExecuteWorkflow)&getExecuteWorkflow)$ b^.bReachLoggingUi
 printHeader
 putStrLn "RELOADED"
 return()


{-|
-}
handleContext :: IO ()
handleContext = do    -- TODO
 return()


writeCorrection :: VGlobals c -> CorrectionResponse -> STM ()
writeCorrection VGlobals{..} correction = do
 modifyTVar (vResponse) $ set (responseCorrection) (Just correction)

writeContext :: (Show c) => VGlobals c -> STM ()           -- TODO
writeContext globals@VGlobals{..} = do
 newContext <- show <$> getContext globals
 modifyTVar vResponse $ set (responseContext) (Just newContext)

writeMode :: VGlobals c -> STM ()           -- TODO
writeMode globals@VGlobals{..} = do
 newMode <- getMode globals
 modifyTVar vResponse $ set (responseVMode) (Just newMode)

setMode :: VGlobals c -> VMode -> STM ()
setMode VGlobals{..} mode = do
 modifyTVar (vMode) $ set id mode

getMode :: VGlobals c -> STM VMode
getMode VGlobals{..} = readTVar vMode

setContext :: VGlobals c -> c -> STM ()
setContext VGlobals{..} c = do
 modifyTVar (vContext) $ set id c

getContext :: VGlobals c -> STM c
getContext VGlobals{..} = readTVar vContext

dnsRespond :: (Server.V Server.DNSResponse m c a)
dnsRespond = do
 globals <- asks (eConfig>>>vGlobals)
 liftIO $ atomically $ do
   swapTVar (globals&vResponse) emptyDNSResponse

printHeader :: IO ()
printHeader = do
 putStrLn"--------------------------------------------------------------------------------"
 replicateM_ 3 (putStrLn"")



--------------------------------------------------------------------------------


forkContextWorker :: (Server.VGlobals c) -> IO ThreadId
forkContextWorker globals = forkWorker (loadContextWorker globals)

loadContextWorker :: (Server.VGlobals c) -> Worker
loadContextWorker globals = (milliseconds 10, loadContext globals)

loadContext :: Backend m -> (Server.VGlobals c) -> IO ()
loadContext Backend{..} globals = do
 theApplication <- (_bExecuteWorkflow&getExecuteWorkflow) W.currentApplication
 let theContext = readSpirosContext theApplication
 atomically$ setContext globals theContext
 atomically$ writeContext globals -- hacky


--------------------------------------------------------------------------------

forkModeWorker :: (Server.VGlobals c) -> IO ThreadId
forkModeWorker globals = forkWorker (loadModeWorker globals)

loadModeWorker :: (Server.VGlobals c) -> Worker
loadModeWorker = (milliseconds 10,) . loadMode

loadMode :: (Server.VGlobals c) -> IO ()
loadMode globals = do
 atomically$ writeMode globals -- hacky


--------------------------------------------------------------------------------

newVGlobals :: c -> IO (VGlobals c)
newVGlobals c = atomically$ do
 vResponse <- newTVar emptyDNSResponse
 vMode <- newTVar NormalMode
 vContext <- newTVar c
 vHypotheses <- newTVar Nothing
 return VGlobals{..}

makeAmbiguousParser :: (forall s r. EarleyParser s r e t a) -> [t] -> (Maybe a, [a])
makeAmbiguousParser p theWords = either (const (Nothing, [])) (\(x:|xs) -> (Just ((p&pBest) (x:|xs)), (x:xs))) (eachParse (p&pProd) theWords) -- TODO

-- spirosInterpreter :: (forall s r. EarleyParser s r e t a) -> [t] -> Either e a
-- spirosInterpreter d p ws = do
--  !(force -> value) <- bestParse p ws
--   -- {force} turns WHNF (from the bang pattern) into NF

--  context <- liftIO$ readSpirosContext <$> xecuteWorkflow W.currentApplication

--  let hParse = either2maybe . (bestParse p)
--  let hDesugar = fromF . (d context)
--  let theHandlers = CommandsHandlers{..}

--  let theAmbiguousParser = makeAmbiguousParser p

--  let workflow = hDesugar value  -- TODO church encoding doesn't accelerate construction
--  let workflowIO = xecuteWorkflowWithDelay 5 workflow

--  return workflowIO

--------------------------------------------------------------------------------

type Worker = (Int, IO())          -- TODO

forkWorker :: Worker -> IO ThreadId
forkWorker = forkIO . forever . runWorker

runWorker :: Worker -> IO ()
runWorker (_delay, _action) = _action >> threadDelay _delay

{-| inputs milliseconds, outputs microseconds (which can be given to threadDelay).

>>> milliseconds 10
10000

-}
milliseconds :: Int -> Int
milliseconds = (*1000)

printMessage :: [String] -> IO ()
printMessage = putStrLn . List.intercalate "\n"

showWords :: [Text] -> String
showWords = T.unpack . T.intercalate (T.pack " ")
printAndPaste :: String -> IO ()

printAndPaste s = do
--  insertByClipboardIO s
  putStrLn s

readSpirosContext :: String -> String --TODO
readSpirosContext = \case
 (isEmacsApp -> Just{}) -> "EmacsContext"
 "Google Chrome" -> "ChromeContext"
 "IntelliJ" -> "IntelliJContext"
 _ -> "GlobalContext"

isEmacsApp :: FilePath -> Maybe FilePath --TODO
isEmacsApp fp = if fp `elem` ["Emacs"]
 then Just fp
 else Nothing

-- isEmacsApp :: FilePath -> Maybe FilePath --TODO
-- isEmacsApp fp = if  `elem` ["Emacs","Work","Notes","Diary","Obs","Commands"]
--  then Just fp
--  else Nothing

bool2exitcode :: Bool -> ExitCode
bool2exitcode False = ExitFailure 1
bool2exitcode True  = ExitSuccess

diffTimeSpecAsMilliseconds :: TimeSpec -> TimeSpec -> Integer
diffTimeSpecAsMilliseconds x y = (timeSpecAsNanoSecs (diffTimeSpec x y)) `div` (1000*1000)

--------------------------------------------------------------------------------
