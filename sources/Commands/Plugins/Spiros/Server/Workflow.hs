{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Commands.Plugins.Spiros.Server.Workflow where
-- import           Commands.Plugins.Spiros.Extra
-- import           Commands.Plugins.Spiros.Edit (Move(..), Direction(..), Region(..), moveEmacs)
-- import           Commands.Plugins.Spiros.Phrase (word2phrase)
-- import           Commands.Plugins.Spiros.Macros (reverse_search_regexp)
-- import Commands.Plugins.Spiros.Windows (BatchScriptR,getBatchScriptPath)
--
-- import qualified Commands.Backends.Workflow         as W
-- -- import Commands.Backends.Workflow
-- import Commands.Frontends.Dragon13.Shim.Types (PythonFile(..))
--
-- import qualified Data.Text.Lazy                as T
-- import Data.Text.Lazy (Text)


{-

{-| find a padded line number (to reach the one-based error column),
then move right (to the one-based error row).

-}
-- findErrorBySearch :: (W.MonadWorkflow m) => Int -> Int -> Int -> Int -> m ()
findErrorBySearch countWidth marginWidth errorRow errorColumn = do --TODO
 reverse_search_regexp (Just (word2phrase (padNumber countWidth errorRow)))
 moveEmacs (Move Left_ Line)
 traverse_ moveEmacs (replicate (marginWidth + errorColumn - 2) (Move Right_ Character))

 --TODO On Windows, write the file, then open in Notepad, and jump to the line (Word Wrap must be disabled)

findErrorBySearch :: (MonadWorkflow m) => PythonFile -> Int -> Int -> Int -> Int -> m ()
findErrorBySearch (PythonFile s) countWidth marginWidth errorRow errorColumn = do


reachCorrectionUi = do          -- TODO configurable. and maybe related to version control and relaunching.
 W.openApplication "Terminal"

unreachCorrectionUi = do
 openPreviousApplication

  -- TODO configurable. and maybe related to version control and relaunching.
reachLoggingUi = do
 W.openApplication "Terminal"

openPreviousApplication = do
 W.press "M-<tab>"
 W.press "<ret>"

-}

-- setClipboardIO :: String -> IO ()
-- setClipboardIO = runWorkflow' . W.setClipboard

-- printWorkflow :: W.Workflow_ -> IO ()
-- printWorkflow = putStrLn . W.showWorkflow
--
-- insertByClipboardIO :: String -> IO ()
-- insertByClipboardIO s = runWorkflow' $ do
--  insertByClipboard ("\n" <> s <> "\n")
--  W.delay 250
--
-- printAndPaste :: String -> IO ()
-- printAndPaste s = do
--  insertByClipboardIO s
--  putStrLn s
--
-- copyShim :: PythonFile -> IO ()
-- copyShim s = setClipboardIO (s&getPythonFile&T.unpack)
--
-- writeShim :: BatchScriptR Text -> PythonFile -> IO ()
-- writeShim aBatchScript aPythonFile = writeFile (aBatchScript&getBatchScriptPath) (aPythonFile&getPythonFile&T.unpack)
--
