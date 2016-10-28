{-# LANGUAGE FlexibleContexts, LambdaCase, RankNTypes, RecordWildCards, OverloadedStrings, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, LambdaCase, RankNTypes, TypeSynonymInstances, FlexibleInstances #-}

{-|

-}
module Commands.Plugins.Spiros.Server.Setup where
import           Commands.Plugins.Spiros.Shim
import           Commands.Plugins.Spiros.Windows
import           Commands.Plugins.Spiros.Server.QQ

--import Commands.Server.Backend.Types as Server
import           Commands.Frontends.Dragon13
import Workflow.Core
import Data.Address

import Control.Lens
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Language.Python.Common.Token
import Language.Python.Common.SrcLocation
import Language.Python.Common.ParseError

import Text.Printf (printf)
import System.IO

import Prelude.Spiros
import Prelude()

{-|

@
findErrorBySearch countWidth marginWidth errorRow errorColumn
@

-}
data NatlinkSetupConfig = NatlinkSetupConfig
 { executeWorkflow   :: ExecuteWorkflow
 , findErrorBySearch :: (forall m. (MonadWorkflow m) => Text -> Int -> Int -> Int -> Int -> m ())
 , dnsSerializedGrammar :: SerializedGrammar
 , natlinkConfig :: NatLinkConfig
 }

--------------------------------------------------------------------------------

{-|

-}
spirosSetup
 :: NatlinkSetupConfig
 -> IO (Either String ())
-- spirosSetup VEnvironment{ePlugin{..},eConfig{..}} = do
spirosSetup NatlinkSetupConfig{..} = do
 let theConfig = natlinkConfig
 let theGrammar = dnsSerializedGrammar
 let theAddress = theConfig&nlAddress

 do
   putStrLn ""
   T.putStrLn$ displayAddress theAddress
   T.putStrLn$ curl_ExampleRequest theAddress

 do
   let myBatchScript = getBatchScript myBatchScriptR

   putStrLn ""
   T.putStrLn$ myBatchScript
   (executeWorkflow&getExecuteWorkflow) $ setClipboard $ T.unpack myBatchScript

   putStrLn ""
   putStrLn$ getBatchScriptPath myBatchScriptR

   (executeWorkflow&getExecuteWorkflow) $ setClipboard $ T.unpack (myBatchScriptR&__batchFilePath__)

 let theRawFile = applyShim_ getShim theConfig theGrammar  -- TODO is this the right place?
 let theFileOrError = fmap (over _PythonFile cleanShim) $ newPythonFile theRawFile

 case theFileOrError of

  Left (PythonSyntaxError e s) -> do
   let (errorRow, errorColumn) = getPythonErrorSpan e
   let (marginWidth, countWidth, code) = leftAppendLineNumbers s

   putStrLn ""
   print e
   putStrLn ""
   T.putStrLn$ code
   putStrLn ""
   (executeWorkflow&getExecuteWorkflow) $ findErrorBySearch theRawFile countWidth marginWidth errorRow errorColumn
   putStrLn "SHIM PARSING FAILURE" -- TODO logging
   return$ Left ""

  Right myShim -> do

   -- putStrLn$ T.unpack shim  -- too long (5k lines)
   putStrLn ""

   -- because:
   -- 1. when pasting into an editor in virtual box, the clipboard contents are often trailed by Unicode garbage
   -- 2. which is why the shim ends in a comment
   -- 3. but Unicode characters can have nonlocal effects on other characters, like on previous lines
   copyShim executeWorkflow myShim
   writeShim myBatchScriptR myShim -- TODO its own function

   putStrLn "SHIM PARSING SUCCESS" -- TODO logging

   return$ Right()

--------------------------------------------------------------------------------

copyShim :: ExecuteWorkflow -> PythonFile -> IO ()
copyShim (ExecuteWorkflow exec) s = exec $ do
  setClipboard (s&getPythonFile&T.unpack)

writeShim :: BatchScriptR Text -> PythonFile -> IO ()
writeShim aBatchScript aPythonFile = writeFile (aBatchScript&getBatchScriptPath) (aPythonFile&getPythonFile&T.unpack)

padNumber :: Integral a => Int -> a -> String
padNumber padding n = printf ("%0." ++ show padding ++ "d") (toInteger n)

leftAppendLineNumbers :: Text -> (Int,Int,Text)
leftAppendLineNumbers code = (marginWidth, countWidth, (T.unlines . imap go) allLines)
 where
 --go :: 
 go ((+1) -> lineNumber) oneLine = getLeftMargin lineNumber <> oneLine
 marginWidth = (fromInteger . toInteger . T.length) (getLeftMargin (0::Int))  -- assumes the length is constant
 getLeftMargin lineNumber = "[" <> T.pack (padNumber countWidth lineNumber) <> "]"
 countWidth = length (show lineCount)
 lineCount = length allLines
 allLines = T.lines code

getPythonErrorSpan :: ParseError -> (Int,Int)
getPythonErrorSpan = maybe (1,1) id . go -- TODO default error span
 where

 go = \case
  UnexpectedToken (token_span -> theSpan) -> fromSourceSpan theSpan
  UnexpectedChar _ location -> fromSourceLocation location
  _ -> Nothing

 fromSourceSpan = \case
  SpanCoLinear{ span_row, span_start_column } -> Just (span_row, span_start_column)
  SpanMultiLine{ span_start_row, span_start_column } -> Just (span_start_row, span_start_column)
  SpanPoint{ span_row, span_column } -> Just (span_row, span_column)
  _ -> Nothing

 fromSourceLocation = \case
  Sloc{ sloc_row, sloc_column } -> Just (sloc_row, sloc_column)
  _ -> Nothing

--------------------------------------------------------------------------------

nofindErrorBySearch :: (MonadWorkflow m) => Text -> Int -> Int -> Int -> Int -> m ()
nofindErrorBySearch rawPythonFile countWidth marginWidth errorRow errorColumn = return () --TODO

--------------------------------------------------------------------------------
