{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Commands.Plugins.Spiros.Windows
 ( module Commands.Plugins.Spiros.Windows
 , module Commands.Plugins.Spiros.Windows.Types 
 , module Commands.Plugins.Spiros.Windows.QQ
 ) where

import Commands.Plugins.Spiros.Windows.Types
import Commands.Plugins.Spiros.Windows.QQ

import qualified Data.Text.Lazy                as T
import Data.Text.Lazy (Text) 

import System.FilePath ((</>))


writeBatchScript :: BatchScriptR Text -> IO ()
writeBatchScript s = writeFile (getBatchScriptPath s) (T.unpack (getBatchScript s))

getBatchScriptPath :: BatchScriptR Text -> FilePath 
-- getBatchScriptPath :: (CanInterpolate t) => BatchScriptR t -> t 
getBatchScriptPath BatchScriptR{..} = (T.unpack __hostDirectory__ </> T.unpack __natlinkFile__)


myBatchScriptR :: BatchScriptR Text 
myBatchScriptR = BatchScriptR{..} -- TODO separate library from configuration {-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

 where 
 __hostDirectory__    = "/Users/sboosalis/Desktop" 
 __guestDirectory__   = "E:" 
 __natlinkDirectory__ = "C:\\NatLink\\NatLink\\MacroSystem" 
 __natlinkFile__      = "_commands.py" 
 __batchFilePath__    = "C:\\Users\\sboosalis\\Desktop\\update.bat" 
