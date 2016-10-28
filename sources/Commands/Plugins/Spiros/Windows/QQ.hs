{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}
-- | (you can read the source for documentation: just think of this module as a config file)
module Commands.Plugins.Spiros.Windows.QQ where
import           Commands.Plugins.Spiros.Windows.Types

import           Text.InterpolatedString.Perl6
import qualified Data.Text.Lazy as T

import Prelude.Spiros
import Prelude()

getBatchScript :: BatchScriptR Text -> Text
getBatchScript BatchScriptR{..} = [qc|
:: {__batchFilePath__}

@echo off
setlocal enableextensions enabledelayedexpansion

wmic process where name="natspeak.exe" CALL setpriority "high priority"

taskkill /IM slui.exe /F
:: taskkill /IM egui.exe /F

taskkill /IM ISUSPM.exe /F
taskkill /IM dnsspserver.exe /F
:: taskkill /IM dgnuiasvr_x64.exe /F

echo {__guestDirectory__}
echo {__natlinkDirectory__}
echo {__natlinkFile__}

set COMMANDSTIMESTAMP=0
set DIFFERENCE=0

:: synchronize every second, updating the timestamp, and skating if the source file hasn't changed
FOR /L %%N IN (0,0,1) DO (
 echo(
 echo(
 echo(
 FOR /f "tokens=2 delims==" %%t IN (
    'WMIC DATAFILE WHERE Name^="{__guestEscaped__}" GET LastModified /value ^| find "="'
 ) DO set TIMESTAMP=%%t
      set TIMESTAMP=!TIMESTAMP:~4,10!
      set /a DIFFERENCE=!TIMESTAMP! - !COMMANDSTIMESTAMP!
      echo(
      echo CURR is !COMMANDSTIMESTAMP!
      echo PREV is !TIMESTAMP!
      echo DIFF is !DIFFERENCE!
      IF not !DIFFERENCE! equ 0 (
           echo "updating..."
           set COMMANDSTIMESTAMP=!TIMESTAMP!
           copy /Y {__guestDirectory__}\\{__natlinkFile__} {__natlinkDirectory__} /a
           copy /b {__natlinkDirectory__}\\{__natlinkFile__}+,, {__natlinkDirectory__}\\{__natlinkFile__}
      ) ELSE (
           echo "not updating."
      )
  timeout /nobreak /t 10
)

|]
 -- beware: in batch, !x! dereferences x at runtime,
 -- while %x% **interpolates** x at parsetime (of the block).
 -- and %%t only seems to exist within the "outermost block" of the for loop.
 -- that's why we need: set TIMESTAMP=%%t
 -- arbitrary arithmetic doesn't work: set TIMESTAMP=!TIMESTAMP:~0,14! ; set /a DIFFERENCE=(!TIMESTAMP! - !COMMANDSTIMESTAMP!)
 -- because numbers must be 32bit.
 where
 __guestEscaped__ = T.replace "\\" "\\\\" (__guestDirectory__ <> "\\" <> __natlinkFile__)
  -- because the path in batch is under double quotes


getUpdateScript :: UpdaterR Text -> Text
getUpdateScript UpdaterR{..} = [qc|
import os
import time
import shutil

PREVIOUS_TIMESTAMP = 0
SOURCE_FILE = "E:/_commands.py"
DESTINATION_FILE = "C:/NatLink/NatLink/MacroSystem/_commands.py"

def test_file():
    global PREVIOUS_TIMESTAMP
    print "PREVIOUS_TIMESTAMP =", PREVIOUS_TIMESTAMP
    current_timestamp = int(os.lstat(SOURCE_FILE).st_mtime)
    if current_timestamp > PREVIOUS_TIMESTAMP:
        copy_file()
    PREVIOUS_TIMESTAMP = current_timestamp

def copy_file():
    print "UPDATING"
    shutil.copyfile(SOURCE_FILE, DESTINATION_FILE)
    # os.rename(SOURCE_FILE, DESTINATION_FILE)

def loop():
    while True:
        try:
             test_file()
        except OSError as e :
            print e
        time.sleep(1)

def main():
    print "SOURCE_FILE =", SOURCE_FILE
    print "DESTINATION_FILE =", DESTINATION_FILE

    loop()

main()

|]
