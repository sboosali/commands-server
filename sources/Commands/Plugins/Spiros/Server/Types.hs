{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, RankNTypes  #-}
module Commands.Plugins.Spiros.Server.Types where
-- import           Commands.Plugins.Spiros.Module (SpirosType, SpirosMonad, SpirosMonad_)
--import           Commands.Plugins.Spiros.Extra

-- import qualified Pipes.Core as P

import Prelude.Spiros
import Prelude()

--type SpirosInterpreterSettings = InterpreterSettings spirosMonad SpirosType -- TODO

type Ranking a = a -> Int

data InterpreterSettings m a = InterpreterSettings
 { iExecute :: m :~> IO
-- , iRanking :: Ranking a
-- , iMagic   :: ServerMagic a
 }

-- | the stages of the DSL
data Stage = RawStage | ParseStage | RunStage deriving (Show,Read,Eq,Ord,Bounded,Enum,Generic,Data)

-- type ServerMagic a m_ = CommandsHandlers a m_ -> AmbiguousParser a -> Ranking a -> [Text] -> a -> IO Bool -- TODO
--
type AmbiguousParser a = [Text] -> (Maybe a, [a])

type CommandsRequest = [Text]

data CommandsResponse a b = CommandsResponse
 { rRaw       :: [Text]
 , rParsed    :: Maybe a
 , rDesugared :: Maybe b
 }

data CommandsHandlers a b = CommandsHandlers
 { hParse   :: [Text] -> Maybe a
 , hDesugar :: a -> b
 }

data Mode
 = ModeNormal
 | ModeCorrecting
 | ModeDictating
 | ModeSleeping
 | ModeOff
 | ModeReading
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data)
instance NFData Mode

{-|

A "resource", with bidirectional communication.

Naming:

* @h@ for "handle"
* @m@ for "Monad"

The server can be defined against the generic Front-end:

@
:: (MonadIO m) => Frontend m -> IO ()
Frontend{..} = do
  h <- fCreate
  rs <- fRecognitions h
  rs >-> P.take 2 >-> P.stdout
  fDestroy h
@

TODO Or against a specific one:

@
Frontend (ReaderT SpeechRecognitionEngine IO)
@

-}
-- data Frontend m = forall h. Frontend
--  { fCreate       ::      m h
--  , fRecognitions :: h -> P.Server (Request a) (Response b) m ()
--  , fDestroy      :: h -> m ()
--  }
--
-- data Input a = Input a Recognition
--
-- newtype Recognition = Recognition [String] -- deriving

{-|

data Frontend m a b = forall h. Frontend
 { fCreate       ::      m h
 , fRecognitions :: h -> P.Server (Input a) (Output b) m ()
 , fDestroy      :: h -> m ()
 }

newtype Recognition = Recognition [String] -- deriving

data Frontend m = forall h. Frontend
 { fCreate       ::      m h
 , fRecognitions :: h -> P.Producer Recognition m ()
 , fDestroy      :: h -> m ()
 }

data Frontend = forall frontend. Frontend
 { fCreate       ::             IO frontend
 , fRecognitions :: frontend -> P.Producer Recognition IO ()
 , fDestroy      :: frontend -> IO ()
 }

type Server b' b = Proxy X () b' b
Server b' b receives requests of type b' and sends responses of type b.
Servers only respond and never request.

respond :: response -> (Server request response) request

do
 aRequest <- respond aResponse

i.e. client-driven

--1st
respond def
respond mempty


-}
