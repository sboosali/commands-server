{-# LANGUAGE DeriveFunctor #-}
-- | (you can read the source for documentation: just think of this module as a config file)
module Commands.Plugins.Spiros.Windows.Types where
  
import Prelude.Spiros
import Prelude()


{-|

-}
data BatchScriptR t = BatchScriptR
 { __hostDirectory__    :: t
 , __guestDirectory__   :: t
 , __natlinkDirectory__ :: t
 , __natlinkFile__      :: t
 , __batchFilePath__    :: t
 -- , :: t
 } deriving (Show,Eq,Ord,Functor,Data,Generic)

data UpdaterR t = UpdaterR
 { __updaterFilePath__  :: t
 -- , __hostDirectory__    :: t
 -- , __guestDirectory__   :: t
 -- , __natlinkDirectory__ :: t
 -- , __natlinkFile__      :: t
 -- , :: t
 } deriving (Show,Eq,Ord,Functor,Data,Generic)
