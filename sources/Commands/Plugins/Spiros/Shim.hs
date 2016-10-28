module Commands.Plugins.Spiros.Shim
 ( module Commands.Plugins.Spiros.Shim
 , module Commands.Plugins.Spiros.Shim.QQ
 -- , module Commands.Plugins.Spiros.Shim.API 
 ) where 
import Commands.Plugins.Spiros.Shim.QQ
-- import Commands.Plugins.Spiros.Shim.API 

import qualified Data.Text.Lazy                as T
import Data.Text.Lazy (Text) 

import Data.Char (isAscii) 

cleanShim :: Text -> Text 
cleanShim = T.filter isAscii

