{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
module Commands.Plugins.Spiros.Server.Settings where
import Commands.Servers.Servant.V
import           Data.Address
import qualified           Commands.Frontends.Dragon13 as Dragon
import Commands.Server.Backend.Types

import           Control.Lens


spirosSettings_ = VSettings_{..}
 where
 vPort = defaultPort     -- spirosPort = spirosSettings_ ^. (to(vPort) . _Port)
 vUIAddress = defaultUIAddress
 vNatLinkSettings = Dragon.NatLinkConfig
   (Address default_VirtualBox_HostOnlyNetwork_Host vPort)
   "E:/commands/log.txt"
   "E:/commands/context.json"

spirosDnsOptimizationSettings = Dragon.defaultDnsOptimizationSettings
 & Dragon.dnsOptimizeInlineSmall .~ True
 -- & .~
