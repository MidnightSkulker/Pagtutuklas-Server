-- | Configuration information relevant to the Resolver function
module Config.ResolverConfig
       ( ResolverConfig (..) 
       , HasResolverConfig (..)
       , processResolverOptions
       , parseResolverConfig
       , resolverConfigOptions
       ) where

import Control.Monad.Trans ( MonadIO )
import Control.Monad.Error ( MonadError )
import System.Console.GetOpt ( OptDescr (..) )
import Config.ConfigClass ( Config (..) )
import Config.Options ( Option )

-- | Configuration information relevant to the baseUrl function
--   So far none
data ResolverConfig = ResolverConfig deriving ( Show )

-- | Class characterizing having Resolver config
class HasResolverConfig c where
  getResolverConfig :: c -> ResolverConfig

instance HasResolverConfig ResolverConfig where
  getResolverConfig = id

instance Config ResolverConfig where
  parseConfig = parseResolverConfig
  options = const resolverConfigOptions
  
resolverConfigOptions :: [ OptDescr Option ]
resolverConfigOptions = []

-- | Parsing of resolver config
processResolverOptions :: (MonadIO m, MonadError e m) =>
                          [Option] -> m ResolverConfig
processResolverOptions _opts = return ResolverConfig

-- | Parse the resolver configuration data
parseResolverConfig :: String -> -- ^ Version string
                       String -> -- ^ Build String
                       String -> -- ^ Program name
                       [String] -> -- ^ Command ling args
                       [OptDescr Option] -> -- ^ Relevant options
                       IO ResolverConfig -- ^ Beanstalk config
parseResolverConfig _versionString _buildString _progName _args _os =
  return ResolverConfig
  