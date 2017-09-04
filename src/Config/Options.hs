-- | Define parsing options
module Config.Options
       ( Option (..) 
       , argsToOptions
       , singleParameter
       , stringParameter
       , hostPortOption
       , parseHostPortOption
       , die
       ) where

import System.Exit        ( ExitCode (..), exitWith )
import System.Console.GetOpt ( ArgOrder (..), getOpt, OptDescr, usageInfo )
import System.IO     ( hPutStrLn, stderr)
import Control.Monad.Trans ( MonadIO, liftIO )
import Control.Monad.Error ( MonadError )
import Network.BSD   ( getHostByName )
import Control.Monad ( liftM )
import Data.List     ( intercalate )
import Data.Char ( isDigit )
import Config.HostPort ( HostPort (..) )

data Option = Help
            | Version
            | ExactVersion
            | ShowConfig
            | Port String                -- IPv4Addr:Port
            | MaxConnections String      -- MinConfig
            | MemcacheHostPort String
            | BeanstalkHostPort String   -- IPv4Addr:Port
            | BsPoolSize String
            | BsTubeName String
            | BsJobLimit String
            | BsOverflowDir String
            | ProxyHostPort String       -- For disco server
            | DefaultProtocol String
            | DbConnString String
            | DbConnPoolSize String
            | RpxHost String
            | TemplatePath String
            | TranslationPath String
            | VerboseLogging
            | MailServer String
            | MailDomain String
            | MailFrom String
            | MailTo String
              deriving ( Eq, Show )

-- | Convert argument strings to options
argsToOptions :: String            -> -- ^ The version string
                 String            -> -- ^ The build string
                 String            -> -- ^ The program name
                 [String]          -> -- ^ The list of argument strings
                 [OptDescr Option] -> -- ^ The list of valid options
                 Either String [Option] -- ^ Returns list of options, or error
argsToOptions versionString buildString pname args options =
  let (opts, _, errs) = getOpt Permute options args
      header = versionString ++ buildString ++ "\nUsage: " ++ pname
      info = usageInfo header options
  in if length errs > 0
     then Left ( info ++ intercalate "\n" errs )
     else Right opts

--   when (Help `elem` opts) $ do
--     putStrLn info
--     exitWith ExitSuccess

--   when (Version `elem` opts) $ do
--     putStrLn versionString
--     exitWith ExitSuccess

--   when (ExactVersion `elem` opts) $ do
--     putStrLn $ versionString ++ "\n"
--     putStrLn buildString
--     exitWith ExitSuccess

  
-- options :: [OptDescr Option]
-- options =
--     [ Option "h" ["help"] (NoArg Help) "Show this help message"
--     , Option "" ["version"] (NoArg Version) "Show version information"
--     , Option "" ["exact-version"] (NoArg ExactVersion)
--                  "Show complete version information (including dependencies)"
--     , Option "s" ["show-config"] (NoArg ShowConfig)
--                  "Show the config we're using"
--     , Option "l" ["listen"] (ReqArg Listen "HOST:PORT")
--                  "address to listen on (default: localhost:4001)"
--     , Option "m" ["memcache-hostport"] (ReqArg MemcacheHostPort "HOST:PORT")
--              "The host and port of the memcached (host:port)"
--     , Option "" ["beanstalk-hostport"] (ReqArg BeanstalkHostPort "HOST:PORT")
--              "The host and port of the beanstalkd (host:port)"
--     , Option "" ["beanstalk-pool-size"] (ReqArg BsPoolSize "SIZE")
--              "The number of connections to put in the beanstalk connection pool"
--     , Option "" ["beanstalk-tube-name"] (ReqArg BsTubeName "NAME")
--              "The beanstalk tube to use instead of \"default\""
--     , Option "" ["beanstalk-job-limit"] (ReqArg BsJobLimit "NUMBER")
--              "Max beanstalk tube size; overflow goes to the overflow directory"
--     , Option "" ["beanstalk-overflow-dir"] (ReqArg BsOverflowDir "DIRECTORY")
--              "The directory where to save overflow beanstalk jobs"
--     , Option "r" ["default-protocol"] (ReqArg DefaultProtocol "(http|https)")
--              "What protocol to use for the redirect if it can't be detected \
--              \(default: https)"
--     , Option "" ["proxy"] (ReqArg ProxyHostPort "HOST:PORT")
--              "The host and port of the HTTP proxy (host:port)"
--     , Option "" ["db-conn-string"] (ReqArg DbConnString "CONNECTION-STRING")
--              "The database connection string, e.g., 'dbname=mydb user=murphy'"
--     , Option "" ["db-conn-pool-size"] (ReqArg DbConnPoolSize "SIZE")
--              "The number of connections to put in the database connection pool"
--     , Option "" ["rpx-host"] (ReqArg RpxHost "HOST")
--              "The base RPX hostname (e.g. rpxnow.com or jd.magbox.com)"
--     , Option "" ["template-path"] (ReqArg TemplatePath "PATH")
--              "The base HStringTemplate template path"
--     , Option "v" ["verbose"] (NoArg VerboseLogging)
--              "More detailed logging"
--     , Option "" ["translation-path"] (ReqArg TranslationPath "PATH")
--              "The directory containing the Rails translations (.yml files)"
--     , Option "" ["mail-server"] (ReqArg MailServer "SERVER")
--              "The SMTP server address to use when sending e-mail"
--     , Option "" ["mail-domain"] (ReqArg MailDomain "MAIL-DOMAIN")
--              "The domain to use in the SMTP EHLO"
--     , Option "" ["mail-from"] (ReqArg MailFrom "ADDRESS")
--              "Source address for e-mail messages (default: tracebacks@MAIL-DOMAIN)"
--     , Option "" ["mail-to"] (ReqArg MailTo "ADDRESS")
--              "Destination address for e-mail messages (repeat option to add addresses)"
--     , Option "c" ["max-connections"] (ReqArg MaxConnections "CONNECTIONS")
--              "The maximum number of simultaneous connections to allow"
--     ]

-- | Parse a single command line parameter
singleParameter :: Monad m =>
                   String     -> -- ^ Name of the parameter
                   [a]        -> -- ^ List of OptDescr that suit the parameter
                   b          -> -- ^ Default value of the parameter
                   (a -> m b) -> -- ^ Function to parse the parameter
                   m b           -- ^ Action that returns the value of param
singleParameter name vals dflt parse =
    case vals of
      [] -> return dflt
      [x] -> parse x
      _ -> fail $ "Duplicate parameter " ++ name

-- | Parse a host port option of the form addr:port, where addr
--   is an IPVr address
hostPortOption :: (MonadError e m, MonadIO m) => String -> [String] -> HostPort
               -> m HostPort
hostPortOption name vals dflt =
    singleParameter name vals dflt $ parseHostPortOption name

-- | Parse a host port option of the form addr:port, where addr
--   is an IPVr address (helper function to hostPortOption)
parseHostPortOption :: (MonadError e m, MonadIO m) => String -> String -> m HostPort
parseHostPortOption name s =
    case break (== ':') s of
      -- Nothing after the colon, so port is missing
      ([], _) -> fail $ "Missing " ++ name ++ " host portion"
      -- Check out what is before and after the colon
      -- The host is looked up using DNS
      (host, ':':maybePort) ->
          do eitherHostEntry <-
                 liftIO $ catch (Right `liftM` getHostByName host)
                                (return . Left . showNameLookupError host name)
             hostEnt <- either fail return eitherHostEntry
             -- Make sure the port name is all digits
             p <- parsePortOption (name ++ " port") maybePort
             -- Return the host name and the port
             return HostPort { hostEntry = hostEnt
                             , hostAddr  = host
                             , hostPort  = p
                             }
      -- Nothing before the colon, so address is missing
      (_, []) -> fail $ "Missing " ++ name ++ " port"
      -- No colon, bad specification
      _ -> error $ "impossible: " ++ show s

-- | Report an error in a DNS name lookup
showNameLookupError :: String -> String -> IOError -> String
showNameLookupError hostname name e =
    "Processing argument " ++
    show name ++ " lookup failed for " ++ show hostname ++ ": " ++ show e

-- | Make sure the port is all digits.
parsePortOption :: MonadError e m => String -> String -> m Int
parsePortOption name s =
    case span isDigit s of
      (portStr, []) -> return $ read portStr
      _ -> fail $ name ++ " not valid: " ++ show s

-- | Parse a single string valued parameter
stringParameter :: Monad m => String -> [String] -> String -> m String
stringParameter name vals dflt = singleParameter name vals dflt return

-- | Die with an error message
die :: String -> -- ^ Information about the error
       String -> -- ^ The error message
       IO a      -- ^ Polymorphic return, you can use this anywhere
die info err = do hPutStrLn stderr $ info ++ "\n\n" ++ err
                  exitWith $ ExitFailure 1
