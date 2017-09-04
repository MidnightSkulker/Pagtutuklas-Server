{-# OPTIONS -XScopedTypeVariables #-}
{-|

This is the entry point for this web server application.  It supports
easily switching between interpreting source and running statically
compiled code.

In either mode, the generated program should be run from the root of
the project tree.  When it is run, it locates its templates, static
content, and source files in development mode, relative to the current
working directory.

 -}

module Main where

-- Hackage and Haskell imports
import System.Environment         ( getArgs, getProgName )
import qualified Data.ByteString.Char8 as B
-- Snap imports
import Snap.Extension.Server      ( httpServe )
import Snap.Http.Server.Config    ( addListen, ConfigListen (..), defaultConfig )
-- Local imports
import BaseUrl.Application        ( applicationInitializer )
import BaseUrl.Site               ( site )
import Handler.BaseUrl.Args       ( parseBaseUrlConfig, baseUrlConfigOptions )
import State.BaseUrl.BaseUrlState ( BaseUrlState ( .. ), getHostPort )
import Version                    ( versionString, buildString )
import State.HandlerState         ( HandlerState ( .. ) )
import Config.HostPort            ( HostPort (..) )

-- | The main program of the BaseUrl server
--   Required arguments:
--     Regarding the database:
--       (ReqArg DbConnString "CONNECTION-STRING")
--       (ReqArg DbConnPoolSize "SIZE")
--     BaseUrl specific
--       None
--     RPX ReqArg
--       (Server DefaultProtocol "(http|https)")
--     Minimal Config
--       (ReqArg Port "HOST:PORT")
--       (Arg MaxConnections "CONNECTIONS")
main :: IO ()
main = do
  -- Tada!
  putStrLn ( "Hello from the refactored BaseUrl server, ported to Snap" )
  -- Get the command line arguments and the program name under which
  -- this server is running
  argv <- getArgs
  putStrLn ( "Program arguments: " ++ show argv )
  pname <- getProgName
  putStrLn ( "Program name is: " ++ pname )
  -- Parsing is divided into two sections, first we get arguments for
  -- the Janrain extensions to the Snap server.
  config <- parseBaseUrlConfig versionString buildString
                               pname argv baseUrlConfigOptions
  putStrLn ( "Config has been parsed: " ++ show config )
  -- Convert the Janrain configuration information into Janrain
  -- state information
  state :: BaseUrlState <- fromCfg config
  putStrLn ( "Config has been converted to state: " ++ show state )
  -- Serve the HTTP requests. The Janrain state is incorporated into Snap
  -- state via the call to @applicationInitializer@
  putStrLn ( "Ready to serve requests:" )
  let HostPort { hostPort = p, hostAddr = a } = getHostPort state
      snapConfig = addListen (ListenHttp (B.pack a) (toEnum p)) defaultConfig
  httpServe snapConfig (applicationInitializer state) site
  -- Should never see this print out
  putStrLn "BaseUrl server has completed"
