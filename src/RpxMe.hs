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
import System.Environment      ( getArgs, getProgName )
import qualified Data.ByteString.Char8 as B
-- Snap imports
import Snap.Extension.Server   ( httpServe )
import Snap.Http.Server.Config ( addListen, ConfigListen (..), defaultConfig )
-- Local imports
import RpxMe.Application      ( applicationInitializer )
import RpxMe.Site             ( site )
import Handler.RpxMe.Args     ( parseRpxMeConfig, rpxMeConfigOptions )
import State.RpxMe.RpxMeState ( RpxMeState ( .. ), getHostPort )
import Version                ( versionString, buildString )
import State.HandlerState     ( HandlerState ( .. ) )
import Config.HostPort        ( HostPort (..) )

main :: IO ()
main = do
  putStrLn ( "Hello from the refactored RpxMe server, ported to Snap" )
  -- Get the command line arguments and the program name under which
  -- this server is running
  argv <- getArgs
  pname <- getProgName
  -- Parsing is divided into two sections, first we get arguments for
  -- the Janrain extensions to the Snap server.
  config <- parseRpxMeConfig versionString buildString pname argv rpxMeConfigOptions
  putStrLn ( "Config has been parsed: " ++ show config )
  -- Convert the Janrain configuration information into Janrain
  -- state information
  state :: RpxMeState <- fromCfg config
  putStrLn ( "Config has been converted to state: " ++ show state )
  -- Serve the HTTP requests. The Janrain state is incorporated into Snap
  -- state via the call to @applicationInitializer@
  putStrLn ( "Ready to serve requests:" )
  let HostPort { hostPort = p, hostAddr = a } = getHostPort state
      snapConfig = addListen (ListenHttp (B.pack a) (toEnum p)) defaultConfig
  httpServe snapConfig (applicationInitializer state) site
  -- Should never see this print out
  putStrLn "RpxMe server has completed"
