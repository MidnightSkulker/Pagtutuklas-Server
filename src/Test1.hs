-- | Tests the extra interfaces for RpxMe and BaseUrl servers.

module Main where

import Network.HTTP          ( simpleHTTP, getRequest )
import Data.List             ( find )
import Data.Maybe            ( fromJust, isNothing )
import Control.Monad         ( when, mapM_ )
import System.Console.GetOpt ( OptDescr (..), getOpt, ArgDescr (..), ArgOrder ( Permute ) )
import System.Environment    ( getArgs )
import System.Exit           ( exitWith, ExitCode (..) )

data Option = IPAddress String
            | Port Int
            | Repeat Int
            | Message String
            | Resource String
              deriving ( Show )

data OptionType = IPOption | PortOption | RepeatOption | MessageOption | ResourceOption
                  deriving ( Eq, Enum, Show )

optionType :: Option -> OptionType
optionType (IPAddress _) = IPOption
optionType (Port _)      = PortOption
optionType (Repeat _)    = RepeatOption
optionType (Message _)   = MessageOption
optionType (Resource _)  = ResourceOption

getOption :: OptionType -> [ Option ] -> Maybe Option
getOption t os = find ( \o -> optionType o == t ) os

-- | Options relevant to testing the handlers
cmdOptions :: [ OptDescr Option ]
cmdOptions =
  [ Option "" ["ip"] (ReqArg IPAddress "IP Address") "The IP Address of the server"
  , Option "" ["port"] (ReqArg portArg "Port") "The TCP Port number of the server"
  , Option "" ["repeat"] (ReqArg repeatArg "Repeat") "Repeat count"
  , Option "" ["resource"] (ReqArg Resource "Resource") "Resource to Request"
  , Option "" ["message"] (OptArg msgArg "Message") "Ping message"
  ]

-- | Convert the port argument
portArg :: String -> Option
portArg s = Port ( read s )

-- | Convert the repeat argument
repeatArg :: String -> Option
repeatArg s = Repeat ( read s )

-- | Convert the message argument
msgArg :: Maybe String -> Option
msgArg mstr = case mstr of
                Nothing  -> Message ""
                Just str -> Message str

main :: IO ()
main = do args <- getArgs
          let ( opts, nomatches, errs ) = getOpt Permute cmdOptions args
          let mip = getOption IPOption opts
              IPAddress ip = fromJust mip
              mp = getOption PortOption opts
              Port p = fromJust mp
              mrsrc = getOption ResourceOption opts
              Resource rsrc = fromJust mrsrc
              mrep = getOption RepeatOption opts
              Repeat rep = fromJust mrep
          when ( isNothing mip ) ( exitWith ( ExitFailure 1 ) )
          when ( isNothing mp ) ( exitWith ( ExitFailure 2 ) )
          when ( isNothing mrsrc ) ( exitWith ( ExitFailure 3 ) )
          when ( isNothing mrep ) ( exitWith ( ExitFailure 4 ) )
          pongReq ip p rsrc rep
          putStrLn ( "All done." )

m = main

pongReq :: String -> Int -> String -> Int -> IO ()
pongReq ip port resource n =
  let addr = "http://" ++ ip ++ ":" ++ show port ++ "/" ++ resource
  in do putStrLn ( "Requesting: " ++ addr )
        res <- simpleHTTP (getRequest addr)
        putStrLn (show res)

pings :: String -> Int -> Int -> IO ()
pings resource port n = mapM_ (pongReq "localhost" port resource) [0..n]

r1 = pings "index.html" 3000 0
r2 = pings "index.htm" 3000 0
r3 = pings "index.tpl" 3000 0
r4 = pings "" 3000 0
t1 = simpleHTTP (getRequest "http://localhost:3000")
t2 = simpleHTTP (getRequest "http://localhost:3000/index.html")
t3 = simpleHTTP (getRequest "http://localhost:3000/index.htm")
t4 = simpleHTTP (getRequest "http://localhost:3000/resources/index.htm")
t5 = simpleHTTP (getRequest "http://localhost:3000/")
t6 = simpleHTTP (getRequest "http://localhost:3000/echo/dog")
t7 = simpleHTTP (getRequest "http://localhost:3000/static/resources/index.htm")
u1 = simpleHTTP (getRequest "http://localhost:8000")
u2 = simpleHTTP (getRequest "http://localhost:8000/index.html")
u3 = simpleHTTP (getRequest "http://localhost:8000/index.htm")
u4 = simpleHTTP (getRequest "http://localhost:8000/resources/index.htm")
u5 = simpleHTTP (getRequest "http://localhost:8000/")
u6 = simpleHTTP (getRequest "http://localhost:8000/echo/dog")
u7 = simpleHTTP (getRequest "http://localhost:8000/static/resources/index.htm")
