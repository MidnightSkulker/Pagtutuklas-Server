{-# LANGUAGE ScopedTypeVariables #-}
module Network.Beanstalk.Pool
    ( BeanstalkPool
    , createBeanstalkPool
    , createBeanstalkPoolConnection
    , withBeanstalkServer
    , shutdownBeanstalkPool
    ) where

import Control.Concurrent.Chan
    ( Chan, writeChan, writeList2Chan, newChan, readChan )
import Control.Exception ( try, onException, IOException )
import Network.Socket ( HostName )
import Network.Beanstalk ( BeanstalkServer, connectBeanstalk
                         , disconnectBeanstalk )


data BeanstalkPool =
  BeanstalkPool { bpChan :: Chan (Maybe BeanstalkServer, HostName, String)
                , bpSize :: Int
                }

-- |Opens a connection to beanstalk and returns a tuple with that
-- connection suitable for insertion into a beanstalk pool.  If
-- connection fails, returns a tuple with Nothing so that the
-- connection can be retried later when it's needed.
createBeanstalkPoolConnection :: HostName -> String -> IO (Maybe BeanstalkServer, HostName, String)
createBeanstalkPoolConnection host port = do
    tried <- try $ connectBeanstalk host port
    case tried of
        Right server -> return (Just server, host, port)
        Left (_::IOException) -> return (Nothing, host, port)

-- |Create a connection pool.
createBeanstalkPool :: [(Maybe BeanstalkServer, HostName, String)] -> IO (BeanstalkPool)
createBeanstalkPool [] = fail "Cannot create a connection pool with \
                     \fewer than one connection"
createBeanstalkPool conns = do
  chan <- newChan
  writeList2Chan chan conns
  return $ BeanstalkPool chan $ length conns

-- |Given a connection pool and an action, wait for an available
-- connection from the pool and execute the action.  On IO errors
-- (which are assumed to be connection loss) try to reconnect (once)
-- and re-execute the action.  Return the result of the action.
withBeanstalkServer :: BeanstalkPool -> (BeanstalkServer -> IO b) -> IO b
withBeanstalkServer pool handler = do
    let chan = bpChan pool
    (mServer, host, port) <- readChan chan
    -- Make sure we write out a Nothing connection to the pool on
    -- exceptions, to avoid depleting the pool.
    (newServer, result) <- onException
                           (withBeanstalkServer' mServer host port handler)
                           (writeChan chan (Nothing, host, port)
                            -- Possible double close, not a problem in Haskell.
                            >> maybe (return ()) disconnectBeanstalk mServer)
    writeChan chan (Just newServer, host, port)
    return result

withBeanstalkServer' :: Maybe BeanstalkServer -> HostName -> String -> (BeanstalkServer -> IO b) -> IO (BeanstalkServer, b)
withBeanstalkServer' mServer host port handler = do
    -- Reconnect if mServer is Nothing.  Let exceptions bubble up if
    -- reconnecting fails.
    server <- maybe (connectBeanstalk host port) return mServer
    -- Run the handler on the server.  On IO errors, try to reconnect
    -- and re-run the handler.
    tried <- try $ handler server
    case tried of
        Right result -> return (server, result)
        Left (_::IOException) -> disconnectBeanstalk server
                                 >> connectBeanstalk host port
                                 >>= (\srv -> handler srv `onException` disconnectBeanstalk srv
                                              >>= \result -> return (srv, result))

shutdownBeanstalkPool :: BeanstalkPool -> IO ()
shutdownBeanstalkPool pool | bpSize pool == 0 = return ()
shutdownBeanstalkPool pool = do
    let chan = bpChan pool
    (mServer, _, _) <- readChan chan
    maybe (return ()) disconnectBeanstalk mServer
    shutdownBeanstalkPool $ pool { bpSize = (bpSize pool) - 1 }
