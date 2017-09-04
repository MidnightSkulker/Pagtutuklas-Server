module Database.HDBC.ConnectionPool
    ( Pool
    , createPool
    , withConnection
    , shutdownPool
    , rollbackPool
    ) where

import Control.Concurrent.Chan ( writeChan, writeList2Chan, newChan, readChan )
import Control.Monad ( replicateM_, replicateM )
import Control.Exception ( finally )
import Database.HDBC ( IConnection(..) )
import State.DBState ( Pool (..) )

-- |Create a connection pool based on an existing database connection
-- with the specified number of connections initially available in the
-- pool.
createPool :: (IConnection a) => a -> Int -> IO (Pool a)
createPool _ numConnections | numConnections < 1 =
                                fail "Cannot create a connection pool with \
                                     \fewer than one connection"
createPool conn numConnections = do
  chan <- newChan
  replicateM_ numConnections $ do
    conn' <- clone conn
    writeChan chan $! conn'

  return $ Pool { connChannel = chan
                , poolNumConnections = numConnections }

-- |Given a connection pool and an action, wait for an available
-- connection from the pool and execute the action.  Return the
-- result.
withConnection :: (IConnection a) => Pool a -> (a -> IO b) -> IO b
withConnection pool handler = do
    conn <- getConnection pool
    commit conn
    result <- handler conn `finally` (commit conn >> putConnection pool conn)
    return result

getConnection :: (IConnection a) => Pool a -> IO a
getConnection pool = readChan $ connChannel pool

putConnection :: (IConnection a) => Pool a -> a -> IO ()
putConnection pool conn = writeChan (connChannel pool) $! conn

shutdownPool :: (IConnection a) => Pool a -> IO ()
shutdownPool pool = do
  replicateM_ (poolNumConnections pool) $ do
      conn <- readChan $ connChannel pool
      disconnect conn

-- This is a horrible workaround for the following horrible problem:
--
-- The HDBC driver starts every connection in the pool running an idle
-- transaction.  Sometimes we need to CREATE INDEX CONCURRENTLY, which
-- requires all transactions started before it to commit or rollback,
-- and will hang until they do.  So we need a way to roll back every
-- idle transaction in the discovery server's pool.  So we create an
-- endpoint which triggers this method.
--
-- Note that this function will disrupt the service (hopefully
-- momentarily) by starving the connection pool.
rollbackPool :: (IConnection a) => Pool a -> IO ()
rollbackPool pool = do
  let chan = connChannel pool
  conns <- replicateM (poolNumConnections pool) $ readChan chan
  mapM_ rollback conns
  writeList2Chan chan conns
