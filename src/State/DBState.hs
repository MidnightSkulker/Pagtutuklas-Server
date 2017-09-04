-- | State information relevant to Beanstalk
--   During initialization of a handler, this state will be created
--   from the corresponding DB configuration information, and then
--   the state will be made part of the state of the handler monad.
module State.DBState
    ( DBState ( .. )
    , MonadDB ( .. )
    , HasDBState ( .. )
    , fromDBConfig  
    , getConnPool
    , Pool (..)
    ) where

import qualified Snap.Types as S
import Snap.Extension
import Control.Monad.Reader ( asks, ReaderT )
import Control.Concurrent.Chan ( Chan, newChan, writeChan )
import Control.Monad ( replicateM_ )
import Database.HDBC ( IConnection(..) )
import Database.HDBC.PostgreSQL ( Connection, connectPostgreSQL )
import Config.DBConfig ( DBConfig ( .. ) )

-- | A pool of database connections. The pool encapsulates an IO
--   channel to the database.
data Pool a = Pool { connChannel :: Chan a
                   , poolNumConnections :: Int
                   }

instance Show DBState where
  show _ = "DBState (Not much to show)"
  
-- | This data type contains all the database relevant state information
data DBState = DBState { stDbConnPool :: Pool Connection }
  
-- | A class that characterizes having a database state
class HasDBState s where
  getDBState :: s -> DBState

instance HasDBState DBState where
  getDBState = id
  
-- | Create the DB relevant state information from the DB relevant
--   configuration information. As a result of initializing the
--   DB state, there will be a connection to a database.
fromDBConfig :: DBConfig -> IO DBState
fromDBConfig cfg = do
  let poolSize = dbConnPoolSize cfg
  conn <- connectPostgreSQL $ dbSettings cfg
  pool <- createPool conn poolSize
  return ( DBState { stDbConnPool = pool } )

-- | Fish the the DBConnPool out of the application state
getConnPool :: (HasDBState st) => st -> Pool Connection
getConnPool st = stDbConnPool (getDBState st)

-- | Create a connection pool based on an existing database connection
-- with the specified number of connections initially available in the
-- pool.
createPool :: (IConnection a) => a -> Int -> IO (Pool a)
createPool _ numConnections | numConnections < 1 =
                                fail "Cannot create a connection pool with \
                                     \fewer than one connection"
createPool conn numConnections = do
  chan <- newChan
  replicateM_ numConnections $ do conn' <- clone conn
                                  writeChan chan $! conn'
  return $ Pool { connChannel = chan
                , poolNumConnections = numConnections }

------------------------------------------------------------------------------
-- | The 'MonadDB' type class.
class S.MonadSnap m => MonadDB m where
  -- | Get the listening port out of the monad plumbing
  fishConnPool :: m ( Pool Connection )

instance HasDBState s => MonadDB (SnapExtend s) where
  fishConnPool = do rpx <- asks getDBState
                    return ( getConnPool rpx )

instance (S.MonadSnap m, HasDBState s) => MonadDB (ReaderT s m) where
  fishConnPool = do rpx <- asks getDBState
                    return ( getConnPool rpx )
