-- | State information relevant to Beanstalk
module State.BeanstalkState
    ( BeanstalkState    ( .. )
    , HasBeanstalkState ( .. )
    , MonadBeanstalk ( .. )
    , fromBeanstalkConfig
    , getBeanstalkTube
    , getBeanstalkJobLimit
    , getBeanstalkOverflowDir
    , getBeanstalkPool
    , getBeanstalkJobPri
    , getBeanstalkJobDelay
    , getBeanstalkJobTTR
    ) where

import Snap.Extension
import qualified Snap.Types as S
import Control.Monad.Reader ( asks, ReaderT )
import qualified Data.ByteString.Char8 as B
import Network.BSD ( hostName )
import Network.Beanstalk.Pool ( createBeanstalkPoolConnection
                              , createBeanstalkPool, BeanstalkPool )
import Control.Monad          ( replicateM )
import Config.BeanstalkConfig ( BeanstalkConfig (..) )
import Config.HostPort        ( HostPort (..) )

-- | This data type contains all the beanstalk relevant state
--   information
data BeanstalkState = BeanstalkState
  { appBeanstalkPool        :: BeanstalkPool
  , appBeanstalkTube        :: String
  , appBeanstalkJobLimit    :: Int
  , appBeanstalkOverflowDir :: String
  , appBeanstalkJobPri      :: Int
  , appBeanstalkJobDelay    :: Int
  , appBeanstalkJobTTR      :: Int
  }

instance Show BeanstalkState where
  show b = "   Tube = " ++ (show $ appBeanstalkTube b) ++ ", " ++
           "   JobLimit = " ++ (show $ appBeanstalkJobLimit b) ++ ", " ++
           "   OverflowDir = " ++ appBeanstalkOverflowDir b ++ ", " ++
           "   JobPri = " ++ (show $ appBeanstalkJobPri b) ++ ", " ++
           "   JobDelay = " ++ (show $ appBeanstalkJobDelay b) ++ ", " ++
           "   JobTTR = " ++ (show $ appBeanstalkJobTTR b)

-- | Convert beanstalk configuration to beanstalk application state
fromBeanstalkConfig :: BeanstalkConfig -> IO BeanstalkState
fromBeanstalkConfig cfg = do
  let HostPort { hostEntry = bsHost, hostPort = bsPort, hostAddr = _a } =
        beanstalkHostPort cfg
      bsPoolSiz = bsPoolSize cfg
      bsTubeNam = bsTubeName cfg
      bsJobLimi = bsJobLimit cfg
      bsOverflowDi = bsOverflowDir cfg
  
  bsConns <- replicateM bsPoolSiz $ createBeanstalkPoolConnection (hostName bsHost) (show bsPort)
  bsPool <- createBeanstalkPool bsConns

  return BeanstalkState { appBeanstalkPool        = bsPool
                        , appBeanstalkTube        = bsTubeNam
                        , appBeanstalkJobLimit    = bsJobLimi
                        , appBeanstalkOverflowDir = bsOverflowDi
                        , appBeanstalkJobPri      = 65536
                        , appBeanstalkJobDelay    = 0
                        , appBeanstalkJobTTR      = 120
                        }
                 
-- | Class characterizing having a beanstalk state
class HasBeanstalkState s where
  getBeanstalkState :: s -> BeanstalkState
  setBeanstalkState :: BeanstalkState -> s -> s
  modifyBeanstalkState :: (BeanstalkState -> BeanstalkState) -> s -> s
  modifyBeanstalkState f s = setBeanstalkState (f $ getBeanstalkState s) s

instance HasBeanstalkState BeanstalkState where
  getBeanstalkState = id
  setBeanstalkState _b s = s

------------------------------------------------------------------------------
-- | The 'MonadBeanstalk' type class.
class S.MonadSnap m => MonadBeanstalk m where
  fishBeanstalkPool        :: m BeanstalkPool
  fishBeanstalkTube        :: m String
  fishBeanstalkJobLimit    :: m Int
  fishBeanstalkOverflowDir :: m String
  fishBeanstalkJobPri      :: m Int
  fishBeanstalkJobDelay    :: m Int
  fishBeanstalkJobTTR      :: m Int

------------------------------------------------------------------------------
instance InitializerState BeanstalkState where
    extensionId = const (B.pack "Beanstalk/Beanstalk")
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

------------------------------------------------------------------------------
instance HasBeanstalkState s => MonadBeanstalk (SnapExtend s) where
   fishBeanstalkPool = do base <- asks getBeanstalkState
                          return ( getBeanstalkPool base )
   fishBeanstalkTube        = undefined
   fishBeanstalkJobLimit    = undefined
   fishBeanstalkOverflowDir = undefined
   fishBeanstalkJobPri      = undefined
   fishBeanstalkJobDelay    = undefined
   fishBeanstalkJobTTR      = undefined

------------------------------------------------------------------------------
instance (S.MonadSnap m, HasBeanstalkState s) => MonadBeanstalk (ReaderT s m) where
   fishBeanstalkPool = do base <- asks getBeanstalkState
                          return ( getBeanstalkPool base )
   fishBeanstalkTube        = undefined
   fishBeanstalkJobLimit    = undefined
   fishBeanstalkOverflowDir = undefined
   fishBeanstalkJobPri      = undefined
   fishBeanstalkJobDelay    = undefined
   fishBeanstalkJobTTR      = undefined

-- | Fish the beanstalk tube out of the application state
getBeanstalkTube :: (HasBeanstalkState st) => st -> String
getBeanstalkTube st = appBeanstalkTube (getBeanstalkState st)
  
-- | Fish the beanstalk job limit out of the application state
getBeanstalkJobLimit :: (HasBeanstalkState st) => st -> Int
getBeanstalkJobLimit st = appBeanstalkJobLimit (getBeanstalkState st)

-- | Fish the beanstalk overflow directory out of the application state
getBeanstalkOverflowDir :: (HasBeanstalkState st) => st -> String
getBeanstalkOverflowDir st = appBeanstalkOverflowDir (getBeanstalkState st)

-- | Fish the beanstalk pool out of the application state
getBeanstalkPool :: (HasBeanstalkState st) => st -> BeanstalkPool
getBeanstalkPool st = appBeanstalkPool (getBeanstalkState st)

-- | Fish the beanstalk job priority out of the application state
getBeanstalkJobPri :: (HasBeanstalkState st) => st -> Int
getBeanstalkJobPri st = appBeanstalkJobPri (getBeanstalkState st)

-- | Fish the beanstalk job delay out of the application state
getBeanstalkJobDelay :: (HasBeanstalkState st) => st -> Int
getBeanstalkJobDelay st = appBeanstalkJobDelay (getBeanstalkState st)

-- | Fish the beanstalk job TTR out of the application state
getBeanstalkJobTTR :: (HasBeanstalkState st) => st -> Int
getBeanstalkJobTTR st = appBeanstalkJobTTR (getBeanstalkState st)
