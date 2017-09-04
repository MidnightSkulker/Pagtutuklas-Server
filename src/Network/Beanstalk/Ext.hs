{-# LANGUAGE ScopedTypeVariables #-}
module Network.Beanstalk.Ext
    ( withBeanstalkServerOrFile
    ) where

import Prelude hiding ( catch )

import System.Posix.Process ( getProcessID )
import Control.Concurrent ( myThreadId )
import Control.Exception ( IOException, catch )
import Control.Monad.Trans ( liftIO )
import Data.ByteString.UTF8 ( fromString )
import Network.Beanstalk ( jobCountWithState
                         , putJob
                         , JobState(..)
                         , BeanstalkException ( DeadlineSoonException
                                              , TimedOutException
                                              , NotIgnoredException
                                              )
                         )
import Network.Beanstalk.Pool ( BeanstalkPool, withBeanstalkServer )
import Application ( Application )
import State.BeanstalkState ( MonadBeanstalk ( .. ), HasBeanstalkState )

tubeJobCount :: BeanstalkPool -> String -> IO Int
tubeJobCount pool tube = do
    withBeanstalkServer pool $ \server -> jobCountWithState server (fromString tube) [ READY, RESERVED, DELAYED, BURIED ]

withBeanstalkServerOrFile :: ( HasBeanstalkState st ) =>
                             String -> Application st ()
withBeanstalkServerOrFile job = do
    tube <- fishBeanstalkTube
    jobLimit <- fishBeanstalkJobLimit
    dir <- fishBeanstalkOverflowDir
    pool <- fishBeanstalkPool
    pri <- fishBeanstalkJobPri
    delay <- fishBeanstalkJobDelay
    ttr <- fishBeanstalkJobTTR
    jobCount <- liftIO $ (tubeJobCount pool tube)
               `catch` (\(_::BeanstalkException) -> return jobLimit)
               `catch` (\(_::IOException) -> return jobLimit)

    -- Dump the job to a file if beanstalk is full or fails.
    liftIO $ if jobCount >= jobLimit
             then appendJobToFile dir job
             else putJob' pool pri delay ttr job
                  `catch` handleBeanstalkExceptions dir
                  `catch` (\(_::IOException) -> appendJobToFile dir job)
                      where
                          putJob' pool pri delay ttr job' = do
                              liftIO $ (withBeanstalkServer pool $ \server -> putJob server pri delay ttr (fromString job'))
                              return ()
                          handleBeanstalkExceptions (dir::String) (e::BeanstalkException) =
                              case e of
                                  DeadlineSoonException -> return ()
                                  TimedOutException -> return ()
                                  NotIgnoredException -> return ()
                                  _ -> appendJobToFile dir job

appendJobToFile :: String -> String -> IO ()
appendJobToFile dir job = do
    pid <- getProcessID
    tid <- myThreadId
    let fname = dir ++ "/" ++ show pid ++ "-" ++ show tid
    return ()
    appendFile fname job
