{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Multipod.PodcastData
  ( addPodcasts
  , mkPodcast
  , initDataBase
  , Podcast
  , podcastName
  , DataError
  , DataApp
  , persistConfig
  , connPool
  , mkDataApp
  ) where

import Control.Monad.Catch hiding (catchIOError)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Text hiding (map)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Environment
import System.Directory
import System.IO
import System.Posix.Files

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Podcast
    name String
    url String
    deriving Show
|]

data DataApp = DataApp
  { persistConfig :: SqliteConf
  , connPool      :: ConnectionPool
  }


data DataError
  = AlreadySync
  deriving (Eq)

instance Show DataError where
  show AlreadySync = "Podcast already subscribed to."

instance Exception DataError

mkPodcast title url = Podcast title url

getDataBasePath :: IO Text
getDataBasePath = do
  home <- getHomeDirectory
  return $ append (pack home) "/.multipod.db"


mkDataApp = do
  path <- getDataBasePath
  let conf = SqliteConf path 1
  pool <- createPoolConfig conf
  return $ DataApp
    { persistConfig = conf
    , connPool      = pool
    }

initDataBase :: IO ()
initDataBase = do
  dataBasePath <- getDataBasePath
  runSqlite dataBasePath $ do runMigration migrateAll

addPodcasts
  :: (MonadIO m, MonadBaseControl IO m, MonadLogger m, MonadThrow m)
  => Podcast -> m ()
addPodcasts podcast = do
  dataBasePath <- liftIO getDataBasePath
  withSqliteConn dataBasePath $
    runSqlConn $
    do runMigration migrateAll
       sameUrl <- selectList [PodcastUrl ==. podcastUrl podcast] []
       case sameUrl of
         [] -> do insert podcast; return ()
         _  -> throwM AlreadySync
