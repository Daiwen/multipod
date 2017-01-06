{-# LANGUAGE EmptyDataDecls             #-}                                     
{-# LANGUAGE FlexibleContexts           #-}                                     
{-# LANGUAGE GADTs                      #-}                                     
{-# LANGUAGE GeneralizedNewtypeDeriving #-}                                     
{-# LANGUAGE MultiParamTypeClasses      #-}                                     
{-# LANGUAGE OverloadedStrings          #-}                                     
{-# LANGUAGE QuasiQuotes                #-}                                     
{-# LANGUAGE TemplateHaskell            #-}                                     
{-# LANGUAGE TypeFamilies               #-}                                     
{-# LANGUAGE ViewPatterns               #-}         

module Multipod.App
  ( launchApp
  ) where

import Database.Persist.Sql
import Yesod

import Multipod.Core
import Multipod.PodcastData

data App = App
  { dataApp :: DataApp
  }

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB (persistConfig . dataApp) $ connPool . dataApp
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner $ connPool . dataApp

mkYesod "App" [parseRoutes|
/ HomeR GET
/podcast/#String PodcastR GET
|]

getHomeR :: Handler Html
getHomeR = do
    podcasts <- runDB $ selectList [] []

    defaultLayout $ do
        setTitle "Synced podcasts"
        [whamlet|
            <ul>
                $forall Entity podcastId podcast <- podcasts
                    <li>
                        <a href=@{PodcastR $ podcastName podcast}>
                            #{podcastName podcast}
        |]

getPodcastR :: String -> Handler Html
getPodcastR = error "Implementation left as exercise to reader"

launchApp = do
  dataApp <- mkDataApp
  warp 3000 App {dataApp = dataApp}
