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

import Control.Applicative
import Data.Text hiding (map, zip)
import Database.Persist.Sql
import Text.XML.Light.Input
import Text.XML.Light.Types
import Yesod

import Multipod.Core
import Multipod.Network
import Multipod.PodcastData
import Multipod.PodcastReader

data App = App
  { dataApp :: DataApp
  }

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB (persistConfig . dataApp) $ connPool . dataApp
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner $ connPool . dataApp
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "App" [parseRoutes|
/ HomeR GET POST
/podcast/#String PodcastR GET
|]


type Form a = Html -> MForm Handler (FormResult a, Widget)

form :: Form Text
form = renderDivs $ areq textField "" Nothing


displayHome podcasts widget enctype = do
    defaultLayout $ do
        setTitle "Synced podcasts"
        [whamlet|
            <ul>
                $forall Entity podcastId podcast <- podcasts
                    <li>
                        <a href=@{PodcastR $ podcastName podcast}>
                            #{podcastName podcast}
            <form method=post action=@{HomeR} enctype=#{enctype}>
                ^{widget}
                <button>Add
        |]

getHomeR :: Handler Html
getHomeR = do
    podcasts <- runDB getAllPodcast
    ((res, widget), enctype) <- runFormPost form

    displayHome podcasts widget enctype

handleResult :: FormResult Text -> Handler ()
handleResult res = case res of
    FormSuccess address -> do

      htmlString <- requestBody (unpack address)
      let contents = parseXML htmlString
      title <- getPodcastTitle contents
      episodes <- getPodcastEpisodes contents

      id <- addPodcast $ mkPodcast (unpack title) (unpack address)
      sequence $ map
        (\episode -> do
           title <- getEpisodeTitle episode
           url   <- getEpisodeUrl   episode
           addEpisode $ mkEpisode (unpack title) (unpack url) id)
        episodes
      return ()

    _ -> return ()

postHomeR :: Handler Html
postHomeR = do
    ((res, widget), enctype) <- runFormPost form

    handleResult res

    podcasts <- runDB $ getAllPodcast
    displayHome podcasts widget enctype


extractInfos :: Maybe (Entity Podcast) -> Handler [(Text, Text)]
extractInfos podcast = case podcast of
    Just (Entity id _) -> do
      episodes <- runDB $ getEpisodesFromPodcastId id
      return $ map
        (\(Entity _ e) -> (pack $ episodeName e, pack $ episodeUrl e))
        episodes

    Nothing -> return []

getPodcastR :: String -> Handler Html
getPodcastR  name = do
    podcastAddress <- runDB $ getPodcastFromName name

    titles <- extractInfos podcastAddress

    defaultLayout $ do
        setTitle "Synced podcasts"
        [whamlet|
            <ul>
                $forall (title, url) <- titles
                    <li>
                        <a href=#{url}>
                          #{title}
        |]

launchApp = do
  dataApp <- mkDataApp
  warp 3000 App {dataApp = dataApp}
