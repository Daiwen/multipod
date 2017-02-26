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

form :: Form (Maybe Text)
form = renderDivs $ aopt textField "" Nothing

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
                <button type=submit name=action value=add>Add
                <button type=submit name=action value=update>Update
        |]

getHomeR :: Handler Html
getHomeR = do
    podcasts <- runDB getAllPodcast
    (widget, enctype) <- generateFormPost form

    displayHome podcasts widget enctype

getTitleAndEpisodes :: Text -> Handler (Text, [Element])
getTitleAndEpisodes address = do
    htmlString <- requestBody (unpack address)
    let contents = parseXML htmlString
    title <- getPodcastTitle contents
    episodes <- getPodcastEpisodes contents
    return (title, episodes)

addEpisodes :: Key Podcast -> Element -> Handler ()
addEpisodes id episode = do
    title <- getEpisodeTitle episode
    url   <- getEpisodeUrl   episode
    addEpisode $ mkEpisode (unpack title) (unpack url) id

updatePodcasts :: Handler ()
updatePodcasts = do
    podcasts <- runDB $ getAllPodcast
    let idurls = map (\(Entity id p) -> (id, pack $ podcastUrl p)) podcasts
    sequence $ map
      (\(id, url) -> do
        (_, episodes) <- getTitleAndEpisodes url
        sequence $ map (addEpisodes id) episodes)
      idurls
    return ()


handleResult :: FormResult (Maybe Text) -> Handler ()
handleResult res = do
    action <- lookupPostParam "action"
    case (res, action) of
      (_, Just "update") -> updatePodcasts
      (FormSuccess (Just address), Just "add") -> do
         (title, episodes) <- getTitleAndEpisodes address

         id <- addPodcast $ mkPodcast (unpack title) (unpack address)
         sequence $ map (addEpisodes id) episodes
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
