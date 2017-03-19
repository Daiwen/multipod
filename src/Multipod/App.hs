{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Multipod.App
  ( launchApp
  ) where

import Control.Applicative
import Data.Text hiding (map, zip)
import Database.Persist.Sql
import Text.Blaze
import Text.Blaze.Html.Renderer.String
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

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET POST
/podcast/#String PodcastR GET POST
/readEpisode/ ReadEpisodeR POST
/removePodcast/ RemovePodcastR POST
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

markAsReadJ =
  toWidgetBody
    [julius|
           function markAsRead(id) {
              var xmlhttp = new XMLHttpRequest();
              xmlhttp.onreadystatechange = function() {
                if (this.readyState == 4 && this.status == 200) {
                  document.getElementById(id).outerHTML = "";
                }
              };
              var params = "?id=" + id;
              xmlhttp.open("POST", "@{ReadEpisodeR}" + params);
              xmlhttp.send(null);
           }
    |]

removePodcastJ =
  toWidgetBody
    [julius|
           function removePodcast(name) {
              var xmlhttp = new XMLHttpRequest();
              xmlhttp.onreadystatechange = function() {
                if (this.readyState == 4 && this.status == 200) {
                  window.open("@{HomeR}","_self");
                }
              };
              var params = "?name=" + name;
              xmlhttp.open("POST", "@{RemovePodcastR}" + params);
              xmlhttp.send(null);
           }
    |]


displayPodcast name infos widget enctype = do
  defaultLayout $ do
    setTitle (toHtml name)

    [whamlet|
            <button onClick=removePodcast('#{name}')>Remove
            <form method=post action=@{PodcastR name} enctype=#{enctype}>
                <button type=submit name=action value=selectall>Select all
                ^{widget}
                <button type=submit name=action value=read>Mark as read
                <button type=submit name=action value=unread>Mark as unread
        |]
    markAsReadJ
    removePodcastJ
  
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
  url <- getEpisodeUrl episode
  addEpisode $ mkEpisode (unpack title) (unpack url) id

updatePodcasts :: Handler ()
updatePodcasts = do
  podcasts <- runDB $ getAllPodcast
  let idurls = map (\(Entity id p) -> (id, pack $ podcastUrl p)) podcasts
  sequence $
    map
      (\(id, url) -> do
         (_, episodes) <- getTitleAndEpisodes url
         sequence $ map (addEpisodes id) episodes)
      idurls
  return ()

handleHomeResult :: FormResult (Maybe Text) -> Handler ()
handleHomeResult res = do
  action <- lookupPostParam "action"
  case (res, action) of
    (                         _, Just "update") -> updatePodcasts
    (FormSuccess (Just address), Just    "add") -> do
      (title, episodes) <- getTitleAndEpisodes address
      id <- addPodcast $ mkPodcast (unpack title) (unpack address)
      sequence $ map (addEpisodes id) episodes
      return ()
    _ -> return ()

postHomeR :: Handler Html
postHomeR = do
  ((res, widget), enctype) <- runFormPost form
  handleHomeResult res
  podcasts <- runDB $ getAllPodcast
  displayHome podcasts widget enctype

extractInfos :: Maybe (Entity Podcast) -> Handler [(Key Episode, Text, Text, Bool)]
extractInfos podcast =
  case podcast of
    Just (Entity id _) -> do
      episodes <- runDB $ getEpisodesFromPodcastId id
      return $
        map
          (\(Entity id e) -> (id, pack $ episodeName e, pack $ episodeUrl e, episodeIsRead e))
          episodes
    Nothing -> return []


episodeListField :: Bool -> [(Key Episode, Text, Text, Bool)] -> Field Handler [Key Episode]
episodeListField b titles =
  Field
  { fieldParse = \rawVals _fileVals -> return $ Right $ Just $ map (read . unpack) rawVals
  , fieldView =
      \idAttr nameAttr otherAttrs eResult isReq ->
        let showIsRead b = if b then "R"::Text else "U" in
        [whamlet|
            <ul>
               $forall (id, title, url, isRead) <- titles
                  <li>
                     #{title}
                     <input name=#{nameAttr} *{otherAttrs} value=#{show id} type=checkbox :b:checked>
                     <br>
                     $if not isRead
                        <audio id=#{show id} controls onended="markAsRead('#{show id}')"}>
                           <source src=#{url} type="audio/mpeg">
                           <a href=#{url}>
                              #{url}
        |]
  , fieldEnctype = UrlEncoded
  }


podcastPageR :: Bool -> String -> Handler Html
podcastPageR b name =  do
  podcastAddress <- runDB $ getPodcastFromName name
  infos <- extractInfos podcastAddress
  (widget, enctype) <- generateFormPost $ renderDivs $ aopt (episodeListField b infos) "" Nothing
  displayPodcast name infos widget enctype



getPodcastR :: String -> Handler Html
getPodcastR = podcastPageR False

handlePodcastResult :: FormResult (Maybe [Key Episode]) -> Handler Bool
handlePodcastResult res = do
  action <- lookupPostParam "action"
  let value = case action of
                Just   "read"    -> Just True
                Just "unread"    -> Just False
                Just "selectall" -> Nothing
  case (res, value) of
    (                     _,    Nothing) -> return True
    (FormSuccess (Just ids), Just value) -> do sequence $ map (updateEpisodeIsRead value) ids; return False
    _ -> return False


postPodcastR :: String -> Handler Html
postPodcastR name = do
  podcastAddress <- runDB $ getPodcastFromName name
  infos <- extractInfos podcastAddress
  ((res, _), _) <- runFormPost $ renderDivs $ aopt (episodeListField False infos) "" Nothing
  b <- handlePodcastResult res
  podcastPageR b name


handleReadEpisode :: Handler ()
handleReadEpisode = do
    id <- lookupGetParam "id"
    case id of
      Just id -> updateEpisodeIsRead True $ read $ unpack id
      Nothing -> return ()


postReadEpisodeR :: Handler ()
postReadEpisodeR = do
    handleReadEpisode
    return ()


handleRemovePodcast :: Handler ()
handleRemovePodcast = do
    name <- lookupGetParam "name"
    case name of
      Just name -> removePodcast $ unpack name
      Nothing   -> return ()


postRemovePodcastR :: Handler ()
postRemovePodcastR = do
    handleRemovePodcast
    return ()


launchApp = do
  dataApp <- mkDataApp
  warp 3000 App {dataApp = dataApp}
