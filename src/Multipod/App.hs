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
import Data.Text
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


handleResult res = case res of
    FormSuccess address -> do

      htmlString <- requestBody (unpack address)
      let contents = parseXML htmlString
      title <- getPodcastTitle contents
      addPodcast $ mkPodcast (unpack title) (unpack address)
    _ -> return ()

postHomeR :: Handler Html
postHomeR = do
    ((res, widget), enctype) <- runFormPost form

    handleResult res

    podcasts <- runDB $ selectList [] []
    displayHome podcasts widget enctype

extractInfos podcast = case podcast of
    Just (Entity _ p) -> do

      let address = podcastUrl p
      htmlString <- requestBody address
      let contents = parseXML htmlString

      getEpisodeTitle contents

    Nothing -> return [""]

getPodcastR :: String -> Handler Html
getPodcastR  name = do
    podcastAddress <- runDB $ getPodcastFromName name

    infos <- extractInfos podcastAddress

    defaultLayout $ do
        setTitle "Synced podcasts"
        [whamlet|
            <ul>
                $forall title <- infos
                    <li> #{title}
        |]

launchApp = do
  dataApp <- mkDataApp
  warp 3000 App {dataApp = dataApp}
