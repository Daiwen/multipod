{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Podcast where

import Import
import Text.Julius

getPodcastR :: String -> Handler Html
getPodcastR podName =
    defaultLayout $ do
        setTitle (toHtml podName)
        let name = rawJS podName
        $(widgetFile "podcast")

getEpisodesR :: String -> Handler Value
getEpisodesR name = do
    pod <- runDB $ getPodcastFromName name
    case pod of
        Just (Entity idPod _) -> do
            podcasts <- runDB $ getEpisodesFromPodcastId idPod
            returnJson $ map episodeData podcasts
        Nothing -> returnJson ([] :: [Value])
  where
    episodeData (Entity key episode) =
        let idEp = show key
            nameEp = episodeName episode
            url = episodeUrl episode
            isRead = episodeIsRead episode
        in object $
           ["id" .= idEp, "url" .= url, "title" .= nameEp, "isRead" .= isRead]
