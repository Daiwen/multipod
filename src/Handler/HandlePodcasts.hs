{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.HandlePodcasts where

import Import

import Text.XML.Light.Input as XML
import Text.XML.Light.Types as XML



getTitleAndEpisodes :: Text -> Handler (Text, [XML.Element])
getTitleAndEpisodes address = do
    htmlString <- getBody (unpack address)
    let contents = parseXML htmlString
    title <- getPodcastTitle contents
    episodes <- getPodcastEpisodes contents
    return (title, episodes)


addEpisodes :: Key Podcast -> XML.Element -> Handler ()
addEpisodes idPod episode = do
    title <- getEpisodeTitle episode
    url <- getEpisodeUrl episode
    runDB $ addEpisode $ Episode (unpack title) (unpack url) idPod False


updatePodcasts :: Handler ()
updatePodcasts = do
    podcasts <- runDB getAllPodcast
    let idurls =
            map (\(Entity idPod p) -> (idPod, pack $ podcastUrl p)) podcasts
    mapM_
        (\(idPod, url) -> do
             (_, episodes) <- getTitleAndEpisodes url
             mapM_ (addEpisodes idPod) episodes)
        idurls
    return ()


postUpdatePodcastsR :: Handler ()
postUpdatePodcastsR = do
    updatePodcasts
    return ()


handleAddPodcast :: Handler ()
handleAddPodcast = do
    addressM <- lookupGetParam "address"
    case addressM of
        Just address -> do
            (title, episodes) <- getTitleAndEpisodes address
            idPod <-
                runDB $ addPodcast $ Podcast (unpack title) (unpack address)
            _ <- mapM_ (addEpisodes idPod) episodes
            return ()
        Nothing -> return ()


postAddPodcastR :: Handler ()
postAddPodcastR = do
    handleAddPodcast
    return ()
