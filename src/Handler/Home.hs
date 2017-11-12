{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import

import Text.XML.Light.Input as XML
import Text.XML.Light.Types as XML

form :: Form (Maybe Text)
form = renderDivs $ aopt textField textSettings Nothing
  where
    textSettings =
        FieldSettings
        { fsLabel = "RSS stream address."
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = []
        }

displayHome :: [Entity Podcast] -> Widget -> Enctype -> Handler Html
displayHome podcasts widget enctype = do
    defaultLayout $ do
        setTitle "Synced podcasts"
        $(widgetFile "homepage")

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    podcasts <- runDB getAllPodcast
    (widget, enctype) <- generateFormPost form
    displayHome podcasts widget enctype

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
    podcasts <- runDB $ getAllPodcast
    let idurls =
            map (\(Entity idPod p) -> (idPod, pack $ podcastUrl p)) podcasts
    _ <-
        sequence $
        map
            (\(idPod, url) -> do
                 (_, episodes) <- getTitleAndEpisodes url
                 sequence $ map (addEpisodes idPod) episodes)
            idurls
    return ()

handleHomeResult :: FormResult (Maybe Text) -> Handler ()
handleHomeResult res = do
    action <- lookupPostParam "action"
    case (res, action) of
        (_, Just "update") -> updatePodcasts
        (FormSuccess (Just address), Just "add") -> do
            (title, episodes) <- getTitleAndEpisodes address
            idPod <-
                runDB $ addPodcast $ Podcast (unpack title) (unpack address)
            _ <- sequence $ map (addEpisodes idPod) episodes
            return ()
        _ -> return ()

postHomeR :: Handler Html
postHomeR = do
    ((res, widget), enctype) <- runFormPost form
    handleHomeResult res
    podcasts <- runDB $ getAllPodcast
    displayHome podcasts widget enctype
