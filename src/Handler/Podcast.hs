{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Podcast where

import Import

import BasicPrelude hiding (map, mapM_)

displayPodcast :: String -> Widget -> Enctype -> Handler Html
displayPodcast name widget enctype =
    defaultLayout $ do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
        setTitle (toHtml name)
        $(widgetFile "podcast")

extractInfos ::
       Maybe (Entity Podcast) -> Handler [(Key Episode, Text, Text, Bool)]
extractInfos podcast =
    case podcast of
        Just (Entity idPod _) -> do
            episodes <- runDB $ getEpisodesFromPodcastId idPod
            return $
                map
                    (\(Entity idEp e) ->
                         ( idEp
                         , pack $ episodeName e
                         , pack $ episodeUrl e
                         , episodeIsRead e))
                    episodes
        Nothing -> return []

episodeListField ::
       Bool -> [(Key Episode, Text, Text, Bool)] -> Field Handler [Key Episode]
episodeListField b titles =
    Field
    { fieldParse =
          \rawVals _fileVals -> return $ Right $ Just $ map read rawVals
    , fieldView = \_ nameAttr otherAttrs _ _ -> $(widgetFile "episode-list")
    , fieldEnctype = UrlEncoded
    }

podcastPageR :: Bool -> String -> Handler Html
podcastPageR b name = do
    podcastAddress <- runDB $ getPodcastFromName name
    infos <- extractInfos podcastAddress
    (widget, enctype) <-
        generateFormPost $
        renderDivs $ aopt (episodeListField b infos) "" Nothing
    displayPodcast name widget enctype

getPodcastR :: String -> Handler Html
getPodcastR = podcastPageR False

postPodcastR :: String -> Handler Html
postPodcastR = podcastPageR True
