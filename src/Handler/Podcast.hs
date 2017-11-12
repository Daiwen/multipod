{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Podcast where

import Import

import BasicPrelude hiding (map)

displayPodcast :: String -> Widget -> Enctype -> Handler Html
displayPodcast name widget enctype = do
    defaultLayout $ do
        setTitle (toHtml name)
        $(widgetFile "podcast")
        $(widgetFile "mark-as-read")
        $(widgetFile "remove-podcast")

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

handlePodcastResult :: FormResult (Maybe [Key Episode]) -> Handler Bool
handlePodcastResult res = do
    action <- lookupPostParam "action"
    let actionValue =
            case action of
                Just "read" -> Just True
                Just "unread" -> Just False
                Just "selectall" -> Nothing
                _ -> Nothing
    case (res, actionValue) of
        (_, Nothing) -> return True
        (FormSuccess (Just ids), Just value) -> do
            _ <- sequence $ map (runDB . (updateEpisodeIsRead value)) ids
            return False
        _ -> return False

postPodcastR :: String -> Handler Html
postPodcastR name = do
    podcastAddress <- runDB $ getPodcastFromName name
    infos <- extractInfos podcastAddress
    ((res, _), _) <-
        runFormPost $
        renderDivs $ aopt (episodeListField False infos) "" Nothing
    b <- handlePodcastResult res
    podcastPageR b name
