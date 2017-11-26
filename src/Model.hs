{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

data DataError =
    AlreadySync
    deriving (Eq)

instance Show DataError where
    show AlreadySync = "Podcast already subscribed to."

instance Exception DataError

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

getAllPodcast :: MonadIO m => ReaderT SqlBackend m [Entity Podcast]
getAllPodcast = selectList [] []

getPodcastFromName ::
       MonadIO m => String -> ReaderT SqlBackend m (Maybe (Entity Podcast))
getPodcastFromName name = selectFirst [PodcastName ==. name] []

getEpisodesFromPodcastId ::
       MonadIO m => PodcastId -> ReaderT SqlBackend m [Entity Episode]
getEpisodesFromPodcastId idPod = selectList [EpisodePodcastId ==. idPod] []

addEpisode :: MonadIO m => Episode -> ReaderT SqlBackend m ()
addEpisode episode = do
    sameUrl <- selectList [EpisodeUrl ==. episodeUrl episode] []
    case sameUrl of
        [] -> do
            _ <- insert episode
            return ()
        _ -> return ()

addPodcast ::
       (MonadIO m, MonadThrow m) => Podcast -> ReaderT SqlBackend m PodcastId
addPodcast podcast = do
    sameUrl <- selectList [PodcastUrl ==. podcastUrl podcast] []
    case sameUrl of
        [] -> insert podcast
        _ -> throwM AlreadySync

updateEpisodeIsRead :: MonadIO m => Bool -> EpisodeId -> ReaderT SqlBackend m ()
updateEpisodeIsRead val idEp = update idEp [EpisodeIsRead =. val]

removePodcast :: MonadIO m => String -> ReaderT SqlBackend m ()
removePodcast name = do
    p <- selectFirst [PodcastName ==. name] []
    case p of
        Just (Entity idPod _) -> do
            deleteWhere [EpisodePodcastId ==. idPod]
            delete idPod
        Nothing -> return ()
