{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.PodcastSpec
    ( spec
    ) where

import TestImport

spec :: Spec
spec =
    withApp $
        it "asserts all episodes are present" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity
            let podcastAddress =
                    "http://localhost:3000/podcast/BBC%20Inside%20Science" :: Text
            get podcastAddress
            statusIs 200
            podcasts <- runDB $ selectList ([] :: [Filter Episode]) []
            htmlCount ".episode-item" $ length podcasts
