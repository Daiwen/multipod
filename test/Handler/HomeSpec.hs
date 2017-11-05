{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HomeSpec (spec) where

import TestImport
import Data.Aeson

spec :: Spec
spec = withApp $ do

    describe "Homepage" $ do
        it "asserts no access to my-account for anonymous users" $ do
            get HomeR
            statusIs 403

        it "asserts access to my-account for authenticated users" $ do
            userEntity <- createUser "foo"
            authenticateAs userEntity

            get HomeR
            statusIs 200

            htmlAllContain ".add-button" "Add"
            htmlAllContain ".update-button" "Update"

        it "asserts all podcast are present" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity

            get HomeR
            statusIs 200
            podcasts <- runDB $ selectList ([] :: [Filter Podcast]) []
            htmlCount ".podcast-item" $ length podcasts

        it "asserts adding a valid podcast increases the number" $ do
            userEntity <- createUser "bar"
            authenticateAs userEntity

            get HomeR
            statusIs 200
            beforePodcasts <- runDB $ selectList ([] :: [Filter Podcast]) []
            let bnb = length beforePodcasts

            request $ do
              setMethod "POST"
              setUrl HomeR
              addToken
              addPostParam "action" "add"
              byLabel "RSS stream address." "http://podcasts.files.bbci.co.uk/b036f7w2.rss"

            statusIs 200

            afterPodcasts <- runDB $ selectList ([] :: [Filter Podcast]) []
            let anb = length afterPodcasts
            assertEq "Should be equal" anb $ bnb + 1
            htmlCount ".podcast-item" $ bnb + 1
