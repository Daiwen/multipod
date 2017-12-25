{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import


getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
        addScriptRemote "https://unpkg.com/react@16.2.0/umd/react.production.min.js"
        addScriptRemote "https://unpkg.com/react-dom@16/umd/react-dom.production.min.js"
        setTitle "Synced podcasts"
        $(widgetFile "homepage")
