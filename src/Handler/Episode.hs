{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Episode where

import Import
import BasicPrelude hiding (map)
import Data.Text hiding (map)


touchReadsDB :: Bool -> Handler ()
touchReadsDB isRead = do
    idEpM <- lookupGetParam "ids"
    case idEpM of
        Just idEp ->
            let ids = split ((==) ',') idEp in
            runDB $ updateEpisodesIsRead isRead $ map read ids
        Nothing -> return ()
    return ()


postReadEpisodesR :: Handler ()
postReadEpisodesR = touchReadsDB True

postUnreadEpisodesR :: Handler ()
postUnreadEpisodesR = touchReadsDB False
