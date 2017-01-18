{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GADTs                      #-}
-- |
-- Module      :  Web.Bot.Metrics
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Bot metrics utils.
--
module Web.Bot.Metrics where

import Database.Persist.TH
import Database.Persist
import Data.Text (Text)
import Web.Bot.User

share [mkPersist sqlSettings, mkMigrate "migrateMetrics"] [persistLowerCase|
UserStat
    ident      Text Unique
    messageOut Int
    messageIn  Int
    StatUser ident
    deriving Show

StoryStat
    name      Text Unique
    calls     Int
    StatStory name
    deriving Show
|]
