{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GADTs                      #-}
-- |
-- Module      :  Web.Bot.User
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Common used user model.
--
module Web.Bot.User where

import Database.Persist.TH
import Database.Persist
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    chat Int
    name Text
    deriving Show
|]
