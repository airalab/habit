{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
-- |
-- Module      :  Web.Bot.Persist
-- Copyright   :  Alexander Krupenkin 2017
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Bot storage utils.
--
module Web.Bot.Persist (
    Persist(..)
  , Connection(..)
  , ConnectInfo(..)
  , runDB
  , module Database.Persist
  ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text (Text)

import Database.Persist.Postgresql (withPostgresqlConn,
                                    ConnectionString)
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.MySQL (withMySQLConn,
                               ConnectInfo(..))
import Database.Persist.Sql (SqlBackend,
                             runSqlPersistM, runMigration)
import Database.Persist

import Web.Bot.Platform
import Web.Bot.Log

-- Models
import Web.Bot.Metrics
import Web.Bot.User

data Connection = Postgresql ConnectionString
                | Sqlite Text
                | MySQL ConnectInfo
  deriving (Eq, Show)

class Platform a => Persist a where
    persist :: Bot a Connection

runDB :: Persist a
      => ReaderT SqlBackend (NoLoggingT (ResourceT IO)) b
      -> Bot a b
runDB ma = do
    db <- persist
    $logDebugS "Persist" (T.append "Connection: " $ T.pack (show db))

    let runConnection = case db of
            Postgresql conn -> withPostgresqlConn conn
            Sqlite conn     -> withSqliteConn conn
            MySQL conn      -> withMySQLConn conn

    runConnection $ \backend ->
        liftIO $ flip runSqlPersistM backend $
            runMigration (migrateMetrics >> migrateUser) >> ma
