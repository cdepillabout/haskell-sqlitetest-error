{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Sqlite (insert, getBy, runMigration, runSqlPool, withSqlitePool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Foo
    bar Int
    UniqueBar bar

    deriving Show
|]

main :: IO ()
main =
   runStderrLoggingT $ withSqlitePool ":memory:" 1 $ \pool -> do
     runSqlPool (runMigration migrateAll) pool
     void $ runSqlPool (insert $ Foo 1) pool
     liftIO $ threadDelay (60 * 1000000)
     ret <- runSqlPool (getBy $ UniqueBar 1) pool
     liftIO $ print ret
