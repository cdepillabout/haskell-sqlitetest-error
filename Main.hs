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
import Database.Persist (getBy, insert)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Database.Persist.Sqlite (runMigration, runSqlConn, withSqliteConn)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Foo
    bar Int
    UniqueBar bar

    deriving Show
|]

main :: IO ()
main =
   runStderrLoggingT $ withSqliteConn ":memory:" $ \sqlbackend -> do
     runSqlConn (runMigration migrateAll) sqlbackend
     void $ runSqlConn (insert $ Foo 1) sqlbackend
     liftIO $ threadDelay (60 * 1000000)
     ret <- runSqlConn (getBy $ UniqueBar 1) sqlbackend
     liftIO $ print ret
