{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Catch (catch, throwM)
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

data MyException = MyException
    deriving Show

instance Exception MyException

main :: IO ()
main =
   runStderrLoggingT $ withSqliteConn ":memory:" $ \sqlbackend -> do
     runSqlConn (runMigration migrateAll) sqlbackend
     liftIO $ putStrLn ""
     void $ runSqlConn (insert $ Foo 1) sqlbackend
     liftIO $ putStrLn ""
     liftIO $ threadDelay (60 * 1000000)
     runSqlConn (getBy (UniqueBar 1) >> throwM MyException) sqlbackend
        `catch` \(e::MyException) -> liftIO $ putStrLn "Caught exception"
     liftIO $ putStrLn ""
     liftIO $ threadDelay (60 * 1000000)
     ret <- runSqlConn (getBy (UniqueBar 1)) sqlbackend
     liftIO $ print ret
