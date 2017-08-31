{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Dates
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Time
import Control.Monad
import Control.Applicative

import Lib

data VarcharIntDate = VarcharIntDate {
  vc :: Maybe String,
  i  :: Maybe Int,
  d  :: Maybe Date
  } deriving (Show)

instance FromRow VarcharIntDate where
  fromRow = VarcharIntDate <$> field <*> field <*> field

main :: IO ()
main = do


  conn <- connect defaultConnectInfo { connectDatabase = "testdb" }

  mapM_ print =<< ( query_ conn "select vc,i,d from varintdate" :: IO [VarcharIntDate] )

  someFunc
