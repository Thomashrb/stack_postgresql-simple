{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Int(Int64)
import Data.Dates
-- import Data.Text as Text
import Data.Tuple.Select as Tuple
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Time
import Control.Monad
import Control.Applicative

import Data.Time.Clock (UTCTime)

import Lib

data VarcharIntDate = VarcharIntDate {
  vc :: Maybe String,
  i  :: Maybe Int,
  d  :: Maybe Date
  } deriving (Show)

data VarcharInt = VarcharInt {
  avc :: Maybe String,
  ai  :: Maybe Int
  } deriving (Show)

instance FromRow VarcharIntDate where
  fromRow = VarcharIntDate <$> field <*> field <*> field

instance ToRow VarcharInt where
  toRow c = [toField (avc c), toField (ai c)]

instance ToRow VarcharIntDate where
  toRow c = [toField (vc c), toField (i c), toField (d c)]

addVarcharInt :: Connection -> VarcharInt -> IO Int64
addVarcharInt c vci = execute c "INSERT INTO varintdate (vc,i) VALUES (?,?)" vci

-- addVarcharIntDate :: Connection -> VarcharIntDate -> IO Int64
-- addVarcharIntDate c varcharintdate = execute c "INSERT INTO varintdate (vc,i,d) VALUES (?,?,?)" varcharintdate


main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "testdb" }

--  mapM_ print =<< ( query_ conn "select vc,i,d from varintdate" :: IO [VarcharIntDate] )
--  mapM_ (putStrLn . show) =<< (query_ conn "select vc,i,d from varintdate" :: IO [VarcharIntDate])
--  print =<< ( query_ conn "select vc,i,d from varintdate" :: IO [VarcharIntDate] )


  res <- (query_ conn "select vc,i,d from varintdate" :: IO [VarcharIntDate] )
  print (mapM vc res)
  print (mapM i  res)
  print (d (res!!0)) -- print d of the 0-th row
  print (mapM d  res)

  execute conn "insert into varintdate (vc, i, d) values (?, ?, ?)" $ VarcharIntDate (vc (res!!0)) (i (res!!0)) (d (res!!0))

  mapM_ print =<< ( query_ conn "select vc,i,d from varintdate" :: IO [VarcharIntDate] )

  print (length res)


--  let a = "abc"
--  let b = 5
--  let d = '1970-01-01' :: Date
--  execute conn "insert into varintdate (vc, i) values (?, ?)" $ VarcharInt (Just a) (Just b)
--  execute conn "insert into varintdate (a, b, d) values (?, ?, ?)" $ VarcharIntDate (Just a) (Just b)

--  let foo = "fffffffff" :: Maybe String
--  let bar = 99999       :: Maybe Int
--  addVarcharInt conn VarcharInt {avc=foo, ai=bar }

  someFunc
