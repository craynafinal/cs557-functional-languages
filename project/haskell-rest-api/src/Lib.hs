{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Web.Scotty
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, object, encode)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

server :: Connection -> ScottyM()
server conn = do
  post "/SumAPI/addNumber" $ do
    item <- jsonData :: ActionM SumNum
    newItem <- liftIO (sumNumber conn item updateNumberQuery)
    json newItem
  get "/SumAPI/showNumber" $ do
    items <- liftIO (query_ conn selectNumberQuery :: IO [SumNum])
    json items
  get "/SumAPI/resetNumber" $ do
    item <- liftIO (sumNumber conn (SumNum (Just 0)) resetNumberQuery)
    json item

updateNumberQuery = "UPDATE sumNumber SET number = number + ? WHERE id = 1 returning number"
resetNumberQuery = "UPDATE sumNumber SET number = ? WHERE id = 1 returning number"
selectNumberQuery = "SELECT number FROM sumNumber"

sumNumber :: Connection -> SumNum -> Query -> IO SumNum
sumNumber conn item qString = do
  [Only number] <- query conn qString item
  return item { number = number }

data SumNum = SumNum { number :: Maybe Int } deriving (Show, Generic)

instance FromRow SumNum where
  fromRow = SumNum <$> field

instance ToRow SumNum where
  toRow c = [toField $ number c]

instance ToJSON SumNum
instance FromJSON SumNum
