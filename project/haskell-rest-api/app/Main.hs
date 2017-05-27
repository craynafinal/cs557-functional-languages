{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Lib
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

server :: Connection -> ScottyM()
server conn = do
	get "/api" $ do
		text "works!!"
	get "/api2" $ do
		text "works2!!"
	get "/checklists" $ do
		checklists <- liftIO (query_ conn "select id, title from checklists" :: IO [Checklist])
		checkWithItems <- liftIO (mapM (setArray conn) checklists)
		json checkWithItems
	post "/checklistitems" $ do
		item <- jsonData :: ActionM ChecklistItem
		newItem <- liftIO (insertChecklist conn item)
		json newItem
	post "/SumAPI/test" $ do
		item <- jsonData :: ActionM SumNum
		newItem <- liftIO (sumNumber conn item)
		json newItem
	get "/SumAPI/test2" $ do
		items <- liftIO (query_ conn selectNumberQuery :: IO [SumNum])
		json items
	get "/SumAPI/:word" $ do
		number <- param "word"
		item <- "{ number: " ++ number ++ " }"
		newItem <- liftIO (sumNumber conn item)
		json newItem
--		html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

updateNumberQuery = "UPDATE sumNumber SET number = ? WHERE id = 1 returning number"

sumNumber :: Connection -> SumNum -> IO SumNum
sumNumber conn item = do
	[Only number] <- query conn updateNumberQuery item
	return item { number = number }

selectNumberQuery = "SELECT number from sumNumber"

data SumNum = SumNum { number :: Maybe Int } deriving (Show, Generic)

instance FromRow SumNum where
	fromRow = SumNum <$> field

instance ToRow SumNum where
	toRow c = [toField $ number c]

instance ToJSON SumNum
instance FromJSON SumNum

main :: IO ()
main = do
	conn <- connectPostgreSQL ("host='localhost' user='postgres' dbname='postgres' password='password'")
	scotty 8080 $ server conn



-- example codes below

selectChecklistQuery = "select id, name, finished, checklist from checklistitems where checklist = (?)"
insertItemsQuery = "insert into checklistitems (name, finished, checklist) values (?, ?, ?) returning id"

setArray :: Connection -> Checklist -> IO Checklist
setArray conn check = do
    items <- liftIO (query conn selectChecklistQuery (Only $ checklistId check) :: IO [ChecklistItem])
    return check { checklistItems = items }

insertChecklist :: Connection -> ChecklistItem -> IO ChecklistItem
insertChecklist conn item = do
    [Only id] <- query conn insertItemsQuery item
    return item { checklistItemId = id }


data Checklist = Checklist { checklistId :: Maybe Int,
    title :: String,
    checklistItems :: [ChecklistItem]} deriving (Show, Generic)

instance FromRow Checklist where
    fromRow = Checklist <$> field <*> field <*> pure []
instance ToRow Checklist where
    toRow c = [toField $ title c]
instance ToJSON Checklist
instance FromJSON Checklist

data ChecklistItem = ChecklistItem { checklistItemId :: Maybe Int,
    itemText :: String,
    finished :: Bool,
    checklist :: Int } deriving (Show, Generic)

instance FromRow ChecklistItem where
    fromRow = ChecklistItem <$> field <*> field <*> field <*> field
instance ToRow ChecklistItem where
    toRow i = [toField $ itemText i, toField $ finished i, toField $ checklist i]
instance ToJSON ChecklistItem
instance FromJSON ChecklistItem
