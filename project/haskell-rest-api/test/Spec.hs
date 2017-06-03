{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Lib

import Database.PostgreSQL.Simple

main :: IO ()
main = do
  conn <- connectPostgreSQL ("host='localhost' user='postgres' dbname='postgres' password='password'")
  hspec $ do
    describe "Verify that sumNumber works fine with resetNumberQuery" $ do
      it "equals zero" $ do
        item <- liftIO (sumNumber conn (SumNum (Just 0)) resetNumberQuery)
        item `shouldBe` SumNum (Just 0)
    describe "Verify that sumNumber works fine with updateNumberQuery" $ do
      it "equals one" $ do
        item <- liftIO (sumNumber conn (SumNum (Just 1)) updateNumberQuery)
        item `shouldBe` SumNum (Just 1)
    describe "Verify that sumNumber works fine with selectNumberQuery" $ do
      it "equals one" $ do
        item <- liftIO (query_ conn selectNumberQuery :: IO [SumNum])
        item `shouldBe` [SumNum (Just 1)]
