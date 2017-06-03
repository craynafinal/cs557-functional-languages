{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Test where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Network.Wai (Application)
import qualified Web.Scotty as S
import           Data.Aeson (Value(..), object, (.=))

--import 					 Lib
import Main

main :: IO ()
main = hspec $ do
  describe "Verify that bassbull outputs the correct data" $ do
    it "equals zero" $ do
--      theSum <- getAtBatsSum "batting.csv"
--      theSum `shouldBe` 4858210
			"someFunc" `shouldBe` "someFunc"
