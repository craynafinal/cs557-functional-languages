{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Web.Scotty
import Lib

server :: ScottyM()
server = do
	get "/api" $ do
		text "works!"

main :: IO ()
main = do
	scotty 8080 server
