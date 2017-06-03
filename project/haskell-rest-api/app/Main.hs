{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Web.Scotty
import Database.PostgreSQL.Simple

main :: IO ()
main = do
	conn <- connectPostgreSQL ("host='localhost' user='postgres' dbname='postgres' password='password'")
	scotty 8080 $ server conn
