{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple

main :: IO ()
main = do
  response <- httpLBS "http://news.ycombinator.com"
  let status = getResponseStatusCode response
  if status == 200
    then do
      print "saving page to file"
      let body = getResponseBody response
      L.writeFile "page.html" body
    else
      print "request failed"
