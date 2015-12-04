{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Main where

import Text.Read
import Downloader (Result)

startCrawl :: String -> String -> Int -> Either [Result] String
startCrawl = undefined

{-
1. tell CL to get starting URL, search string, num pages to crawl from user

2. give downloader URL, search string, num pages - downloader will schedule requests (starting with robots.txt)

3. downloader returns (URL, snippet)

4. tell CL to print output



-}

main :: IO ()
main = do
  return ()