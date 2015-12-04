{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Main where

import Text.Read
import Downloader (Result, sendReqs)

startCrawl :: String -> String -> Int -> Either [Result] String
startCrawl = undefined

{-
1. tell CL to get starting URL, search string, num pages to crawl from user

2. give downloader URL, search string, num pages - downloader will schedule requests (starting with robots.txt)

3. downloader returns (URL, snippet)

4. tell CL to print output



-}

-- | given a message to print, print it and get a line of string input
getStParam :: String -> IO String
getStParam s = do
  putStrLn s
  getLine

-- | given a message to print print it and get a line of integer input,
-- repeating until valid input is given
getIntParam :: String -> IO Int
getIntParam s = do
  putStrLn s
  st <- getLine
  case readMaybe st :: Maybe Int of
    Just i -> return i
    Nothing -> getIntParam "You provided a non-integer input. Try again."

main :: IO ()
main = do
  url <- getStParam "Provide a starting URL."
  str <- getStParam "Provide a string you want to search for."
  num <- getIntParam "Provide the maximum number of pages to crawl."
  (rs, i) <- sendReqs url str num
  putStrLn $ show rs
  putStrLn $ show i
  return ()
