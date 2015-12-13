{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Main where

import Text.Read
import Downloader (sendReqs)
import PostProcessor

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

-- | formatting for printing crawled pages and snippets
formatOutput :: [(String, String, Double, String)] -> IO ()
formatOutput ((pg, _, _, snip):xs) = do
  putStrLn $ "***** " ++ pg ++ " *****"
  putStrLn snip
  putStrLn ""
  formatOutput xs
formatOutput [] = do
  putStrLn ""

main :: IO ()
main = do
  url <- getStParam "Provide a starting URL."
  str <-
    getStParam "Provide whitespace-delimited keywords you want to search for."
  num <- getIntParam "Provide the maximum number of pages to crawl."
  putStrLn "Crawling..."
  (rs, i) <- sendReqs url str num
  putStrLn $ "Crawled " ++ show i ++ " pages."
  formatOutput $ rankPages rs (txtFormat str)
  return ()
