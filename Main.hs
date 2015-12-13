{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Main where

import Text.Read
import Downloader (sendReqs)
import PostProcessor
import System.IO
import System.Environment
import Control.Monad (unless)

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
formatOutput :: [PageData] -> String
formatOutput ((pg, _, _, snip):xs) = "***** " ++ pg ++ " *****\n" ++ snip ++
  "\n\n" ++ formatOutput xs
formatOutput [] = ""

main :: IO ()
main = do
  args <- getArgs
  out <-
    if not (null args) then openFile (head args) WriteMode else return stdout
  url <- getStParam
    "Provide a starting absolute URL, such as \
     \http://www.cis.upenn.edu/current-students."
  str <-
    getStParam "Provide space-delimited keywords you want to search for."
  num <- getIntParam "Provide the maximum number of pages to crawl."
  putStrLn "Crawling..."
  (rs, i) <- sendReqs url str num
  hPutStrLn out $ "Crawled " ++ show i ++ " pages."
  hPutStrLn out $ formatOutput $ rankPages rs (txtFormat str)
  unless (null args) $ hClose out
  putStrLn "Done."
