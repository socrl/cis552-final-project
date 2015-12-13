{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module CommandLine where

-- | get user input
getParams :: IO (String, String, String)
getParams = do
  putStrLn "Provide a starting URL."
  url <- getLine
  putStrLn "Provide a string you want to search for."
  str <- getLine
  putStrLn "Provide the maximum number of pages to crawl."
  num <- getLine
  return (url, str, num)



