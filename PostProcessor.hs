{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module PostProcessor where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Utils
import Data.Char
import Data.List
import Data.Ord

-- | Given a list of words and a list of keywords, find the indices at which
-- each keyword occurs
findWords :: [String] -> [String] -> Map String [Int]
findWords = f Map.empty 0 where
  f m i (w:ws) ks =
    if elem w ks then
      case Map.lookup w m of
        Just l -> f (Map.insert w (l ++ [i]) m) (i + 1) ws ks
        Nothing -> f (Map.insert w [i] m) (i + 1) ws ks
    else
      f m (i + 1) ws ks
  f m _ _      _  = m

-- | convert a string to lowercase, wherever possible
strLower :: String -> String
strLower = foldr (\ c st -> (toLower c):st) ""

-- | given a String, trim non-alphanumeric character from the front and the back
trimNonAlpha :: String -> String
trimNonAlpha s = reverse $ f $ reverse $ f s where
  f = dropWhile (not . isAlphaNum)

-- | rank the texts by their relevance to the keywords, in descending order
rankPages :: [(String, String)] -> [String] -> [(String, String, Int)]
rankPages pgs keys = sortBy (flip $ comparing (\ (_, _, x) -> x))
  (map (getPgValue (keyFormat keys)) pgs)

-- | given the desired keywords and a page, get an integer representing its
-- worth
getPgValue :: [String] -> (String, String) -> (String, String, Int)
getPgValue keys (url, txt) = (url, txt, val) where
  m = findWords (txtFormat txt) keys
  val = (numOccur m) * (numDistinct m) `div` if avgD > 0 then avgD else 1
  avgD = avgDist m

-- | given a map of occurrences, determine the number of times any keyword
-- occurred
numOccur :: Map String [Int] -> Int
numOccur m = foldr (\ x accu -> length x + accu) 0 (Map.elems m)

-- | given a map of occurrences, determine the number of distinct keywords that
-- occurred
numDistinct :: Map String [Int] -> Int
numDistinct = length . Map.keys

-- | given a map of occurrences, determine the average number of words between
-- occurrences of keywords
avgDist :: Map String [Int] -> Int
avgDist m = if x > 0 then (f $ sort $ concat $ Map.elems m) `div` x else 0 where
  f (a:s@(b:_)) = b - a + (f s)
  f _           = 0
  x = numOccur m

-- | formatting the text from the webpage: split text on whitespace, trim
-- non-alphanumeric characters, convert to lowercase, remove empty strings
txtFormat :: String -> [String]
txtFormat doc =
  filter (not . null) (map (strLower . trimNonAlpha) (splitWs doc))

-- | formatting the keywords: trim non-alphanumeric characters, convert to
-- lowercase, remove empty strings
keyFormat :: [String] -> [String]
keyFormat k =
  filter (not . null) (foldr (\ s st -> (strLower $ trimNonAlpha s):st) [] k)
