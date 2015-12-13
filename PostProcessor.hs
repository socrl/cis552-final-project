{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module PostProcessor where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Utils
import Data.Char
import Data.List
import Data.Ord
import Downloader (Result)

type OccurMap = Map String [Int]
type PageData = (String, String, Double, String)

-- | Given a list of words and a list of keywords, find the indices at which
-- each keyword occurs
findWords :: [String] -> [String] -> OccurMap
findWords = f Map.empty 0 where
  f m i (w:ws) ks =
    if elem w ks then
      case Map.lookup w m of
        Just l -> f (Map.insert w (l ++ [i]) m) (i + 1) ws ks
        Nothing -> f (Map.insert w [i] m) (i + 1) ws ks
    else
      f m (i + 1) ws ks
  f m _ _      _  = m

-- | The number of words a snippet should contain
snipSize :: Int
snipSize = 30

-- | Given a map, total number of keys, get the end index of a good snippet
getSnippet :: OccurMap -> Int
getSnippet m = fst $ g $ f (pairs m) 0 Map.empty where
  -- | while iterating, maintain the following: endIndex, num keywords in
  -- window, map
  f ((key, i):xs) k wind = (i, newCount, newMap) : f xs newCount newMap where
    newMap = case Map.lookup key shortened of
      Just l  -> Map.insert key (l ++ [i]) shortened
      Nothing -> Map.insert key [i] shortened
    (shortened, count) = dropSmall wind (i - snipSize)
    newCount = k - count + 1
  f []            _ _    = []
  g l = maximumBy (comparing snd) $ map val l
  val (endIndex, numOc, mapCur) = (endIndex, numOc * (numDistinct mapCur))

-- | drop the values outside the snippet we are currently examining
dropSmall :: OccurMap -> Int -> (OccurMap, Int)
dropSmall m i = (Map.fromList b, count)
  where
    a = Map.toList m
    b = filter (not . null . snd) $ map f a
    count = length a - length b
    f c@(str, l) = if h < i then (str, tail l) else c where
      h = head l

-- | given a map of occurrences, create a tuple where the first is the key and
-- the second is an index where the key occurs, sorted based on the second
pairs :: OccurMap -> [(String, Int)]
pairs m = sortBy (comparing snd) x where
  x = foldr (\ (key, l) accu -> (f key l) ++ accu) [] (Map.toList m)
  f k inds = foldr (\ i is -> (k, i):is) [] inds

-- | convert a string to lowercase, wherever possible
strLower :: String -> String
strLower = foldr (\ c st -> (toLower c):st) ""

-- | given a String, trim non-alphanumeric character from the front and the back
trimNonAlpha :: String -> String
trimNonAlpha s = reverse $ f $ reverse $ f s where
  f = dropWhile (not . isAlphaNum)

-- | rank the texts in descending order by their relevance to the keywords and
-- give a good snippet
rankPages :: [Result] -> [String] -> [PageData]
rankPages pgs keys = sortBy (flip $ comparing (\ (_, _, x, _) -> x))
  (map (getPgValue (keyFormat keys)) pgs)

-- | given the desired keywords and a page, get an integer representing its
-- worth and get a good snippet
getPgValue :: [String] -> Result -> PageData
getPgValue keys (url, txt) = (url, txt, val, snip) where
  m = findWords doc keys
  val = valFormula (numOccur m) (numDistinct m) (avgDist m) (length doc)
    (length keys)
  doc = txtFormat txt
  snip = if val == 0 then "" else
    intercalate " " $ take (snipEndInd - snipStartInd) $ drop snipStartInd $ doc
  snipStartInd = max 0 (snipEndInd - 29)
  snipEndInd = getSnippet m + 1

-- | constants for the formula
freqWt :: Double
freqWt = 1.5

diverseWt :: Double
diverseWt = 2

distWt :: Double
distWt = 10000

-- | the value formula
valFormula :: Int -> Int -> Double -> Int -> Int -> Double
valFormula numOc numDisti avgD lenDoc numKeys =
  freqWt * fromIntegral numOc / chkDenom lenDoc +
    diverseWt * fromIntegral numDisti / chkDenom numKeys + avgTerm
  where
    avgTerm = if avgD > 0 then distWt / (exp 1)**avgD else 0

-- | given a map of occurrences, determine the number of times any keyword
-- occurred
numOccur :: OccurMap -> Int
numOccur m = foldr (\ x accu -> length x + accu) 0 (Map.elems m)

-- | given a map of occurrences, determine the number of distinct keywords that
-- occurred
numDistinct :: OccurMap -> Int
numDistinct = Map.size

-- | given a map of occurrences, determine the average number of words between
-- occurrences of keywords
avgDist :: OccurMap -> Double
avgDist m = lAvg $ map (\ (v, w) -> abs $ v - w) (eachPair avgs) where
  avgs = map (\ (_, l) -> lAvg $ map fromIntegral l) (Map.toList m)

-- | get each unordered pair of a list
eachPair :: [a] -> [(a, a)]
eachPair (x:xs) = (foldr (\ a accu -> (x, a):accu) [] xs) ++ eachPair xs
eachPair _      = []

-- | get average of a list of Doubles
lAvg :: [Double] -> Double
lAvg l = (sum l) / (chkDenom $ length l) where

chkDenom :: Int -> Double
chkDenom x = if x > 0 then fromIntegral x else 1

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
