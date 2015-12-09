{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module PostProcessor where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Utils
import Data.Char

-- | given a string and a list of keywords, find the indices at which each
-- keyword occurs
findWords :: String -> [String] -> Map String [Int]
findWords doc ks =
  findWordsHelp
    (filter (not . null) (map (strLower . trimNonAlpha) (splitWs doc)))
    (map (strLower . trimNonAlpha) ks)

findWordsHelp :: [String] -> [String] -> Map String [Int]
findWordsHelp = f Map.empty 0 where
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
