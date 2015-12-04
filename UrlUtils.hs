{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module UrlUtils where

import Data.List.Split
import Data.List
import Data.Maybe
import Network.URL

-- | given absolute URL, get domain (if any)
getDomain :: String -> Maybe String
getDomain s = importURL s >>= \ x -> case url_type x of
  Absolute h -> Just $ host h
  _ -> Nothing

-- | given URL, get relative path (exclude domain and all preceding it)
getRelPath :: String -> Maybe String
getRelPath s = importURL s >>= Just . url_path

-- | given URL, get the type of document .type at the end
getType :: String -> Maybe String
getType s = getRelPath s >>= parseType

-- | given relative URL, get the type of the document
parseType :: String -> Maybe String
parseType s =
  if isJust $ find ((==) '.') s then Just $ last $ splitOn "." s else Nothing

-- | given absolute URL and relative URL, return whether the relative path is
-- part of the absolute path
matchPath :: String -> String -> Bool
matchPath a r = maybe False f (getRelPath a) where
    f x = validPrefix r x ||
            if not (null r) && head r == '/' then validPrefix (tail r) x
            else False
    validPrefix [p]    (s:sl:_) = p == s && (p == '/' || sl == '/')
    validPrefix (p:ps) (s:st)   = if p == s then validPrefix ps st else False
    validPrefix []     _        = True
    validPrefix _      _        = False





