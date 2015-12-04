{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
    FlexibleInstances #-}

module Downloader where

import Queue
import UrlUtils
import PageParser

import Control.Monad
import Data.Time.Clock
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.IO.Class
import Text.HandsomeSoup

import Data.HashSet

import Data.Map (Map)
import qualified Data.Map as Map

type Result = (String, [String])

type Frontier   = Queue String
type Visited    = HashSet String
type ServerInfo = Map String (Robot, UTCTime)

type Status = (Frontier, Visited, ServerInfo, [Result])

sendReqs :: String -> String -> Int -> Either (IO [Result]) String
sendReqs = undefined

execute :: String -> Int -> Int -> StateT Status IO ()
execute query ord lim | ord > lim = return ()
                      | otherwise = do 
  (f, v, s, rl) <- get
  case dequeue f of 
     Nothing        -> return ()
     Just (url, f') -> do
      -- check robots information
      case getDomain url >>= (\host -> Map.lookup host s) of
        Nothing -> do 
          -- robots info hasn't been retrieved yet
          let domain = fromJust $ getDomain url
          contents <- fetchContents ("http://" ++ domain ++ "/robots.txt")
          case contents of
            Nothing  -> return ()
            Just con -> do currT <- liftIO getCurrentTime
                           put (f, v, Map.insert domain 
                                ((parseRobot $ fromJust contents), currT) s, rl)
                           execute query ord lim
        Just (robot, l_vt) -> do
          -- contain robots info
          let pass = canCrawl robot l_vt url 
          -- cannot crawl, drop this url
          if pass < 0 then do
            put (f', v, s, rl)
            execute query ord lim
          -- can crawl, but need delay
          else if pass == 0 then do
            put (enqueue url f', v, s, rl)
            execute query ord lim
          -- can crawl right now
          else do
            contents <- fectchContents url 
            case contents >>= (Just . (getSnips query)) of
              Nothing -> do put (f', v, s, rl)
                            execute query (ord+1) lim
              Just _ ->  undefined


fetchContents :: (MonadIO m) => String -> m (Maybe String)
fetchContents = liftIO . runMaybeT . openUrl

-- check if the url can be crawled, 
-- 0  - can crawl but need wait; 
-- 1  - can crawl right now; 
-- -1 - cannot crawl
canCrawl :: (MonadIO m) => Robot -> UTCTime -> String -> m Int
canCrawl (ll, c_del) l_vt url = do 
  if not $ pathAllow ll url then return (-1)
    else do
      currT <- liftIO getCurrentTime
      if (diffUTCTime currT l_vt < c_del) then return 0
        else return 1

-- Check if the url is allowed to crawl
pathAllow :: [LineInfo] -> String -> Bool
pathAllow ll url = Prelude.foldr (&&) True $ Prelude.map (lineCheck url) ll

-- Check if the line info is compatible with a url
lineCheck :: String -> LineInfo -> Bool
lineCheck url li = 
  case li of
    Allow    p   -> matchPath url p
    Disallow p   -> not $ matchPath url p
    Comment      -> True
    CrawlDelay _ -> True


{-
1. receive URL, search string, num pages from Main

2. put input URL in queue, do the initialization stuff (map <server, <last visit time, frequency>, queue <url> - the frontier, hashset <url> - did we visit this webpage)
    when we get things off the queue, if we haven't got the site's robots.txt in the map, we request it

3. perform requests
    a. get from queue
    b. check for that url's server - is that in the map
        if not, get its robots.txt
    c. make a request to get the text of that page
    d. tell parser to get snippets
    e. store the tuples of (URL, snippets)
    f. update 3 data structures as necessary

4. return the (URL, snippets)

-}

