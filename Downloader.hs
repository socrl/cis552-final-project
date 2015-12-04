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
import qualified Data.HashSet as Hashset

import Data.Map (Map)
import qualified Data.Map as Map

type Result = (String, [String])

type Frontier   = Queue String
type Visited    = HashSet String
type ServerInfo = Map String (Robot, UTCTime)

-- Status maintains four states
-- 1. Frontier  : storing the URLs to be crawled
-- 2. Visited   : the URLs that have been crawled
-- 3. ServerInfo: mapping the domain to the robots information of the 
--                domain, and last visit time
-- 4. [Result]  : list of results, including the URL and a list of 
--                matched snippets
type Status = (Frontier, Visited, ServerInfo, [Result])

-- API for top level function, pass in a starting URL address,
-- a search query and upper limit of # of pages to crawl
sendReqs :: String -> String -> Int -> IO ([Result], Int)
sendReqs url query lim = do 
  (_, (_, v, _, rl)) <- runStateT (execute query 0 lim) 
                        (single url, Hashset.empty, Map.empty, [])
  return (rl, size v)

-- Main execution method, responsible for scheduling tasks, sending
-- HTTP request, pass web content to parser and storing results 
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
                                ((parseRobot con), currT) s, rl)
                           execute query ord lim
        Just (robot, l_vt) -> do
          -- contain robots info
          pass <- canCrawl robot l_vt url 
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
            contents <- fetchContents url 
            case contents >>= (Just . (getSnips query)) of
              Nothing -> do put (f', v, s, rl)
                            execute query (ord+1) lim
              Just _ ->  return ()


fetchContents :: (MonadIO m) => String -> m (Maybe String)
fetchContents = liftIO . runMaybeT . openUrl

-- check if the url can be crawled, 
-- 0  - can crawl but need wait; 
-- 1  - can crawl right now; 
-- -1 - cannot crawl
canCrawl :: (MonadIO m) => Robot -> UTCTime -> String -> m Int
canCrawl (ll, delay) last_vt url = do 
  if not $ pathAllow ll url then return (-1)
    else do
      currT <- liftIO getCurrentTime
      -- check if enough time has passed since last site visit
      if ((realToFrac $ diffUTCTime currT last_vt :: Double) < 
          (fromIntegral delay :: Double)) then return 0 
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

