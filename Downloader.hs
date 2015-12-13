{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction,
    FlexibleInstances #-}

module Downloader where

import Queue
import UrlUtils
import PageParser

import Data.List
import Data.Time.Clock
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Exception.Base
import Control.Monad.State
import Text.HandsomeSoup

import Data.HashSet
import qualified Data.HashSet as Hashset

import Data.Map (Map)
import qualified Data.Map as Map

type Result = (String, String)

type Frontier   = Queue String
type Visited    = HashSet String
type ServerInfo = Map String (Robot, UTCTime)

type URL        = String
type Query      = String

data CrawlCheck = NoCrawl
                | CanCrawl
                | NeedWait
                deriving (Eq, Show)

-- Status maintains four states
-- 1. Frontier  : storing the URLs to be crawled
-- 2. Visited   : the URLs that have been crawled
-- 3. ServerInfo: mapping the domain to the robots information of the
--                domain, and last visit time
-- 4. [Result]  : list of results, including the URL and the HTML doc
type Status = (Frontier, Visited, ServerInfo, [Result])

allowedTypes :: [String]
allowedTypes = ["html", "php", "txt"]

-- API for top level function, pass in a starting URL address,
-- a search query and upper limit of # of pages to crawl
sendReqs :: URL -> Query -> Int -> IO ([Result], Int)
sendReqs url query lim = do
  (_, (_, v, _, rl)) <- runStateT (execute query 0 lim)
                        (single url, Hashset.empty, Map.empty, [])
  return (rl, size v)

-- Main execution method, responsible for scheduling tasks, sending
-- HTTP request, pass web content to parser and storing results
execute :: Query -> Int -> Int -> StateT Status IO ()
execute query ord lim | ord >= lim = return ()
                      | otherwise  = do
  status@(f, v, s, rl) <- get
  currT <- liftIO getCurrentTime
  case dequeue f of
     Nothing        -> return ()
     Just (url, f') ->
      -- check if we have visited the page
      if Hashset.member url v then do
        put (f', v, s, rl)
        execute query ord lim
      else
        -- check robots information
        case getDomain url >>= (`Map.lookup` s) of
          Nothing -> do
            -- robots info hasn't been retrieved yet
            let domainM = getDomain url
            if isNothing domainM then do
              put (f', v, s, rl)
              execute query ord lim
            else do
              getRobot status (fromJust domainM) currT
              execute query ord lim
          Just (robot, lastT) -> do
            -- contain robots info, try crawl the web
            let check = canCrawl robot lastT currT url
                domain = fromJust $ getDomain url
            case check of
              NoCrawl  -> do
                put (f', v, s, rl)
                execute query ord lim
              NeedWait -> do
                put (enqueue url f', v, s, rl)
                execute query ord lim
              CanCrawl -> do
                getContentAndSearch status url query f' domain robot currT
                execute query (ord+1) lim


-- helper function for scheduler that fetch the robots.txt info
getRobot :: Status -> String -> UTCTime -> StateT Status IO ()
getRobot status domain currT = do
  let (f, v, s, rl) = status
  contents <- fetchContents ("http://" ++ domain ++ "/robots.txt")
  case contents of
    Nothing  -> put (f, v, Map.insert domain (([],1), currT) s, rl)
    Just con -> put (f, v, Map.insert domain
                     (parseRobot con, currT) s, rl)

-- helper function for scheduler that fetch the web page, parse and search
-- for keywords
getContentAndSearch :: Status -> URL -> Query -> Frontier -> String
                              -> Robot -> UTCTime -> StateT Status IO ()
getContentAndSearch status url query f' domain robot currT = do
  let (_, v, s, rl) = status
  let s'  = Map.insert domain (robot, currT) s
  contents <- fetchContents url
  -- parse the document to get contained URLs and see if any query word matched
  case contents >>= (Just . parseDoc url query) of
    Nothing  -> put (f', v, s', rl)
    Just result -> do
      (urls, doc) <- liftIO result
      let v'  = Hashset.insert url v
          f'' = Prelude.foldr enqueue f' urls
      if Prelude.null doc then
        put (f'', v', s', rl)
      else
        put (f'', v', s', (url, doc) : rl)

-- helper function that fetch the contents from an URL
fetchContents :: (MonadIO m) => URL -> m (Maybe String)
fetchContents url = liftIO $ catch ((liftIO . runMaybeT . openUrl) url)
                                   (\e -> return Nothing `const`
                                          (e :: IOException))

-------------- helper functions that do not require IO -----------------

-- check if the url can be crawled with three possible states
canCrawl :: Robot -> UTCTime -> UTCTime -> URL -> CrawlCheck
canCrawl (ll, delay) lastT currT url
  | not (pathAllow ll url && typeAllow url) = NoCrawl
  | timeAllow lastT currT delay             = NeedWait
  | otherwise                               = CanCrawl

-- Check if the URL has proper type for crawling
typeAllow :: URL -> Bool
typeAllow url = case getType url >>= \x -> find (x ==) allowedTypes of
                  Nothing -> False
                  Just _  -> True

-- Check if not enough time has passed since last crawl
timeAllow :: UTCTime -> UTCTime -> Int -> Bool
timeAllow lastT currT delay =
  (realToFrac $ diffUTCTime currT lastT :: Double) <
  (fromIntegral delay :: Double)

-- Check if the url is allowed to crawl based on path info from Robots.txt
pathAllow :: [LineInfo] -> URL -> Bool
pathAllow ll url = not $ (&&) (pathCheck url ll disallowPath)
                              (not $ pathCheck url ll allowPath)

-- Check if any line info disallow crawling the path
pathCheck :: URL -> [LineInfo] -> (URL -> LineInfo -> Bool) -> Bool
pathCheck url ll check = any (check url) ll

-- Check if a line info disallow a path
disallowPath :: URL -> LineInfo -> Bool
disallowPath url li =
  case li of
    Disallow p -> matchPath url p
    _          -> False

-- Check if a line info allow a path
allowPath :: URL -> LineInfo -> Bool
allowPath url li =
  case li of
    Allow p   -> matchPath url p
    _         -> False
