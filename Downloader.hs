{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction, 
    FlexibleInstances #-}

module Downloader where

import Queue
import UrlUtils
import PageParser

import Control.Monad
import Data.Time.Clock
import Data.HashSet
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.IO.Class
import Text.HandsomeSoup

import Data.Map (Map)
import qualified Data.Map as Map

import State (State)
import qualified State as S

type Result = (String, String)

type Frontier   = Queue String
type Visited    = HashSet String
type ServerInfo = Map String Robot

type Status = (Frontier, Visited, ServerInfo, [Result])

sendReqs :: String -> String -> Int -> Either [Result] String
sendReqs = undefined

execute :: (MonadState Status m) => Int -> Int -> m ()
execute ord lim | ord > lim = return ()
				        | otherwise = do 
  (f, v, s, rl) <- S.get
  case dequeue f of 
  	 Nothing        -> return ()
  	 Just (url, f') -> case getDomain url >>= (\host -> Map.lookup host s) of
                         Nothing -> do contents <- (runMaybeT $ openUrl ("http://" ++ domain ++ "/robots.txt"))
                                       S.put (f, v, Map.insert domain (parseRobot $ fromJust contents) s, rl)
                                       execute ord lim where
                                         domain = fromJust $ getDomain url
      	                 Just (lst_v, freq) -> undefined 


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

