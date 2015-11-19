{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Downloader where

import Network.URL

type Result = (URL, String)

sendReqs :: String -> String -> Int -> Either [Result] String
sendReqs = undefined
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