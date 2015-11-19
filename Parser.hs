{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Parser where

type Robot = ([RelPath], Int)

type RelPath = String

getSnips :: String -> String -> [String]
getSnips = undefined

parseRobot :: String -> Robot
parseRobot = undefined



{-




robots.txt String -> [disallowed URL], permitted frequency

HTML String -> Search String -> [Snippet String]

-}