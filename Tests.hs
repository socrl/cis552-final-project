{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Tests where

import Test.HUnit
import UrlUtils

-- | Tests for UrlUtils.hs

tDom1 :: Test
tDom1 = getDomain "https://en.wikipedia.org/wiki/Pineapple" ~?=
  Just "en.wikipedia.org"

tDom2 :: Test
tDom2 = getDomain "https://www.google.com/" ~?= Just "www.google.com"

tDom3 :: Test
tDom3 = getDomain "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html"
  ~?= Just "hackage.haskell.org"

tDom4 :: Test
tDom4 = getDomain "abcdefghi" ~?= Nothing

tDom5 :: Test
tDom5 = getDomain "/package/base-4.8.1.0/docs/Data-List.html" ~?= Nothing

tDom6 :: Test
tDom6 = getDomain "http://dla.library.upenn.edu/dla/franklin/record.html?q=seven%20samurai&meta=t&id=FRANKLIN_6416874&"
  ~?= Just "dla.library.upenn.edu"

tDom7 :: Test
tDom7 = getDomain "isss/opt" ~?= Nothing

tDom8 :: Test
tDom8 = getDomain "" ~?= Nothing

tRelPath1 :: Test
tRelPath1 = getRelPath "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html"
  ~?= Just "package/base-4.8.1.0/docs/Data-List.html"

tRelPath2 :: Test
tRelPath2 = getRelPath "https://www.google.com/" ~?= Just ""

tRelPath3 :: Test
tRelPath3 = getRelPath "https://global.upenn.edu/isss/opt" ~?= Just "isss/opt"

tRelPath4 :: Test
tRelPath4 = getRelPath "isss/opt" ~?= Just "isss/opt"

tRelPath5 :: Test
tRelPath5 = getRelPath "https://global.upenn.edu/isss/opt#tutorial" ~?=
  Just "isss/opt"

tRelPath6 :: Test
tRelPath6 = getRelPath "abcde" ~?= Just "abcde"

tRelPath7 :: Test
tRelPath7 = getRelPath "" ~?= Just ""

tType1 :: Test
tType1 = getType "https://global.upenn.edu/isss/opt#tutorial" ~?= Nothing

tType2 :: Test
tType2 = getType "https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/Eraserhead.jpg/800px-Eraserhead.jpg"
  ~?= Just "jpg"

tType3 :: Test
tType3 = getType "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html"
  ~?= Just "html"

tType4 :: Test
tType4 = getType "package/base-4.8.1.0/docs/Data-List.html" ~?= Just "html"

tType5 :: Test
tType5 = getType "https://www.google.com" ~?= Nothing

tType6 :: Test
tType6 = getType "https://global.upenn.edu/isss/opt" ~?= Nothing

tMatchPath1 :: Test
tMatchPath1 = matchPath "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html" "package/base-4.8.1.0/docs/Data-List.html"
  ~?= True

tMatchPath2 :: Test
tMatchPath2 = matchPath "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html" "package/base-4.8.1.2/docs/Data-List.html"
  ~?= False

tMatchPath3 :: Test
tMatchPath3 = matchPath "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html" "docs/Data-List.html"
  ~?= True

tMatchPath4 :: Test
tMatchPath4 = matchPath "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html" "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html"
  ~?= False

tMatchPath5 :: Test
tMatchPath5 = matchPath "https://global.upenn.edu/isss/opt" "opt" ~?= True

tMatchPath6 :: Test
tMatchPath6 = matchPath "https://global.upenn.edu/isss/opt" "u/isss/opt"
  ~?= False

tMatchPath7 :: Test
tMatchPath7 = matchPath "https://global.upenn.edu/isss/opt" "/isss/opt"
  ~?= True

main :: IO ()
main = do
  _ <- runTestTT $ TestList [tDom1, tDom2, tDom3, tDom4, tDom5, tDom6, tDom7,
                             tDom8, tRelPath1, tRelPath2, tRelPath3, tRelPath4,
                             tRelPath5, tRelPath6, tRelPath7, tType1, tType2,
                             tType3, tType4, tType5, tType6, tMatchPath1,
                             tMatchPath2, tMatchPath3, tMatchPath4, tMatchPath5,
                             tMatchPath6, tMatchPath7]
  return ()
