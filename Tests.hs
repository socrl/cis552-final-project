{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Tests where

import Test.HUnit

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements, 
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize, maxSuccess)
import Control.Monad (unless)

import UrlUtils
import qualified PageParser as P
import qualified Parser as P
import qualified ParserCombinators as P
import PostProcessor
import Data.Map (Map)
import qualified Data.Map as Map
import Downloader

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
tRelPath7 = getRelPath "" ~?= Nothing

tType1 :: Test
tType1 = getType "https://global.upenn.edu/isss/opt#tutorial" ~?= Just "html"

tType2 :: Test
tType2 = getType "https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/Eraserhead.jpg/800px-Eraserhead.jpg"
  ~?= Just "jpg"

tType3 :: Test
tType3 = getType "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html"
  ~?= Just "html"

tType4 :: Test
tType4 = getType "package/base-4.8.1.0/docs/Data-List.html" ~?= Just "html"

tType5 :: Test
tType5 = getType "https://www.google.com" ~?= Just "html"

tType6 :: Test
tType6 = getType "https://global.upenn.edu/isss/opt" ~?= Just "html"

tMatchPath1 :: Test
tMatchPath1 = matchPath "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html" "package/base-4.8.1.0/docs/Data-List.html"
  ~?= True

tMatchPath2 :: Test
tMatchPath2 = matchPath "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html" "package/base-4.8.1.2/docs/Data-List.html"
  ~?= False

tMatchPath3 :: Test
tMatchPath3 = matchPath "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html" "docs/Data-List.html"
  ~?= False

tMatchPath4 :: Test
tMatchPath4 = matchPath "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html" "https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html"
  ~?= False

tMatchPath5 :: Test
tMatchPath5 = matchPath "https://global.upenn.edu/isss/opt" "opt" ~?= False

tMatchPath6 :: Test
tMatchPath6 = matchPath "https://global.upenn.edu/isss/opt" "u/isss/opt"
  ~?= False

tMatchPath7 :: Test
tMatchPath7 = matchPath "https://global.upenn.edu/isss/opt" "/isss/opt" ~?= True

tMatchPath8 :: Test
tMatchPath8 = matchPath "https://global.upenn.edu/isss/opt" "/isss/o" ~?= False

tMatchPath9 :: Test
tMatchPath9 = matchPath "https://global.upenn.edu/isss/opt" "isss/opt" ~?= True

tMatchPath10 :: Test
tMatchPath10 = matchPath "https://global.upenn.edu/isss/opt" "isss/opt/"
  ~?= False

tMatchPath11 :: Test
tMatchPath11 = matchPath "https://global.upenn.edu/isss/opt" "/isss/" ~?= True

tMatchPath12 :: Test
tMatchPath12 = matchPath "https://global.upenn.edu/isss/opt" "isss/" ~?= True

tMatchPath13 :: Test
tMatchPath13 = matchPath "https://global.upenn.edu/isss/opt" "/isss" ~?= True

tMatchPath14 :: Test
tMatchPath14 = matchPath "https://global.upenn.edu/isss/opt" "isss" ~?= True

tMatchPath15 :: Test
tMatchPath15 = matchPath "https://global.upenn.edu/isss/opt" "iss" ~?= False

tMatchPath16 :: Test
tMatchPath16 = matchPath "https://global.upenn.edu/isss/opt" "" ~?= False

tMatchPath17 :: Test
tMatchPath17 = matchPath "" "" ~?= False

tMatchPath18 :: Test
tMatchPath18 = matchPath "" "a" ~?= False

tCheckProt1 :: Test
tCheckProt1 = checkProt "http://www.dcs.bbk.ac.uk/~martin/sewn/ls3/testpage.html"
  ~?= Just True

tCheckProt2 :: Test
tCheckProt2 = checkProt "https://www.haskell.org/hoogle/"
  ~?= Just False

tCheckProt3 :: Test
tCheckProt3 = checkProt "www.haskell.org/hoogle/"
  ~?= Nothing

tCheckProt4 :: Test
tCheckProt4 = checkProt "hoogle"
  ~?= Nothing

tCheckProt5 :: Test
tCheckProt5 = checkProt ""
  ~?= Nothing

-- | tests for PageParsers.hs

comment :: String
comment = "# robots.txt for http://www.wikipedia.org/ and friends\n"

multilineComment :: String
multilineComment = "#\n# robots.txt for http://www.wikipedia.org/ and friends\n#\n# Please note: There are a lot of pages on this site, and there are\n# some misbehaved spiders out there that go _way_ too fast. If you're\n# irresponsible, your access to the site may be blocked.\n#\n\n# advertising-related bots:\n"

notOurUserAgent :: String
notOurUserAgent = "User-agent: Mediapartners-Google*\nDisallow: /\n"

agentCommentMix :: String
agentCommentMix = multilineComment ++ notOurUserAgent ++ "\n# Wikipedia work bots:\nUser-agent: *\nDisallow: /\n"

validUserAgent :: String 
validUserAgent = "User-agent: *\nDisallow: /search\nAllow: /search/about\nDisallow: /sdch\nDisallow: /groups\nDisallow: /catalogs\nAllow: /catalogs/about\nAllow: /catalogs/p?\nDisallow: /catalogues\n"

userAgentInlineComment :: String
userAgentInlineComment = "User-agent: *\nDisallow: /search\nAllow: /search/about # COMMENTARY\nDisallow: /sdch\n"

validRobotsPg :: String
validRobotsPg = multilineComment ++ notOurUserAgent ++ "\nUser-agent: Orthogaffe\nDisallow:\n" ++ comment ++ validUserAgent ++ comment ++ "Disallow: /wiki/Wikipedia:Strony_do_usuni%C4%99cia\n"

userAgentCrawlDelay :: String
userAgentCrawlDelay = "User-agent: *\nCrawl-delay: 10\nDisallow: /search\nAllow: /search/about\nDisallow: /sdch\n"

fullRobotsTxt :: String
fullRobotsTxt = multilineComment ++ notOurUserAgent ++ userAgentCrawlDelay ++ "\nSitemap: https://www.yahoo.com/food/sitemaps/sitemap_index_us_en-US.xml.gz"

tParseComment :: Test
tParseComment = "1-line comment" ~: P.parse P.commentP comment ~?= Right P.Comment

tParseMultiComment :: Test
tParseMultiComment = "multiline comment" ~: P.parse P.multiCommentP multilineComment 
                                         ~?= Right P.Comment

tParseNotOurUserAgent :: Test
tParseNotOurUserAgent = "parser ignores this user agent" ~: TestList [
  "not our agent success"   ~: P.parse P.notOurUserAgentP notOurUserAgent 
                            ~?= Right [],
  -- | our "not our user agent" parser should fail when presented with the 
  --   agent we should be parsing for.
  "not our agent fail"      ~: P.parse P.notOurUserAgentP validUserAgent
                            ~?= Left "No parses" ]


tParseTgtUserAgent :: Test
tParseTgtUserAgent = "parser returns this agent's info" ~: TestList [
  "tgt agent success"    ~: P.parse P.userAgentP validUserAgent
                  ~?= Right [P.Disallow "/search",   P.Allow "/search/about",
                             P.Disallow "/sdch",     P.Disallow "/groups", 
                             P.Disallow "/catalogs", P.Allow "/catalogs/about", 
                             P.Allow "/catalogs/p?", P.Disallow "/catalogues"],
   "tgt agent crawl del" ~: P.parse P.userAgentP userAgentCrawlDelay
                  ~?= Right [P.CrawlDelay 10,         
                             P.Disallow "/search",   P.Allow "/search/about",
                             P.Disallow "/sdch"],
   "not our agent fail"  ~: P.parse P.userAgentP notOurUserAgent
                  ~?= Left "No parses" ]


tParseRobotsTxt :: Test
tParseRobotsTxt = "parser for the robots.txt" ~: TestList [
  "simple comments and agents" ~: P.parse P.robotP agentCommentMix 
            ~?= Right [[], [P.Disallow "/"]],
  "agent with in-line comment" ~: P.parse P.robotP userAgentInlineComment
            ~?= Right [[P.Disallow "/search",   P.Allow "/search/about",
                        P.Disallow "/sdch"]],
  "example robots.txt file"    ~: P.parse P.robotP validRobotsPg
            ~?= Right [[],[],
                      [P.Disallow "/search",   P.Allow "/search/about",
                       P.Disallow "/sdch",     P.Disallow "/groups",
                       P.Disallow "/catalogs", P.Allow "/catalogs/about",
                       P.Allow "/catalogs/p?", P.Disallow "/catalogues",
                       P.Comment,
                       P.Disallow "/wiki/Wikipedia:Strony_do_usuni%C4%99cia"]],
  "robots with sitemap"       ~: P.parse P.robotP fullRobotsTxt
            ~?= Right [[],
                       [P.CrawlDelay 10,         
                             P.Disallow "/search",   P.Allow "/search/about",
                             P.Disallow "/sdch"]] ]


-- | tests for PostProcessor.hs
tTrimNonAlpha1 :: Test
tTrimNonAlpha1 = trimNonAlpha "" ~?= ""

tTrimNonAlpha2 :: Test
tTrimNonAlpha2 = trimNonAlpha "abc" ~?= "abc"

tTrimNonAlpha3 :: Test
tTrimNonAlpha3 = trimNonAlpha "ab .c" ~?= "ab .c"

tTrimNonAlpha4 :: Test
tTrimNonAlpha4 = trimNonAlpha ".,//(* )ab .c. .   " ~?= "ab .c"

str1 :: String
str1 = "Huang Weikai assembles footage from a dozen amateur videographers and weaves them into a unique symphony of urban social dysfunction."

key1 :: [String]
key1 = ["assembles", "UrBaN", "huang", "duck", "dysfunction", "a"]

map1 :: Map String [Int]
map1 = Map.fromList [("huang", [0]), ("assembles", [2]), ("urban", [17]), ("a", [5, 13]), ("dysfunction", [19])]

str2 :: String
str2 = "Festen is best known for being the first Dogme 95 film (its full title in Denmark is Dogme #1 - Festen). Dogme films are governed by a manifesto that insists on specific production and narrative limitations (such as banning any post-production sound editing), in part as a protest against the expensive Hollywood-style film-making. The film was shot on a Sony DCR-PC3 Handycam on standard Mini-DV cassettes."

key2 :: [String]
key2 = ["FESTEN", "dogme", "95", "facebook"]

map2 :: Map String [Int]
map2 = Map.fromList [("festen", [0, 19]), ("dogme", [8, 17, 20]), ("95", [9])]

tFindWords1 :: Test
tFindWords1 = findWords (txtFormat str1) (keyFormat key1) ~?= map1

tFindWords2 :: Test
tFindWords2 = findWords (txtFormat str2) (keyFormat key2) ~?= map2

tNumOccur1 :: Test
tNumOccur1 = numOccur map1 ~?= 6

tNumOccur2 :: Test
tNumOccur2 = numOccur map2 ~?= 6

tNumDistinct1 :: Test
tNumDistinct1 = numDistinct map1 ~?= 5

tNumDistinct2 :: Test
tNumDistinct2 = numDistinct map2 ~?= 3

-- | check for equality of doubles (from StackOverflow)
assertEquals :: String -> Double -> Double -> Double -> Assertion
assertEquals preface delta expected actual =
  unless (abs (expected - actual) < delta) (assertFailure msg)
  where
    msg = (if null preface then "" else preface ++ "\n") ++ "expected: " ++
      show expected ++ "\n but got: " ++ show actual

delt :: Double
delt = 0.001

tAvgDist1 :: Test
tAvgDist1 = TestCase $ assertEquals "" delt 10.6 (avgDist map1)

tAvgDist2 :: Test
tAvgDist2 = TestCase $ assertEquals "" delt 4 (avgDist map2)

tGetPgValue1 :: Test
tGetPgValue1 = TestCase $ assertEquals "" delt 2.3658 x where
  x = f $ getPgValue (keyFormat key1) ("URL1", str1)
  f (_, _, y, _) = y

tGetPgValue2 :: Test
tGetPgValue2 = TestCase $ assertEquals "" delt 184.7949 x where
  x = f $ getPgValue (keyFormat key2) ("URL2", str2)
  f (_, _, y, _) = y

tRankPages1 :: Test
tRankPages1 = rankPages [("URL1", str1), ("URL2", str2)] ["URbAn"] ~?=
  [("URL1", str1, 2.075, "huang weikai assembles footage from a dozen amateur videographers and weaves them into a unique symphony of urban"), ("URL2", str2, 0, "")]

-- tests for Downloader.hs

tTypeAllow1 :: Test
tTypeAllow1 = typeAllow "http://www.cis.upenn.edu/index.php" ~?= True

tTypeAllow2 :: Test
tTypeAllow2 = typeAllow "http://www.dcs.bbk.ac.uk/~mark/" ~?= True

tTypeAllow3 :: Test
tTypeAllow3 = typeAllow "http://www.dcs.bbk.ac.uk/~martin/sewn/ls3/testpage.html" ~?= True

tTypeAllow4 :: Test
tTypeAllow4 = typeAllow "http://www.dcs.bbk.ac.uk/~martin/sewn/ls3/images/GoodGoing-YouGotTheLink.jpg" ~?= False


main :: IO ()
main = do
  _ <- runTestTT $ TestList [tDom1, tDom2, tDom3, tDom4, tDom5, tDom6, tDom7,
                             tDom8, tRelPath1, tRelPath2, tRelPath3, tRelPath4,
                             tRelPath5, tRelPath6, tRelPath7, tType1, tType2,
                             tType3, tType4, tType5, tType6, tMatchPath1,
                             tMatchPath2, tMatchPath3, tMatchPath4, tMatchPath5,
                             tMatchPath6, tMatchPath7, tMatchPath8, tMatchPath9,
                             tMatchPath10, tMatchPath11, tMatchPath12,
                             tMatchPath13, tMatchPath14, tMatchPath15,
                             tMatchPath16, tMatchPath17, tMatchPath18,
                             tCheckProt1, tCheckProt2, tCheckProt3, tCheckProt4,
                             tCheckProt5, tParseComment, tParseMultiComment,
                             tParseNotOurUserAgent, tParseTgtUserAgent,
                             tParseRobotsTxt, tTrimNonAlpha1, tTrimNonAlpha2,
                             tTrimNonAlpha3, tTrimNonAlpha4, tFindWords1,
                             tFindWords2, tNumOccur1, tNumOccur2, tNumDistinct1,
                             tNumDistinct2, tAvgDist1, tAvgDist2, tGetPgValue1,
                             tGetPgValue2, tRankPages1, tTypeAllow1,
                             tTypeAllow2, tTypeAllow3, tTypeAllow4]
  return ()

