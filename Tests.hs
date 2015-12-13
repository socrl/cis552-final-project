{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Tests where

import Test.HUnit
import UrlUtils

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements, 
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize, maxSuccess)

import qualified PageParser as P
import qualified Parser as P
import qualified ParserCombinators as P

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
tRelPath7 = getRelPath "" ~?= Just ""

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

-- tests for PageParsers.hs

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

-- tests for Downloader.hs

tTypeAllow1 :: Test
tTypeAllow1 = typeAllow "http://www.cis.upenn.edu/index.php" ~?= True

tTypeAllow2 :: Test
tTypeAllow2 = typeAllow "http://www.dcs.bbk.ac.uk/~mark/" ~?= True

tTypeAllow3 :: Test
tTypeAllow3 = typeAllow "http://www.dcs.bbk.ac.uk/~martin/sewn/ls3/testpage.html" ~?= True

tTypeAllow4 :: Test
tTypeAllow4 = typeAllow "http://www.dcs.bbk.ac.uk/~martin/sewn/ls3/images/GoodGoing-YouGotTheLink.jpg" ~?= False

robotInfo1 :: [P.LineInfo]
robotInfo1 = [P.Disallow "/search", P.Allow "/search/about", P.Disallow "/sdch", P.Disallow "/groups"]

tPathAllow1 :: Test
tPathAllow1 = pathAllow robotInfo1 "http://www.test.com/search/other/index.php" ~?= False

tPathAllow2 :: Test
tPathAllow2 = pathAllow robotInfo1 "http://www.test.com/search/about/index.php" ~?= True

tPathAllow3 :: Test
tPathAllow3 = pathAllow robotInfo1 "http://www.test.com/something/else.html" ~?= True

tPathAllow4 :: Test
tPathAllow4 = pathAllow robotInfo1 "http://www.test.com/search/other/index.php" ~?= False


tDownloaderTests :: [Test]
tDownloaderTests = [tTypeAllow1, tTypeAllow2, tTypeAllow3, tTypeAllow4,
                    tPathAllow1, tPathAllow2, tPathAllow3, tPathAllow4]


main :: IO ()
main = do
  _ <- runTestTT $ TestList [tDom1, tDom2, tDom3, tDom4, tDom5, tDom6, tDom7,
                             tDom8, tRelPath1, tRelPath2, tRelPath3, tRelPath4,
                             tRelPath5, tRelPath6, tRelPath7, tType1, tType2,
                             tType3, tType4, tType5, tType6, tMatchPath1,
                             tMatchPath2, tMatchPath3, tMatchPath4, tMatchPath5,
                             tMatchPath6, tMatchPath7]
  return ()

