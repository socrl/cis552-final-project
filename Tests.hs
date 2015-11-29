{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Tests where

import Test.HUnit (runTestTT,Test(..),Assertion, (~?=), (~:), assert)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements, 
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize, maxSuccess)

import qualified PageParser as P
import qualified Parser as P
import qualified ParserCombinators as P

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



