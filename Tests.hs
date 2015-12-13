{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
module Tests where

import Test.HUnit

import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, elements, 
  oneof, frequency, sized, quickCheckWith, stdArgs, maxSize, maxSuccess)
import Control.Monad (unless)

import PageParser 
import ParserCombinators
import Text.Regex
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

-- tests for PageParsers.hs
tParseWebpageFns :: Test
tParseWebpageFns = TestList 
  [tParseDocFail, tParseDocFail, tListUrls, tRetPageContents,
   tQuerySuccess, tOrOperation, tLowercase, tTrim, tRmTrail]

validUrlsForParseDoc :: [String]
validUrlsForParseDoc = ["http://en.wikipedia.org/wiki/alan_turing","http://www.claymath.org/millennium-problems/p-vs-np-problem/","http://www.cis.upenn.edu/~alur/","http://www.automatatutor.com/","http://www.cis.upenn.edu/~alur/"]

testHTML :: String
testHTML = "<html>\n<head><title>CIS 262: Automata., Computability, and Complexity</title>\n</head>\n<body bgcolor=#ffffff>\n<h2><FONT COLOR=red>\n CIS 262: Automata, Computability, and Complexity<br>\nFall 2015, \nUniversity of Pennsylvania</font></h2>\n<hr>\n<H3><font color=green>Introduction</font></H3>\nThis course offers an introduction to the <i>science </i> behind computing by\nstudying computation abstractly without worrying about specifics of programming\nlanguages and/or computing platforms.\nIn particular, we will study\n(a) finite automata that capture what can be computed using constant\nmemory, and define the class of regular languages useful for pattern matching\nlanguages;\n(b) the universal computational model of \n<a href=\"http://en.wikipedia.org/wiki/Alan_Turing\"> Turing</a> machines, and the inherent\nlimits of what can be solved on a computer (undecidability), and\n(c) the notion of computational tractability (which problems\ncan be solved in polynomial-time), \nand <a href=\"http://www.claymath.org/millennium-problems/p-vs-np-problem/\">the million-dollar P vs NP question.</a>\nThe course also emphasizes rigorous thinking and mathematical proofs.\n\n<hr>\n<h3><font color=green>Logistics</font></h3>\n<ul>\n<li>Class: Tues and Thurs 12--1.20, Towne 100, Heilmeier Hall\n<li>Instructor: \n<a href=\"http://www.cis.upenn.edu/~alur/\">Rajeev Alur</a>\n(<a href=\"mailto:alur@cis.upenn.edu\">alur@cis</a>), Office hour: Wed 4-5, Levine 609\n<li>Teaching Assistants: \n<ul>\n<li> Jie Chen (<a href=\"mailto:chenjr@seas.upenn.edu\">chenjr@seas</a>); Office hour: Tues 8-9, Moore 100C\n<li> Lucas Dagostino (<a href=\"mailto:lucasdag@seas.upenn.edu\">lucasdag@seas</a>); Office hour: Mon 7-8, Towne 305\n<li> Jungi Kim (<a href=\"mailto:jungikim@sas.upenn.edu\">jungikim@sas</a>); Office hour: Tues 7.30-8.30, Towne 307\n<li> Derick Olson (<a href=\"mailto:dericko@sas.upenn.edu\">dericko@sas</a>); Office hour: Wed 5-6, Moore 100C\n<li> Omar Paladines (<a href=\"mailto:omarp@sas.upenn.edu\">omarp@sas</a>); Office hour: Mon 11-12, Moore 100B\n<li> Alex Piatski (<a href=\"mailto:alpi@seas.upenn.edu\">alpi@seas</a>); Office hour: Mon 6-7 and Tues 5-6, both in Moore 100C\n<li> Kelly Tan (<a href=\"mailto:kellytan@seas.upenn.edu\">kellytan@seas</a>); Office hour: Wed 12-1, Towne 315\n\n</ul>\n<li>Recitation: Monday 4.30 -- 5.30, DRLB A1\n<li>Pre-requisites: CIS 160\n     </ul>\n<hr>\n<H3><font color=green>Textbooks</font></H3>\n     <ul>\n\t    <li>\nRequired: Introduction to the theory of computation, Michael\nSipser, Third Edition, 2012, or Second Edition, 2006.\n       <li>Additional reference: Introduction to Automata Theory, Languages\nand Computation, J.E. Hopcroft, R. Motwani, \nand J.D. Ullman, Addison Wesley, Third edition, 2006.\n</ul>\n\n<hr>\n\t\t <h3><font color=green>Grading</font></h3>\nGrades will be based on\n<ul>\n  <li>Weekly Homework Assignments:  10 total, 10 pts each\n     <li> Two Midterms:  50 pts each\n     <li> Final Exam: 100 pts\n  </ul>\n<hr>\n <h3><font color=green>Schedule</font></h3>\n<ul>\n  <li>Aug 27: Course logistics, Discussion of course content, Introduction to\n    automata\n<li>Sept 1, 3, 8, 10, 15, 17, 22, 24: Finite\n  automata and regular languages (Chapter 1)\n<li> <b>Oct 1: Midterm 1 (in class)</b>\n<li>Oct 6, 13, 15, 20, 22, 27, 29; Nov 3: Turing machines and undecidability (Chapters 3, 4, 5) \n<li><b> Nov 5: Midterm 2  (in class)</b>\n<li> Nov 10, 17, 19, 24; Dec 1, 3, 8:  Complexity classes and NP-completeness (Chapters 7 and 8)\n<li> <b>Dec 18, 12 - 2: Final Exam</b>\n </ul>\n<hr>\n  <h3><font color=green>Notes</font></h3>\n  <ul>\n<li> We will use <a href=\"https://piazza.com/upenn/fall2015/cis262/home\">Piazza</a> for discussions.\nHomeworks, lecture slides, and handouts will also be posted on Piazza.\n<li> Grades are posted on <a href=\"https://canvas.upenn.edu/\">canvas</a>.\n<li> For a few problems, we will use the tool <a href=\"http://www.automatatutor.com/\">AutomataTutor</a>.\nSign up for an account on AutomataTutor, make sure that name and email on this account matches your Penn account.\n<li>During the class, use of mobile devices and laptops is prohibited.\n<li> Recitation: During Monday's recitation, we will mainly focus on solving\nproblems. Many students find the homeworks difficult, and even when they have\nsolved the problem, some find it difficult to write the answer concisely and\nrigorously. Recitation should help with these challenges. However, attendance\nfor recitation is optional as no new material will be covered.\n\n\n<li>Homework Drop-off and Pick-up: On the day the homework is due, it should be\nsubmitted before the lecture starts.\nGraded exams and homeworks can be collected from Laura Fox in Levine 308\n(<a href=\"mailto:lffox@seas.upenn.edu\">lffox@seas</a>) during Mon--Fri, 9--12 and 2--4pm.\n\n\n<li>Plagiarism Policy: For homeworks, you can use your class notes, the textbook, and the reference book, but not old solutions, friends, other books, or any other material from the web.\n\t For violations of this rule, you will be reported to the Office of\n  Student Conduct at Penn.\nStart working on homework problems early, and if you get stuck, contact one of\nus; we will be happy to help you progress.\n<li>Open book exams: Both midterms and final exam will be open book: during the exam, you can consult\nthe textbook, your class notes, and handouts distributed during the course, but are not allowed to\naccess the internet.\n  </ul>\n  <hr>\n  <a href=\"http://www.cis.upenn.edu/~alur/\">Maintained by Rajeev Alur</a>\n</body>\n</html>\n"

outputStringForParseDoc :: String
outputStringForParseDoc = "CIS 262: Automata, Computability, and Complexity Fall 2015, University of Pennsylvania     Introduction  This course offers an introduction to the  science   behind computing bystudying computation abstractly without worrying about specifics of programminglanguages and/or computing platforms.In particular, we will study(a) finite automata that capture what can be computed using constantmemory, and define the class of regular languages useful for pattern matchinglanguages;(b) the universal computational model of   Turing  machines, and the inherentlimits of what can be solved on a computer (undecidability), and(c) the notion of computational tractability (which problemscan be solved in polynomial-time), and  the million-dollar P vs NP question. The course also emphasizes rigorous thinking and mathematical proofs.   Logistics    Class: Tues and Thurs 12--1.20, Towne 100, Heilmeier Hall  Instructor:  Rajeev Alur ( alur@cis ), Office hour: Wed 4-5, Levine 609  Teaching Assistants:    Jie Chen ( chenjr@seas ); Office hour: Tues 8-9, Moore 100C   Lucas Dagostino ( lucasdag@seas ); Office hour: Mon 7-8, Towne 305   Jungi Kim ( jungikim@sas ); Office hour: Tues 7.30-8.30, Towne 307   Derick Olson ( dericko@sas ); Office hour: Wed 5-6, Moore 100C   Omar Paladines ( omarp@sas ); Office hour: Mon 11-12, Moore 100B   Alex Piatski ( alpi@seas ); Office hour: Mon 6-7 and Tues 5-6, both in Moore 100C   Kelly Tan ( kellytan@seas ); Office hour: Wed 12-1, Towne 315    Recitation: Monday 4.30 -- 5.30, DRLB A1  Pre-requisites: CIS 160          Textbooks             Required: Introduction to the theory of computation, MichaelSipser, Third Edition, 2012, or Second Edition, 2006.         Additional reference: Introduction to Automata Theory, Languagesand Computation, J.E. Hopcroft, R. Motwani, and J.D. Ullman, Addison Wesley, Third edition, 2006.      Grading  Grades will be based on    Weekly Homework Assignments:  10 total, 10 pts each        Two Midterms:  50 pts each        Final Exam: 100 pts        Schedule      Aug 27: Course logistics, Discussion of course content, Introduction to    automata  Sept 1, 3, 8, 10, 15, 17, 22, 24: Finite  automata and regular languages (Chapter 1)    Oct 1: Midterm 1 (in class)   Oct 6, 13, 15, 20, 22, 27, 29; Nov 3: Turing machines and undecidability (Chapters 3, 4, 5)     Nov 5: Midterm 2  (in class)    Nov 10, 17, 19, 24; Dec 1, 3, 8:  Complexity classes and NP-completeness (Chapters 7 and 8)    Dec 18, 12 - 2: Final Exam         Notes       We will use  Piazza  for discussions.Homeworks, lecture slides, and handouts will also be posted on Piazza.   Grades are posted on  canvas .   For a few problems, we will use the tool  AutomataTutor .Sign up for an account on AutomataTutor, make sure that name and email on this account matches your Penn account.  During the class, use of mobile devices and laptops is prohibited.   Recitation: During Monday's recitation, we will mainly focus on solvingproblems. Many students find the homeworks difficult, and even when they havesolved the problem, some find it difficult to write the answer concisely andrigorously. Recitation should help with these challenges. However, attendancefor recitation is optional as no new material will be covered.  Homework Drop-off and Pick-up: On the day the homework is due, it should besubmitted before the lecture starts.Graded exams and homeworks can be collected from Laura Fox in Levine 308( lffox@seas ) during Mon--Fri, 9--12 and 2--4pm.  Plagiarism Policy: For homeworks, you can use your class notes, the textbook, and the reference book, but not old solutions, friends, other books, or any other material from the web. For violations of this rule, you will be reported to the Office of  Student Conduct at Penn.Start working on homework problems early, and if you get stuck, contact one ofus; we will be happy to help you progress.  Open book exams: Both midterms and final exam will be open book: during the exam, you can consultthe textbook, your class notes, and handouts distributed during the course, but are not allowed toaccess the internet.          Maintained by Rajeev Alur"

helloRegex :: Regex
helloRegex = mkRegex "hello"

orRegex :: Regex
orRegex = mkRegex "hello|hey|hi"

tParseDocFail :: Test
tParseDocFail = TestCase $ 
  do out <- parseDoc "http://www.seas.upenn.edu/~cse262/"
                     "CANNOTFINDQUERY" 
                     testHTML
     assertEqual "parse cis262 page" 
        out (validUrlsForParseDoc, "")

tParseDocSuccess :: Test
tParseDocSuccess = TestCase $ 
  do out <- parseDoc "http://www.seas.upenn.edu/~cse262/"
                     "THEORY TURING potato" 
                     testHTML
     assertEqual "parse cis262 page" 
        out (validUrlsForParseDoc, outputStringForParseDoc)

tListUrls :: Test 
tListUrls = listUrls "google.com" ["relpath.txt", 
                                   "www.stillarelpath.com", 
                                   "http://validsite.com", 
                                   "https://ignoreme.com", 
                                   "javascript:void(0)",
                                   "ftp://noparse",
                                   "HTTP://HI.com",
                                   "mailto:kchen2013@gmail.com"] 
                     ~?= ["http://google.com/relpath.txt",
                          "http://google.com/www.stillarelpath.com",
                          "http://validsite.com",
                          "http://hi.com"]

tRetPageContents :: Test
tRetPageContents = TestCase $ 
  do out <- retPageContents "fluorescent orange computability "
                            testHTML
     assertEqual "check if any of these words are in page" 
        out ("    " ++ outputStringForParseDoc ++ "  ")

tQuerySuccess :: Test 
tQuerySuccess = "check if regex is found in the string" ~:
  TestList [ querySuccess helloRegex "How is it going?" ~?= "",
             querySuccess helloRegex "hello!"           ~?= "hello!",
             querySuccess orRegex    "laheyyyyyy!"      ~?= "laheyyyyyy!",
             querySuccess orRegex    "appleHElLo!"      ~?= "appleHElLo!",
             querySuccess helloRegex "nothing here"     ~?= ""]

tOrOperation :: Test 
tOrOperation = querySuccess (orOperation ["hello", "hey", "hi"]) "oh hey!" ~?=
               querySuccess orRegex                              "oh hey!"

tLowercase :: Test
tLowercase = lowercase "HI how 1234 ArE you" ~?= "hi how 1234 are you"

tTrim :: Test 
tTrim = "trim test cases" ~: 
  TestList [trim ""  ~?= "", 
            trim "  hi   do   "  ~?= "hi   do"]

tRmTrail :: Test 
tRmTrail = "remove trailing spaces" ~:
  TestList [rmTrail "" ""                 ~?= "", 
            rmTrail "" "   hi   do   "    ~?= "   hi   do",
            rmTrail "IH" "   hi   do   "  ~?= "HI   hi   do"]

-- | test strings for the robots.txt parser
tRobotsTxtFns :: Test
tRobotsTxtFns = TestList 
  [tParseComment, tParseMultiComment, tParseNotOurUserAgent, 
   tParseTgtUserAgent, tParseRobotsTxt]

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
tParseComment = "1-line comment" ~: parse commentP comment ~?= Right Comment

tParseMultiComment :: Test
tParseMultiComment = "multiline comment" ~: parse multiCommentP multilineComment 
                                         ~?= Right Comment

tParseNotOurUserAgent :: Test
tParseNotOurUserAgent = "parser ignores this user agent" ~: TestList [
  "not our agent success"   ~: parse notOurUserAgentP notOurUserAgent 
                            ~?= Right [],
  -- | our "not our user agent" parser should fail when presented with the 
  --   agent we should be parsing for.
  "not our agent fail"      ~: parse notOurUserAgentP validUserAgent
                            ~?= Left "No parses" ]


tParseTgtUserAgent :: Test
tParseTgtUserAgent = "parser returns this agent's info" ~: TestList [
  "tgt agent success"    ~: parse userAgentP validUserAgent
                  ~?= Right [Disallow "/search",   Allow "/search/about",
                             Disallow "/sdch",     Disallow "/groups", 
                             Disallow "/catalogs", Allow "/catalogs/about", 
                             Allow "/catalogs/p?", Disallow "/catalogues"],
   "tgt agent crawl del" ~: parse userAgentP userAgentCrawlDelay
                  ~?= Right [CrawlDelay 10,         
                             Disallow "/search",   Allow "/search/about",
                             Disallow "/sdch"],
   "not our agent fail"  ~: parse userAgentP notOurUserAgent
                  ~?= Left "No parses" ]


tParseRobotsTxt :: Test
tParseRobotsTxt = "parser for the robots.txt" ~: TestList [
  "simple comments and agents" ~: parse robotP agentCommentMix 
            ~?= Right [[], [Disallow "/"]],
  "agent with in-line comment" ~: parse robotP userAgentInlineComment
            ~?= Right [[Disallow "/search",   Allow "/search/about",
                        Disallow "/sdch"]],
  "example robots.txt file"    ~: parse robotP validRobotsPg
            ~?= Right [[],[],
                      [Disallow "/search",   Allow "/search/about",
                       Disallow "/sdch",     Disallow "/groups",
                       Disallow "/catalogs", Allow "/catalogs/about",
                       Allow "/catalogs/p?", Disallow "/catalogues",
                       Comment,
                       Disallow "/wiki/Wikipedia:Strony_do_usuni%C4%99cia"]],
  "robots with sitemap"       ~: parse robotP fullRobotsTxt
            ~?= Right [[],
                       [CrawlDelay 10,         
                             Disallow "/search",   Allow "/search/about",
                             Disallow "/sdch"]] ]


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

tUrlTests :: Test
tUrlTests = TestList
            [tDom1, tDom2, tDom3, tDom4, tDom5, tDom6, tDom7, tDom8, tRelPath1,
             tRelPath2, tRelPath3, tRelPath4, tRelPath5, tRelPath6, tRelPath7,
             tType1, tType2, tType3, tType4, tType5, tType6, tMatchPath1,
             tMatchPath2, tMatchPath3, tMatchPath4, tMatchPath5, tMatchPath6,
             tMatchPath7, tMatchPath8, tMatchPath9, tMatchPath10, tMatchPath11,
             tMatchPath12, tMatchPath13, tMatchPath14, tMatchPath15,
             tMatchPath16, tMatchPath17, tMatchPath18, tCheckProt1, tCheckProt2,
             tCheckProt3, tCheckProt4, tCheckProt5]

tPostProcTests :: Test
tPostProcTests = TestList
                 [tTrimNonAlpha1, tTrimNonAlpha2, tTrimNonAlpha3,
                  tTrimNonAlpha4, tFindWords1, tFindWords2, tNumOccur1,
                  tNumOccur2, tNumDistinct1, tNumDistinct2, tAvgDist1,
                  tAvgDist2, tGetPgValue1, tGetPgValue2, tRankPages1]

main :: IO ()
main = do
  _ <- runTestTT tUrlTests
  _ <- runTestTT tPostProcTests
  _ <- runTestTT tParseWebpageFns
  _ <- runTestTT tRobotsTxtFns
  _ <- runTestTT tDownloaderTests
  return ()

