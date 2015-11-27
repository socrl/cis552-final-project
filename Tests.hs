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
agentCommentMix = multilineComment ++ notOutUserAgent ++ "\n# Wikipedia work bots:\nUser-agent: *\nDisallow:\n"

validUserAgent :: String 
validUserAgent = "User-agent: *\nDisallow: /search\nAllow: /search/about\nDisallow: /sdch\nDisallow: /groups\nDisallow: /catalogs\nAllow: /catalogs/about\nAllow: /catalogs/p?\nDisallow: /catalogues\nAllow: /newsalerts\nDisallow: /news\nAllow: /news/directory\nDisallow: /nwshp\nDisallow: /setnewsprefs?\nDisallow: /index.html?\nDisallow: /?\nAllow: /?hl=\nDisallow: /?hl=*&\nAllow: /?hl=*&gws_rd=ssl$\nDisallow: /?hl=*&*&gws_rd=ssl\nAllow: /?gws_rd=ssl$\nAllow: /?pt1=true$\nDisallow: /addurl/image?\nAllow:    /mail/help/\nDisallow: /mail/\nDisallow: /pagead/\nDisallow: /relpage/\nDisallow: /relcontent\nDisallow: /imgres\nDisallow: /imglanding\nDisallow: /sbd\nDisallow: /keyword/\nDisallow: /u/\nDisallow: /univ/\nDisallow: /cobrand\nDisallow: /custom\nDisallow: /advanced_group_search\nDisallow: /googlesite\nDisallow: /preferences\nDisallow: /setprefs\nDisallow: /swr\nDisallow: /url\nDisallow: /default\nDisallow: /m?\nDisallow: /m/\nAllow:    /m/finance\nDisallow: /wml?\nDisallow: /wml/?\nDisallow: /wml/search?\nDisallow: /xhtml?\nDisallow: /xhtml/?\nDisallow: /xhtml/search?\nDisallow: /xml?\nDisallow: /imode?\nDisallow: /imode/?\nDisallow: /imode/search?\nDisallow: /jsky?\nDisallow: /jsky/?\nDisallow: /jsky/search?\nDisallow: /pda?\nDisallow: /pda/?\nDisallow: /pda/search?\nDisallow: /sprint_xhtml\nDisallow: /sprint_wml\nDisallow: /pqa\nDisallow: /palm\nDisallow: /gwt/\nDisallow: /purchases\nDisallow: /bsd?\nDisallow: /linux?\nDisallow: /mac?\nDisallow: /microsoft?\nDisallow: /unclesam?\nDisallow: /answers/search?q=\nDisallow: /local?\nDisallow: /local_url\nDisallow: /shihui?\nDisallow: /shihui/\nDisallow: /froogle?\nDisallow: /products?\nDisallow: /froogle_\nDisallow: /product_\nDisallow: /products_\nDisallow: /products;\nDisallow: /print\nDisallow: /books/\nDisallow: /bkshp?*q=*\nDisallow: /books?*q=*\nDisallow: /books?*output=*\nDisallow: /books?*pg=*\nDisallow: /books?*jtp=*\n"

userAgentInlineComment :: String
userAgentInlineComment = "User-agent: *\nDisallow: /search\nAllow: /search/about # COMMENTARY\nDisallow: /sdch\n"

validRobotsPg :: String
validRobotsPg = multilineComment ++ notOurUserAgent ++ "\nUser-agent: Orthogaffe\nDisallow:\n" ++ comment ++ validUserAgent ++ comment ++ "Disallow: /wiki/Wikipedia:Strony_do_usuni%C4%99cia\n"

