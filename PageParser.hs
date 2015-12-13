{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module PageParser where
import qualified Parser as P
import qualified ParserCombinators as P
import Control.Applicative (Alternative(..))
import Text.Regex
import Data.Char
import Text.XML.HXT.Core
import Text.HandsomeSoup
import qualified UrlUtils as U
import Data.List

-- | PARSE WEBPAGE HTML 

-- | Given: an absolute URL, a space-separated list of words to search for,
--         and a string of the HTML webpage.
--   Return: ([list of absolute URLs in page],
--            If any queried words present: text of body. Otherwise: "")
parseDoc :: String -> String -> String -> IO ([String], String)
parseDoc _   _     ""       = return ([], [])
parseDoc url query fulltext =
  case U.getDomain url of
    Nothing     -> error "Cannot retrieve valid domain"
    Just domain ->
      do let doc   = readString [withParseHTML yes, withWarnings no] fulltext
         let iostr = runX $ doc >>> css "a" >>> getAttrValue "href"
         sl <- iostr
         sps <- retPageContents query fulltext
         return (listUrls domain sl, trim sps)

-- | Given: domain and list of URLs found in the HTML.
--   Return: list of absolute URLs in page
listUrls :: String -> [String] -> [String]
listUrls domain sl =
  filter (isHttp . lowercase) $ map (convertToAbsUrl domain . lowercase)
                                    (filter ignorePrefixes sl) where
    -- | helpers
    convertToAbsUrl d s = case stripPrefix "http" s of
                            Nothing -> "http://" ++ lowercase d ++ "/" ++ s
                            Just _  -> s  
    -- HandsomeSoup can only handle HTTP, not HTTPS
    isHttp s            = case stripPrefix "https" s of 
                            Nothing -> True
                            Just _  -> False
    ignorePrefixes s    = not $ any (inPref s) avoidLinks where
      avoidLinks = ["mailto", "ftp", "javascript"]
      inPref y x = x `isPrefixOf` lowercase y

-- | Query must be a space separated list of words. 
--   Given: query and the full HTML text
--   Return: body of HTML if any queried words are present, 
--   otherwise the empty string (no matches found). 
retPageContents :: String -> String -> IO String
retPageContents query fulltext =
  do let doc   = readString [withParseHTML yes, withWarnings no] fulltext
     let iostr = runX . xshow $ doc >>> css "body"
     sl <- iostr
     return $ querySuccess (orOperation (words $ lowercase query))
                           (stripSpaceDelims . stripTags $ concat sl) where
  -- | helpers
  stripSpaceDelims          = subCustomRegex "\n|\r|\t" ""
  stripTags                 = subCustomRegex "<[^>]*>" " "
  subCustomRegex expr sub s = let rx = mkRegex expr in
    subRegex rx s sub

querySuccess :: Regex -> String -> String
querySuccess rx s = case matchRegex rx (lowercase s) of
  Nothing -> ""
  Just _  -> s

orOperation :: [String] -> Regex
orOperation sl = mkRegexWithOpts (intercalate "|" sl) True False

lowercase :: String -> String 
lowercase = map toLower 

trim :: String -> String
trim w = rmTrail "" $ dropWhile isSpace w

rmTrail :: String -> String -> String
rmTrail _ ""          = ""
rmTrail m (x:xs)
    | isSpace x       = rmTrail (x:m) xs
    | null m          = x:rmTrail "" xs
    | otherwise       = reverse m ++ x:rmTrail "" xs

-- | PARSE ROBOTS.TXT

type Robot = ([LineInfo], Int)
type RelPath = String
data LineInfo =
    Allow RelPath
  | Disallow RelPath
  | Comment 
  | CrawlDelay Int 
  deriving (Eq, Show)

-- | This is the full robots.txt parser. It takes in the robots.txt 
--   body and returns a tuple with allowed/disallowed URLs and the crawl-delay.
--   Crawl-delay defaults to 1 if none is given. 
parseRobot :: String -> Robot
parseRobot s  =
  case P.parse robotP s of
    Left _  -> ([], 1)
    Right l -> let ua = foldr (\x rs -> if null x then rs else x++rs) [] l in
      (retRobot ua, crawlDel ua) where
      retRobot []                     = []
      retRobot (a@(Allow _):xs)       = a:retRobot xs
      retRobot (da@(Disallow _):xs)   = da:retRobot xs
      retRobot (_:xs)                 = retRobot xs

      crawlDel []                  = 1
      crawlDel (CrawlDelay n:_)    = n 
      crawlDel (_:xs)              = crawlDel xs

robotP :: P.Parser [[LineInfo]]
robotP = many (notOurUserAgentP <|> userAgentP)

-- | we are only looking for "User-Agent: *"
userAgentP :: P.Parser [LineInfo]
userAgentP = do _ <- multiCommentP
                _ <- wsP isUserAgent
                _ <- wsP $ P.char '*'
                lineInfoP

notOurUserAgentP :: P.Parser [LineInfo]
notOurUserAgentP = do _ <- multiCommentP
                      _ <- wsP isUserAgent
                      _ <- P.satisfy (/= '*')
                      _ <- wsP untilEOL
                      _ <- lineInfoP
                      return []

lineInfoP :: P.Parser [LineInfo]
lineInfoP = many (allowUrlP <|> disallowUrlP <|> crawlDelayP <|> commentP)

disallowUrlP :: P.Parser LineInfo
disallowUrlP = do _ <- wsP $ P.string "Disallow:"
                  l <- relUrlP
                  _ <- wsP untilEOL
                  return $ Disallow l

allowUrlP :: P.Parser LineInfo
allowUrlP = do _ <- wsP $ P.string "Allow:"
               l <- relUrlP
               _ <- wsP untilEOL
               return $ Allow l

crawlDelayP :: P.Parser LineInfo
crawlDelayP = do _ <- wsP $ P.string "Crawl-delay:"
                 n <- P.int
                 _ <- wsP untilEOL
                 return $ CrawlDelay n

isUserAgent :: P.Parser String
isUserAgent = P.string "User-agent:" <|> P.string "User-Agent:"

untilEOL :: P.Parser String
untilEOL = many $ P.satisfy (/= '\n')

relUrlP :: P.Parser String
relUrlP = many $ P.satisfy (\x -> x /= '\n' && x /= '#' && x /= ' ')

commentP :: P.Parser LineInfo
commentP = do _ <- P.char '#'
              _ <- wsP untilEOL
              return Comment

multiCommentP :: P.Parser LineInfo
multiCommentP = do _ <- many commentP
                   return Comment

wsP :: P.Parser a -> P.Parser a
wsP p = do n <- p 
           _ <- many P.space             
           return n
