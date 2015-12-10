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

type Robot = ([LineInfo], Int)
type RelPath = String
data LineInfo =
    Allow RelPath
  | Disallow RelPath
  | Comment 
  | CrawlDelay Int 
  deriving (Eq, Show)

-- | Given an absolute URL, a query, and an HTML string.
--   Return: ([list of absolute URLs in doc], text of matched body)
parseDoc :: String -> String -> String -> IO ([String], String)
parseDoc _   _     ""       = return $ ([], [])
parseDoc url query fulltext = 
  case U.getDomain url of 
    Nothing     -> error "Cannot retrieve valid domain"
    Just domain -> 
      do let doc   = readString [withParseHTML yes, withWarnings no] fulltext
         let iostr = runX $ doc >>> css "a" >>> getAttrValue "href"
         sl <- iostr
         sps <- retPageContents query fulltext
         return ((listUrls domain sl), trim sps)

listUrls :: String -> [String] -> [String]
listUrls domain sl = 
  filter isHttp (map (convertToAbsUrl domain) (filter ignorePrefixes sl)) where
    -- | helpers upon helpers
    convertToAbsUrl d s = case stripPrefix "http" (lowercase s) of 
                            Nothing -> "http://" ++ d ++ "/" ++ s
                            Just _  -> lowercase s  
    isHttp s            = case stripPrefix "https" (lowercase s) of 
                            Nothing -> True
                            Just _  -> False
    ignorePrefixes s    = not (mailto s || ftp s || js s) where
      mailto     = prefIn "mailto" 
      ftp        = prefIn "ftp" 
      js         = prefIn "javascript" 
      prefIn x y = isPrefixOf x (lowercase y)
     
lowercase :: String -> String 
lowercase = map toLower 

-- | query must be a space separated list of words
retPageContents :: String -> String -> IO String
retPageContents query fulltext = 
  do let doc   = readString [withParseHTML yes, withWarnings no] fulltext
     let iostr = runX . xshow $ doc >>> (css "body")
     sl <- iostr
     return $ querySuccess (orOperation (words $ lowercase query)) 
                           (stripSpaceDelims . stripTags $ concat sl) where
  -- | helpers
  stripSpaceDelims          = subCustomRegex "\n|\r|\t" "" 
  stripTags                 = subCustomRegex "<[^>]*>" " "
  subCustomRegex expr sub s = let rx = mkRegex expr in
    subRegex rx s sub

orOperation :: [String] -> Regex
orOperation sl = mkRegexWithOpts (concat $ intersperse "|" sl) True False

concatTexts :: [String] -> String
concatTexts = concatMap (\x -> x ++ " ")

querySuccess :: Regex -> String -> String
querySuccess rx s = case matchRegex rx (lowercase s) of 
  Nothing -> ""
  Just _  -> s 

trim :: String -> String
trim w = noSp "" $ dropWhile isSpace w

noSp :: String -> String -> String
noSp _ ""             = ""
noSp m (x:xs)
    | isSpace x       = noSp (x:m) xs
    | null m          = x:noSp "" xs
    | otherwise       = reverse m ++ x:noSp "" xs

{-
-- | separate helper function specifies the # words we take on either side 
--   so that we can use small parameters when testing the function.
snips :: Regex -> Int -> String -> [String]
snips rx len s = 
  case matchRegexAll rx s of 
    Nothing -> [] 
    Just (b, m, a, _) -> 
       let n1 = min (length $ words b) len in 
       let n2 = min (length $ words a) len in 
       let snip1 = foldl (const . drop 1) (words b) (drop n1 (words b)) in
       let (snip2, remain) = splitAt n2 (words a) in
       let snip = unwords snip1 ++ " " ++ m ++ " " ++ unwords snip2 in
       trim snip:(snips rx len (unwords remain))
-}

-- | PARSE ROBOTS.TXT 

parseRobot :: String -> Robot
parseRobot s  = 
  case P.parse robotP s of 
    Left _  -> ([], 1)
    Right l -> let ua = foldr (\x rs -> if x == [] then rs else x++rs) [] l in
      (retRobot ua, crawlDel ua) where 
      retRobot []                     = []
      retRobot (a@(Allow _):xs)       = a:retRobot xs
      retRobot (da@(Disallow _):xs)   = da:retRobot xs
      retRobot (_:xs)                 = retRobot xs 

      crawlDel []                  = 1
      crawlDel ((CrawlDelay n):_)  = n 
      crawlDel (_:xs)              = crawlDel xs

robotP :: P.Parser [[LineInfo]]
robotP = many (notOurUserAgentP <|> userAgentP)

-- | Applies a parser and then skips over any whitespace directly after it
wsP :: P.Parser a -> P.Parser a
wsP p = do n <- p 
           _ <- many P.space             
           return n

userAgentP :: P.Parser [LineInfo]
userAgentP = do _ <- multiCommentP
                _ <- wsP $ isUserAgent
                _ <- wsP $ P.char '*'
                out <- lineInfoP
                return out

notOurUserAgentP :: P.Parser [LineInfo]
notOurUserAgentP = do _ <- multiCommentP
                      _ <- wsP $ isUserAgent
                      _ <- P.satisfy (/= '*')
                      _ <- wsP $ untilEOL
                      _ <- lineInfoP
                      return []

lineInfoP :: P.Parser [LineInfo]
lineInfoP = many (allowUrlP <|> disallowUrlP <|> crawlDelayP <|> commentP)

disallowUrlP :: P.Parser LineInfo 
disallowUrlP = do _ <- wsP $ P.string "Disallow:"
                  l <- relUrlP 
                  _ <- wsP $ untilEOL
                  return $ Disallow l 

allowUrlP :: P.Parser LineInfo 
allowUrlP = do _ <- wsP $ P.string "Allow:"
               l <- relUrlP 
               _ <- wsP $ untilEOL
               return $ Allow $ l 

crawlDelayP :: P.Parser LineInfo
crawlDelayP = do _ <- wsP $ P.string "Crawl-delay:"
                 n <- P.int 
                 _ <- wsP $ untilEOL
                 return $ CrawlDelay n 

isUserAgent :: P.Parser String 
isUserAgent = P.string "User-agent:" <|> P.string "User-Agent:"

untilEOL :: P.Parser String
untilEOL = many $ P.satisfy (/= '\n')

relUrlP :: P.Parser String
relUrlP = many $ P.satisfy (\x -> x /= '\n' && x /= '#' && x /= ' ')

commentP :: P.Parser LineInfo
commentP = do _ <- P.char '#'
              _ <- wsP $ untilEOL
              return Comment

multiCommentP :: P.Parser LineInfo
multiCommentP = do _ <- many commentP
                   return Comment
