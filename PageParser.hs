{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module PageParser where
import qualified Parser as P
import qualified ParserCombinators as P
import Control.Applicative (Alternative(..))
import Text.Regex
import Data.Char  

type Robot = ([LineInfo], Int)
type RelPath = String
data LineInfo =
    Allow RelPath
  | Disallow RelPath
  | Comment 
  | CrawlDelay Int 
  deriving (Eq, Show)

getSnips :: String -> String -> [String]
getSnips query fulltext = snips (mkRegex query) fulltext 15 

-- | separate helper function specifies the # words we take on either side 
--   so that we can use small parameters when testing the function.
snips :: Regex -> String -> Int -> [String]
snips rx s len = 
  case matchRegexAll rx s of 
    Nothing -> [] 
    Just (b, m, a, _) -> 
       let n1 = min (length $ words b) len in 
       let n2 = min (length $ words a) len in 
       let snip1 = foldl (const . drop 1) (words b) (drop n1 (words b)) in
       let (snip2, remain) = splitAt n2 (words a) in
       let snip = unwords snip1 ++ " " ++ m ++ " " ++ unwords snip2 in
       trim snip:(snips rx (unwords remain) len)

trim :: String -> String
trim w = noSp "" $ dropWhile isSpace w

noSp :: String -> String -> String
noSp _ "" = ""
noSp m (x:xs)
    | isSpace x       = noSp (x:m) xs
    | null m          = x:noSp "" xs
    | otherwise       = reverse m ++ x:noSp "" xs

parseRobot :: String -> Robot
parseRobot s = 
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
                _ <- wsP $ P.string "User-agent:"
                _ <- wsP $ P.char '*'
                out <- lineInfoP
                return out

notOurUserAgentP :: P.Parser [LineInfo]
notOurUserAgentP = do _ <- multiCommentP
                      _ <- wsP $ P.string "User-agent:"
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
