{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

module PageParser where
import qualified Parser as P
import qualified ParserCombinators as P
import Control.Applicative (Alternative(..))

type Robot = ([LineInfo], Int)
type RelPath = String
data LineInfo =
    Allow RelPath
  | Disallow RelPath
  | Comment 
  | CrawlDelay Int 
  deriving (Eq, Show)

getSnips :: String -> String -> [String]
getSnips = undefined

parseRobot :: String -> Robot
parseRobot = undefined

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
