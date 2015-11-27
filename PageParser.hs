{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}
{-# LANGUAGE FlexibleInstances #-}

module PageParser where
import qualified Parser as P
import qualified ParserCombinators as P

import Control.Applicative (Alternative(..))

type Robot = ([RelPath], Int)
type RelPath = String


getSnips :: String -> String -> [String]
getSnips = undefined

parseRobot :: String -> Robot
parseRobot _ = undefined {-retrieval $ lines s where
	retrieval []     = ([], 0)
	retrieval (l:ls) = case words l of-} 

-- | Applies a parser and then skips over any whitespace directly after it
wsP :: P.Parser a -> P.Parser a
wsP p = do n <- p 
           _ <- many P.space             
           return n

disallowUrlP :: P.Parser (String, Bool) 
disallowUrlP = do _ <- wsP $ P.string "Disallow:"
                  l <- relUrlP 
                  _ <- wsP $ untilEOL
                  return (l, False) 

allowUrlP :: P.Parser (String, Bool) 
allowUrlP = do _ <- wsP $ P.string "Allow:"
               l <- relUrlP 
               _ <- wsP $ untilEOL
               return (l, True) 

userAgentP :: P.Parser [(String, Bool)]
userAgentP = do _ <- many commentP
                _ <- wsP $ P.string "User-agent:"
                _ <- wsP $ P.char '*'
                out <- many (allowUrlP <|> disallowUrlP)
                return out

newline :: P.Parser () 
newline = do _ <- wsP $ P.char '\n'
             return ()

untilEOL :: P.Parser String
untilEOL = many $ P.satisfy (/= '\n')

relUrlP :: P.Parser String
relUrlP = many $ P.satisfy (\x -> x /= '\n' && x /= '#')

commentP :: P.Parser String
commentP = do _ <- P.char '#'
              c <- untilEOL
              _ <- newline
              return c
