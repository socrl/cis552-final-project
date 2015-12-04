{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- CIS 552, University of Pennsylvania
-- based on Parsec and ReadP parsing libraries
module ParserCombinators where

import Parser
import Control.Applicative
import Control.Monad
import Data.Char
import System.IO
            
type ParseError = String

-- | Use a parser for a particular string. Note that this parser
-- combinator library doesn't support descriptive parse errors.
-- However, for compatibility with Parsec, we give this function 
-- the same type.
parse :: Parser a -> String -> Either ParseError a
parse parser str = case (doParse parser str) of 
    []      -> Left  "No parses"
    [(a,_)] -> Right a
    _       -> Left  "Multiple parses"
    
-- | parseFromFile p filePath runs a string parser p on the input 
-- read from filePath using readFile. Returns either a 
-- ParseError (Left) or a value of type a (Right).    
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do 
  handle <- openFile filename ReadMode 
  str <- hGetContents handle
  return $ parse parser str  
  
-- | Return the next character if it satisfies the given predicate
-- (this was called satP in lecture)
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- get
               if (p c) then return c else empty
    
-- | Parsers for specific sorts of characters 
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit            
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace
   
-- | Parses and returns the specified character        
-- succeeds only if the input is exactly that character
char :: Char -> Parser Char
char c = satisfy (c ==)   

-- | Parses and returns the specified string. 
-- Succeeds only if the input is the given string
string :: String -> Parser String
string = mapM char

-- | succeed only if the input is a (positive or negative) integer
int :: Parser Int
int = do n <- string "-" <|> return []
         s <- some digit  
         return $ (read (n ++ s) :: Int)

-- | @chainl p op x@ parses zero or more occurrences of @p@, separated by @op@.
--   Returns a value produced by a /left/ associative application of all
--   functions returned by @op@. If there are no occurrences of @p@, @x@ is
--   returned.
chainl :: Parser b -> Parser (b -> b -> b) -> b -> Parser b
chainl p op x = chainl1 p op <|> return x

-- | Like 'chainl', but parses one or more occurrences of @p@.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` pop = p >>= rest
    where rest x = next x <|> return x 
          next x = do o <- pop
                      y <- p
                      rest $ x `o` y 
                      
-- | Combine all parsers in the list (sequentially)
choice :: [Parser a] -> Parser a
choice = foldr (<|>) (fail "")

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is returned.
between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = do _ <- open
                          x <- p
                          _ <- close
                          return x

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))
