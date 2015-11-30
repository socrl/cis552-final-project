{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- The basic definition of the parsing monad as developed in lecture.
-- Operations for building composite parsers are in the module
-- ParserCombinators.

module Parser (Parser,                  
                   get,
                   choose,
                   eof,
                   doParse,  
                   ) where

import Control.Applicative
import Control.Monad(ap)

newtype Parser a = P (String -> [(a, String)])

doParse :: Parser a -> String -> [(a, String)] 
doParse (P p) s = p s

-- | Return the next character from the input
-- (this was called 'oneChar' in lecture)
get :: Parser Char
get = P (\cs -> case cs of 
                (x:xs) -> [ (x,xs) ]
                []     -> [])
               
-- | This parser *only* succeeds at the end of the input.
eof :: Parser ()
eof = P $ \cs -> case cs of
                  []  -> [((),[])]
                  _:_ -> []

instance Monad Parser where
   p1 >>= fp2 = P (\cs -> do (a,cs') <- doParse p1 cs 
                             doParse (fp2 a) cs') 

   return x   = P (\cs -> [ (x, cs) ])

   fail _     = P (\_ ->  [ ] )

instance Applicative Parser where  
   pure   = return
   (<*>)  = ap

instance Functor Parser where
   fmap = liftA

-- | Combine two parsers together in parallel, producing all 
-- possible results from either parser.                 
choose :: Parser a -> Parser a -> Parser a
p1 `choose` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)

instance Alternative Parser where  
  -- the parser that always fails
  empty     = P $ \ _ -> [] 
  -- | Combine two parsers together in parallel, but only use the 
  -- first result. This means that the second parser is used only 
  -- if the first parser completely fails. 
  p1 <|> p2 = P $ \cs -> case doParse (p1 `choose` p2) cs of
                          []   -> []
                          x:_ -> [x]
  