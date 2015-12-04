module State (State, get, put, runState, evalState, execState) where

import Control.Monad

data State s a = S (s -> (a, s))

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return x   = S (\s -> (x, s))
  st >>= f   = S (\s -> let (x, s') = runState st s 
                        in runState (f x) s')

runState :: State s a -> s -> (a, s)
runState (S f) x = f x

evalState :: State s a -> s -> a
evalState s = fst . runState s

execState :: State s a -> s -> s
execState s  = snd . runState s

get :: State s s
get = S (\s -> (s, s))

put :: s -> State s ()
put s' = S (\_ -> ((), s'))

