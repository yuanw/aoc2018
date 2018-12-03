-- learn state monad
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State
module Dice where

import Control.Applicative
import System.Random

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftA2 (,) (randomRIO (1,6)) (randomRIO (1,6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO 0 = return []
rollNDiceIO n = liftA2 (:) (randomRIO (1,6)) (rollNDiceIO (n - 1))

clumsyRollDice :: (Int, Int)
clumsyRollDice = (n, m)
        where
        (n, g) = randomR (1,6) (mkStdGen 0)
        (m, _) = randomR (1,6) g

-- Use our own version for now
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    f `fmap` State x = State (\ s -> let (y, s') = x s in (f y, s'))

instance Applicative (State s) where
    pure a = State (\ s -> (a, s))
    (State h) <*> (State g) = State (\ s -> let (f, s') = h s
                                                (a, s'') = g s' in (f a, s''))

instance Monad (State s) where
    (State g) >>= mf = State (\ s -> let (a, s') = g s
                                         (b, s'') = runState (mf a) s' in (b, s''))

state :: (s -> (a, s)) -> State s a
state = State

put :: s -> State s ()
put newState = state $ \_ -> ((), newState)

get :: State s s
get = state $ \s -> (s, s)

evalState :: State s a -> s -> a
evalState p s = fst (runState p s)

execState :: State s a -> s -> s
execState p s = snd (runState p s)

-- -- The StdGen type we are using is an instance of RandomGen.
-- randomR :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
-- randomR = undefined


rollDie :: State StdGen Int
rollDie = state $ randomR (1, 6)

rollDie' :: State StdGen Int
rollDie' = do generator <- get
              let (value, newGenerator) = randomR (1,6) generator
              put newGenerator
              return value
