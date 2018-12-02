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

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    f `fmap` State x = State (\ s -> let (y, s') = x s in (f y, s'))

instance Applicative (State s) where
    pure a = State (\ s -> (a, s))
    (State h) <*> (State g) = State (\ s -> let (f, s') = h s
                                                (a, s'') = g s' in (f a, s''))
