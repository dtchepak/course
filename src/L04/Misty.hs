module L04.Misty where

import L01.Optional
import L01.Validation
import L02.List
import L03.Parser


class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use banana and unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 5
-- Relative Difficulty: 2
instance Misty List where
  banana = flatMap
  unicorn a = a:|Nil

-- Exercise 6
-- Relative Difficulty: 2
instance Misty Optional where
  banana _ Empty = Empty
  banana f (Full a) = f a
  unicorn a = Full a

-- Exercise 7
-- Relative Difficulty: 3
instance Misty Parser where
  banana f p = bindParser p f
  unicorn = valueParser

-- Exercise 8
-- Relative Difficulty: 2
jellybean :: Misty m => m (m a) -> m a
jellybean = banana id

-- Exercise 9
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
sausage [] = unicorn []
sausage (h:t) = banana (\a ->
                   banana (\a' -> unicorn (a:a')) (sausage t)) h
{- need to revist abouve. Use furry' and sausage -}

-- Exercise 10
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
moppy f = sausage . furry' f

-- Exercise 11
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar n = furry' (replicate n)

-- Exercise 12
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
filtering _ [] = unicorn []
filtering f (x:xs) = undefined

-- Exercise 13
-- Relative Difficulty: 10
apple :: Misty m => m (a -> b) -> m a -> m b
apple f x = banana (\f' -> furry' f' x) f

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
lemon2 :: Misty m => (a -> b -> c) -> m a -> m b -> m c
lemon2 = error "todo"

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use apple + lemon2)
lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lemon3 = error "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + lemon3)
lemon4 :: Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lemon4 = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Misty [] where
  banana = concatMap
  unicorn = return
