module L03.Misty where

import L01.Id
import L01.Optional
import L02.List


class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use banana and unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)
  skittle :: m a -> m b -> m b
  skittle ma mb = (const mb) `banana` ma


-- Exercise 5
-- Relative Difficulty: 1
instance Misty Id where
  banana = error "todo"
  unicorn = error "todo"

-- Exercise 6
-- Relative Difficulty: 2
instance Misty List where
  banana = flatMap
  unicorn = (:|Nil)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty Optional where
  banana _ Empty = Empty
  banana f (Full a) = f a
  unicorn = Full

-- Exercise 8
-- Relative Difficulty: 3
instance Misty ((->) t) where
  --banana :: (a -> t -> b) -> (t -> a) -> (t -> b)
  banana f g = \t -> f (g t) t
  unicorn = const

-- Exercise 9
-- Relative Difficulty: 2
jellybean :: Misty m => m (m a) -> m a
jellybean = banana id

-- Exercise 9
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
sausage = foldr (\x acc -> banana (\a -> furry' (a:) acc) x) (unicorn [])
--sausage [] = unicorn []
--sausage (x:xs) = banana (\a -> furry' (a:) (sausage xs)) x

-- Exercise 10
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
moppy f = sausage . map f

-- Exercise 11
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar n = sausage . replicate n

-- Exercise 12
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
filtering _ [] = unicorn []
filtering p (h:t) = 
    let include = lemon2 (:) (unicorn h) (filtering p t)
    in banana (\pick -> if pick then include else filtering p t) (p h)

-- Relative Difficulty: 10
apple :: Misty m => m (a -> b) -> m a -> m b
--apple mf ma = jellybean $ furry' (\f -> furry' f ma) mf
apple mf ma = (`furry'` ma) `banana` mf

-- Exercise 11
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
lemon2 :: Misty m => (a -> b -> c) -> m a -> m b -> m c
lemon2 f ma mb = f `furry'` ma `apple` mb

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apple + lemon2)
lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lemon3 f ma mb mc = lemon2 f ma mb `apple` mc

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apple + lemon3)
lemon4 :: Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lemon4 f ma mb mc md = lemon3 f ma mb mc `apple` md

-- Exercise 14
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
sausage = error "todo"

-- Exercise 15
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
moppy = error "todo"

-- Exercise 16
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar = error "todo"

-- Exercise 17
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Misty [] where
  banana = concatMap
  unicorn = return
