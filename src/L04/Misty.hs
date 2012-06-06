module L04.Misty where

import L01.Optional
import L01.Validation
import L02.List
import L03.Parser


class Misty m where
  banana :: (a -> m b) -> m a -> m b    -- bind / flatMap / (=<<) / SelectMany
  unicorn :: a -> m a                   -- unit, pure, point, return
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use banana and unicorn)
  furry' :: (a -> b) -> m a -> m b      -- fmap
  furry' f = banana (unicorn . f)

-- Exercise 5
-- Relative Difficulty: 2
instance Misty List where
  banana = flatMap
  unicorn a = a :| Nil

instance Misty ((->) t) where
  -- (a -> (t -> b)) -> (t -> a) -> (t -> b)
  banana f g = \t -> f (g t) t
  -- a -> (t -> a)
  unicorn = const

-- Exercise 6
-- Relative Difficulty: 2
instance Misty Optional where
  banana f (Full x) = f x
  banana _ Empty = Empty
  unicorn = Full

-- Exercise 7
-- Relative Difficulty: 3
instance Misty Parser where
  banana = flip bindParser
  unicorn = valueParser

-- Exercise 8
-- Relative Difficulty: 2
jellybean :: Misty m => m (m a) -> m a  --flatten / join
jellybean = banana id

-- Exercise 9
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
-- TAKE 1
--sausage [] = unicorn []
--sausage (m:ms) = banana (\a -> banana (\as -> unicorn (a:as)) (sausage ms)) m
--sausage (m:ms) = banana (\a -> banana (\as -> unicorn ((:) a as)) (sausage ms)) m
--sausage (m:ms) = banana (\a -> banana (\as -> (unicorn . ((:) a)) as)) (sausage ms)) m
--sausage (m:ms) = banana (\a -> banana (unicorn . ((:) a)) (sausage ms)) m
--sausage (m:ms) = banana (\a -> furry' (a:) (sausage ms)) m
-- TAKE 2
--sausage [] = unicorn []
--sausage (m:ms) = banana (\as -> banana (\a -> unicorn (a:as)) m) (sausage ms)
-- TAKE 3
--sausage [] = unicorn []
--sausage (m:ms) = banana (\as -> banana (\a -> (unicorn . ((:) a)) as) m) (sausage ms)
--sausage (m:ms) = banana (\as -> furry' (\f -> (f:as)) m) (sausage ms)
--sausage (m:ms) = banana (\as -> furry' (:as) m) (sausage ms)
-- TAKE 4
--sausage ms = foldr (\ma mas -> banana (\a -> furry' (a:) mas) ma) (unicorn []) ms
sausage = foldr (lemon2 (:)) (unicorn [])

-- Exercise 10
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
moppy = error "todo"

-- Exercise 11
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar = error "todo"

-- Exercise 12
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-- Exercise 13
-- Relative Difficulty: 10
apple :: Misty m => m (a -> b) -> m a -> m b        -- ap / <*> (starship)
apple f ma = banana (\f' -> 
             banana (\a -> unicorn (f' a)) ma ) f

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
lemon2 :: Misty m => (a -> b -> c) -> m a -> m b -> m c     -- liftM2
lemon2 f ma mb = banana (\a -> 
                 banana (\b -> 
                 unicorn (f a b)) mb) ma

lemon2' :: Misty m => (a -> b -> c) -> m a -> m b -> m c     -- liftM2
--lemon2' f ma mb = (furry' f ma) `apple` mb
lemon2' f = apple . furry' f


-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use apple + lemon2)
lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lemon3 f a b c = 
    banana (\a' ->
    banana (\b' ->
    banana (\c' ->
    unicorn (f a' b' c')) c) b) a

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
