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
  unicorn = (:|Nil)

-- Exercise 6
-- Relative Difficulty: 2
instance Misty Optional where
  banana _ Empty = Empty
  banana f (Full a) = f a
  unicorn = Full

-- Exercise 7
-- Relative Difficulty: 3
instance Misty Parser where
  banana = flip bindParser
  unicorn = valueParser

-- Exercise 8
-- Relative Difficulty: 2
jellybean :: Misty m => m (m a) -> m a
jellybean = banana id

-- Exercise 9
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
sausage = foldr (apple . furry' (:)) (unicorn [])
--    let combine :: Misty m => m a -> m [a] -> m [a]
--        combine m acc = apple (furry' (:) m) acc 
--    in foldr combine (unicorn [])

--    let combine :: Misty m => m a -> m [a] -> m [a]
--        combine m acc = 
--            banana (\as ->
--                banana (\a -> unicorn (a:as)) m) acc
--    in foldr combine (unicorn [])

--sausage [] = unicorn []
--sausage (m:ms) = banana (\as -> 
--                    banana (\a -> unicorn (a:as)) m) (sausage ms)

-- Exercise 10
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
moppy f = sausage . furry' f

-- Exercise 11
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar i = sausage . replicate i 

-- Exercise 12
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
--filtering _ [] = unicorn []
--filtering predM (a:as) =
--    let mOk = predM a
--        rest = filtering predM as -- :: m [a]
--    in banana (\ok -> if ok then lemon2 (:) (unicorn a) rest else rest) mOk

filtering predM = 
    let (|:) = lemon2 (:)
        combine x acc = banana (\ok -> if ok then (unicorn x)|:acc else acc) (predM x)
    in foldr combine (unicorn [])


-- Exercise 13
-- Relative Difficulty: 10
apple :: Misty m => m (a -> b) -> m a -> m b
--apple f ma = banana (\f' -> 
--                  banana (unicorn . f') ma) f
--NOTE: banana (unicorn . f') = furry' f'
--apple f ma = banana (\f' -> furry' f' ma) f
apple f ma = banana (flip furry' ma) f
--apple f = flip banana f . flip furry'

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
lemon2 :: Misty m => (a -> b -> c) -> m a -> m b -> m c
lemon2 f ma mb = furry' f ma `apple` mb

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use apple + lemon2)
lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lemon3 f ma mb mc = lemon2 f ma mb `apple` mc

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + lemon3)
lemon4 :: Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lemon4 f ma mb mc md = furry' f ma `apple` mb `apple` mc `apple` md

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Misty [] where
  banana = concatMap
  unicorn = return
