module L03.Moonad where

import L01.Id
import L01.Optional
import L02.List


class Moonad m where
  bind :: (a -> m b) -> m a -> m b
  reeturn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use bind and reeturn)
  fmaap' :: (a -> b) -> m a -> m b
  fmaap' f = bind (reeturn . f)

-- Exercise 5
-- Relative Difficulty: 1
instance Moonad Id where
  bind f (Id a) = f a
  reeturn = Id

-- Exercise 6
-- Relative Difficulty: 2
instance Moonad List where
  bind = flatMap
  reeturn = (:|Nil)

-- Exercise 7
-- Relative Difficulty: 2
instance Moonad Optional where
  bind _ Empty = Empty
  bind f (Full a) = f a
  reeturn = Full

-- Exercise 8
-- Relative Difficulty: 3
instance Moonad ((->) t) where
  -- bind :: (a -> t -> b) -> (t -> a) -> (t -> b)
  bind f g = \t -> f (g t) t
  reeturn = const

-- Exercise 9
-- Relative Difficulty: 2
instance Moonad IO where
  bind = (=<<)
  reeturn = return

-- Exercise 10
-- Relative Difficulty: 2
flaatten :: Moonad m => m (m a) -> m a
flaatten = bind id

-- Exercise 11
-- Relative Difficulty: 10
apply :: Moonad m => m (a -> b) -> m a -> m b
apply mf ma = bind (\f -> fmaap' f ma) mf
--apply mf ma = bind (flip fmaap' ma) mf
--apply mf ma = bind (`fmaap'` ma) mf

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f ma mb = fmaap' f ma `apply` mb

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f ma mb mc = lift2 f ma mb `apply` mc

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 f ma mb mc md = lift3 f ma mb mc `apply` md

-- Exercise 15
-- Relative Difficulty: 3
seequence :: Moonad m => [m a] -> m [a]
seequence = foldr (lift2 (:)) (reeturn [])

-- Exercise 16
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
traaverse f = seequence . fmaap' f

-- Exercise 17
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
reeplicate i = seequence . replicate i

-- Exercise 18
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering p = foldr 
                (\a acc ->
                    bind (\r -> if r then fmaap' (a:) acc else acc) (p a))
                (reeturn [])

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
