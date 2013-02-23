{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

module L03.State where

import L01.Optional
import L02.List
import L03.Fuunctor
import L03.Moonad
import Control.Applicative
import Control.Arrow
import Data.Char
import qualified Data.Set as S
import qualified Data.Foldable as F

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- Exercise 1
-- Relative Difficulty: 2
-- Implement the `Fuunctor` instance for `State s`.
instance Fuunctor (State s) where
  fmaap f sa =
    State $ \s -> let (a,s') = runState sa s
                  in (f a, s')

-- Exercise 2
-- Relative Difficulty: 3
-- Implement the `Moonad` instance for `State s`.
-- Make sure the state value is passed through in `bind`.
instance Moonad (State s) where
  bind f sa =
    State $ \s -> let (a,s') = runState sa s
                  in runState (f a) s'
  reeturn =
    State . (,)
    -- OR:
    --      reeturn a = State (\s -> (a,s))
    --      reeturn a = State (a,)              -- by tuple section
    --      reeturn a = State ((,) a)           -- move (,) to prefix pos
    --      reeturn   = State . (,)             -- by (f.g) = \x -> f (g x)

-- Exercise 3
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting state.
exec ::
  State s a
  -> s
  -> s
exec sa = 
  snd . runState sa

-- Exercise 4
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting value.
eval ::
  State s a
  -> s
  -> a
eval sa =
  fst . runState sa

-- Exercise 5
-- Relative Difficulty: 2
-- A `State` where the state also distributes into the produced value.
get ::
  State s s
get =
  State $ \s -> (s,s)

-- Exercise 6
-- Relative Difficulty: 2
-- A `State` where the resulting state is seeded with the given value.
put ::
  s
  -> State s ()
put =
  State . const . ((),)

-- Exercise 7
-- Relative Difficulty: 5
-- Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Moonad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
findM ::
  Moonad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
--findM _ Nil = reeturn Empty
--findM f (h:|t) = bind (\b -> if b then reeturn (Full h) else findM f t) (f h)
findM f = foldRight (\h acc -> 
            bind (\b -> if b then reeturn (Full h) else acc) (f h)) 
            (reeturn Empty)

-- Exercise 8
-- Relative Difficulty: 4
-- Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
-- ~~~ Use findM and State with a Data.Set#Set. ~~~
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat =
  --Showing all details of how State is updated:
  --    flip eval S.empty 
  --      . findM (\x -> State (\s -> if x `S.member` s then (True,s) else (False, x `S.insert` s)))
  --Collapsing if statement:
  --    flip eval S.empty 
  --      . findM (\x -> State (\s -> (x `S.member` s,x `S.insert` s)))
  --Apply s to both sides of tuple (using (&&&) from Control.Arrow):
  --    flip eval S.empty
  --      . findM (\x -> State $ (x `S.member`) &&& (x `S.insert`))
  --Tidy up
  --    flip eval S.empty
  --      . findM (\x -> State $ S.member x &&& S.insert x)
  --c/o @dibblego
  flip eval S.empty
    . findM (State . liftA2 (&&&) S.member S.insert)

-- Exercise 9
-- Relative Difficulty: 5
-- Remove all elements in a `List` that fail a given predicate.
-- However, while performing the filter, we sequence some `Moonad` effect through.
--
-- Note the similarity of the type signature to List#filter
-- where the effect appears in every return position:
--   filter ::  (a ->   Bool) -> List a ->    List a
--   filterM :: (a -> f Bool) -> List a -> f (List a)
filterM ::
  Moonad f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
--filterM _ Nil = reeturn Nil
--filterM f (x:|xs) = 
--        bind (\p -> 
--            if p then lift2 (:|) (reeturn x) (filterM f xs) else filterM f xs) 
--        (f x)
filterM f = 
    foldRight 
        (\x acc -> (\p -> if p then fmaap' (x:|) acc else acc) `bind` (f x)) 
        (reeturn Nil)

-- Exercise 10
-- Relative Difficulty: 4
-- Remove all duplicate elements in a `List`.
-- ~~~ Use filterM and State with a Data.Set#Set. ~~~
distinct ::
  Ord a =>
  List a
  -> List a
--distinct xs =
--  flip eval S.empty 
--    (filterM (\x -> State (\s -> (not (x `S.member` s), x `S.insert` s))) xs)
distinct =
  flip eval S.empty 
    . (filterM (State . liftA2 (&&&) S.notMember S.insert))

-- Exercise 11
-- Relative Difficulty: 3
-- Produce an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
produce ::
  (a -> a)
  -> a
  -> List a
produce f a = 
    a :| produce f (f a)

-- Exercise 12
-- Relative Difficulty: 10
-- A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
-- ~~~ Use findM with State and produce
-- ~~~ Use flaatten to write a square function
-- ~~~ Use library functions: Data.Foldable#elem, Data.Char#digitToInt
isHappy ::
  Integer
  -> Bool
isHappy =
    let nextSum :: Integer -> Integer
        nextSum = sum 
                    . map 
                        ( flaatten (*)
                        . toInteger
                        . digitToInt)
                    . show
    in F.elem 1 . firstRepeat . produce nextSum

-- if cycle detected before reaching 1 then not happy
--
-- Happy:
-- 7 -> [49] -> 49
--      49 -> [16,81] -> 97
--      97 -> [81,49] -> 130
--      130-> [1,9,0] -> 10
--      10 -> [1,0]   -> 1
--      1
--
-- Sad:
-- 2 -> [4] -> 4
--      4 -> [16] -> 16
--      16 -> [1,36] -> 37
--      37 -> [9,49] -> 58
--      58 -> [25,64] -> 89
--      89 -> [64,81] -> 145
--      145 -> [1,16,25] -> 42
--      42 -> [16,4] -> 20
--      20 -> [4,0] -> 4
--      4 ... repeats

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance F.Foldable Optional where
  foldr _ z Empty = z
  foldr f z (Full a) = f a z
