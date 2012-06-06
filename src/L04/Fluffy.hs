module L04.Fluffy where

import L01.Optional
import L01.Validation
import L02.List
import L03.Parser

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy List where
  furry = maap

instance Fluffy ((->) t) where
  -- (a -> b) -> (t -> a) -> (t -> b)
  furry = (.)

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Optional where
  furry _ Empty = Empty
  furry f (Full x) = Full (f x)


foldOpt :: (a -> x) -> x -> Optional a -> x
foldOpt _ z Empty = z
foldOpt f _ (Full x) = f x

-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where
  furry f p = p `bindParser` (valueParser . f)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
