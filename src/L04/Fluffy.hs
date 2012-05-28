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

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Optional where
  furry = mapOptional

-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where
  furry f p = p `bindParser` (\a -> valueParser (f a))

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
