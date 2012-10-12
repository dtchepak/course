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
  furry f (Full a) = Full (f a)
  furry _ Empty = Empty

instance Fluffy Validation where
    furry _ (Error e) = Error e
    furry f (Value v) = Value (f v)

instance Fluffy ((,) a) where
    furry fn (f,s) = (f,fn s)

-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where 
    furry f p = P $ (furry . furry) f . parse p
--  furry f p = P $ \input -> case parse p input of
--                    Error e       -> Error e
--                    Value (i',a) -> Value (i', f a)
--  furry f p = P $ \i -> (furry . furry) f (parse p i)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
