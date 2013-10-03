{-# LANGUAGE NoImplicitPrelude #-}

module Monad.Functor where

import Core(IO, Maybe(..), (.))
import qualified Prelude as P(fmap)
import Intro.Id(Id(..))
import Intro.Optional(Optional(..))
import Structure.List(List(..), map)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- $setup
-- >>> import Core(Num(..), putStr, reverse, (>>))
-- >>> import qualified Prelude as P(return)

-- Exercise 1
-- Relative Difficulty: 1
--
-- | Maps a function on the Id functor.
--
-- >>> fmap (+1) (Id 2)
-- Id 3
instance Functor Id where
  fmap f (Id x) = Id (f x)

-- Exercise 2
-- Relative Difficulty: 2
--
-- | Maps a function on the List functor.
--
-- >>> fmap (+1) Nil
-- []
--
-- >>> fmap (+1) (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  fmap = map

-- Exercise 3
-- Relative Difficulty: 2
--
-- | Maps a function on the Optional functor.
--
-- >>> fmap (+1) Empty
-- Empty
--
-- >>> fmap (+1) (Full 2)
-- Full 3
instance Functor Optional where
  fmap f (Full x) = Full (f x)
  fmap _ Empty = Empty

-- Exercise 4
-- Relative Difficulty: 3
--
-- | Maps a function on the reader ((->) t) functor.
--
-- >>> fmap (+1) (*2) 8
-- 17
instance Functor ((->) t) where
  fmap =
    (.)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> fmap reverse (putStr "hi" >> P.return "abc")
-- hi"cba"
instance Functor IO where
  fmap =
    P.fmap

instance Functor [] where
  fmap =
    P.fmap

instance Functor Maybe where
  fmap =
    P.fmap
