module L09.Compose where

import Control.Applicative

-- Exactly one of these exercises will not be possible to achieve.

newtype Compose f g a =
  Compose (f (g a))

-- Exercise 1
-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  -- :: (a -> b) -> Compose f g a -> Compose f g b
  -- c :: f (g a)
  --fmap f (Compose c) = Compose ((fmap . fmap) f c)
  fmap f (Compose c) = Compose $ (fmap . fmap) f c

instance (Applicative f, Applicative g) =>
    Applicative (Compose f g) where
-- Exercise 2
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
-- Exercise 3
-- Implement the (<*>) function for an Applicative instance for Compose
  _ <*> _ =
    error "todo"

instance (Monad f, Monad g) =>
    Monad (Compose f g) where
-- Exercise 4
-- Implement the return function for a Monad instance for Compose
  return = 
    error "todo"
-- Exercise 5
-- Implement the (>>=) function for a Monad instance for Compose
  _ >>= _ =
    error "todo"

