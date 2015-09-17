{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  f <$> Compose c =
    Compose $ ((<$>) . (<$>)) f c

instance (Apply f, Apply g) =>
  Apply (Compose f g) where
  Compose f <*> Compose a =
    Compose $ lift2 (<*>) f a

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure =
    Compose . pure . pure

instance (Bind f, Bind g) =>
  Bind (Compose f g) where
  (=<<) =
    undefined
