{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List
-- My imports:
import Course.Optional (Optional(..), (??))
import Course.Traversable

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

* To run from ./share:

share/ $ runhaskell -i../src ../src/Course/FileIO.hs files.txt
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = getArgs >>= main'

single :: List a -> Optional a
single (x:.Nil) = Full x
single _ = Empty

-- For testing arg handling from ghci
main' ::
  List Chars
  -> IO ()
main' args =
  single (run <$> args) ?? putStrLn "Usage: runhaskell FileIO.hs (filename)"


type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run file =
  let files = lines <$> readFile file
  in files >>= (printFiles <=< getFiles)

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  traverse getFile -- sequence . map getFile

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile path =
  (path,) <$> readFile path

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
  void . traverse (uncurry printFile)

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile path contents =
  putStrLn ("============ " ++ path)
  >> putStrLn contents

