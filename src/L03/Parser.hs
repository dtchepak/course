module L03.Parser where

import Data.Char
import L01.Validation
import L03.Person
import Control.Arrow (second)

type Input = String

data Parser a = P {
  parse :: Input -> Validation (Input, a)
}

data ParserT f x = Pt {
  parseT :: Input -> f (Validation (Input, x))
}
 
instance Functor f => Functor (ParserT f) where
  -- (a -> b) -> ParserT f a -> ParserT f b
  --fmap f (Pt p) = Pt (\i -> fmap (mapValidation (\(i',v) -> (i', f v))) (p i))
  --fmap f (Pt p) = Pt (\i -> fmap (mapValidation (second f)) (p i))
  --fmap f (Pt p) = Pt $ ((fmap . mapValidation . second) f) . p
  --fmap f (Pt p) = Pt $ ((fmap . mapValidation . second) f) `fmap` p
  --fmap f (Pt p) = Pt $ fmap ((fmap . mapValidation . second) f) p
  fmap f (Pt p) = Pt $ ((fmap . fmap . mapValidation . second) f) p
  --If we had instances of fmaps everywhere:
  --    fmap f (Pt p) = Pt $ ((fmap . fmap . fmap . fmap) f) p

--let a  = parseT (Pt (\i -> [Value (i,1), Value (i,2)])) "abc"

instance Monad f => Monad (ParserT f) where
  (>>=) = undefined
  return = undefined


-- Exercise 1
-- Return a parser that always succeeds
-- with the given value and consumes no input.
valueParser :: a -> Parser a
valueParser x = P (\i -> Value (i, x))

-- Exercise 2
-- Return a parser that always fails
-- with the given error.
failed :: Err -> Parser a
--failed err = P (\_ -> Error err)
failed = P . const . Error

-- Exercise 3
-- Return a parser that succeeds with a character
-- off the input or fails with an error if the input is empty.
character :: Parser Char
character = P (\i -> case i of (c:cs) -> Value (cs, c)
                               []     -> Error "input is empty")

-- Exercise 4
-- Return a parser that puts its input into the given parser and
--   * if that parser succeeds with a value (a), put that value into the given function
--     then put in the remaining input in the resulting parser.
--   * if that parser fails with an error the returned parser fails with that error.
bindParser :: Parser a -> (a -> Parser b) -> Parser b
(P p) `bindParser` f = P (\i -> case p i of Value (i',x) -> parse (f x) i'
                                            Error e      -> Error e)

-- Exercise 5
-- Return a parser that puts its input into the given parser and
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--   * if that parser fails with an error the returned parser fails with that error.
-- ~~~ This function should call bindParser. ~~~
(>>>) :: Parser a -> Parser b -> Parser b
--(P p) >>> (P q) = P (\i -> case p i of Value (i',_) -> q i'
--                                       Error e      -> Error e)
p >>> q = p `bindParser` (\_ -> q)

-- Exercise 6
-- Return a parser that tries the first parser for a successful value.
--   * If the first parser succeeds then use this parser.
--   * If the first parser fails, try the second parser.
(|||) :: Parser a -> Parser a -> Parser a
(P p) ||| (P q) = P (\i -> case p i of v@(Value _) -> v
                                       Error _     -> q i)

infixl 3 |||

-- Exercise 7
-- Return a parser that continues producing a list of values from the given parser.
-- ~~~ Use many1, valueParser and (|||). ~~~
list :: Parser a -> Parser [a]
list p = many1 p ||| valueParser []

-- Exercise 8
-- Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
-- The returned parser fails if
--   * The input is empty
-- ~~~ Use bindParser, list and value. ~~~
many1 :: Parser a -> Parser [a]
many1 p = p `bindParser` (\x ->
          list p `bindParser` (\xs ->
          valueParser (x:xs) ))

-- Exercise 9
-- Return a parser that produces a character but fails if
--   * The input is empty.
--   * The character does not satisfy the given predicate.
-- ~~~ The bindParser and character functions will be helpful here. ~~~
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = character `bindParser` (\c -> if pred c 
                                             then valueParser c 
                                             else failed $ "not satisfied for " ++ [c])

-- Exercise 10.1
-- Return a parser that produces the given character but fails if
--   * The input is empty.
--   * The produced character is not equal to the given character.
-- ~~~ Use the satisfy function. ~~~
is :: Char -> Parser Char
is = satisfy . (==)

-- Exercise 10.2
-- Return a parser that produces a character between '0' and '9' but fails if
--   * The input is empty.
--   * The produced character is not a digit.
-- ~~~ Use the satisfy and Data.Char.isDigit functions. ~~~
digit :: Parser Char
digit = satisfy isDigit

-- Exercise 10.3
-- Return a parser that produces zero or a positive integer but fails if
--   * The input is empty.
--   * The input does not produce a value series of digits
-- ~~~ Use the bindParser, valueParser, list and digit functions. ~~~
natural :: Parser Int
natural = many1 digit `bindParser` (valueParser . read)

-- Exercise 10.4
-- Return a parser that produces a space character but fails if
--   * The input is empty.
--   * The produced character is not a space.
-- ~~~ Use the satisfy and Data.Char.isSpace functions. ~~~
space :: Parser Char
space = satisfy isSpace

-- Exercise 10.5
-- Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--   * The input is empty.
--   * The first produced character is not a space.
-- ~~~ Use the many1 and space functions. ~~~
spaces1 :: Parser String
spaces1 = many1 space

-- Exercise 10.6
-- Return a parser that produces a lower-case character but fails if
--   * The input is empty.
--   * The produced character is not lower-case.
-- ~~~ Use the satisfy and Data.Char.isLower functions. ~~~
lower :: Parser Char
lower = satisfy isLower

-- Exercise 10.7
-- Return a parser that produces an upper-case character but fails if
--   * The input is empty.
--   * The produced character is not upper-case.
-- ~~~ Use the satisfy and Data.Char.isUpper functions. ~~~
upper :: Parser Char
upper = satisfy isUpper

-- Exercise 10.8
-- Return a parser that produces an alpha character but fails if
--   * The input is empty.
--   * The produced character is not alpha.
-- ~~~ Use the satisfy and Data.Char.isAlpha functions. ~~~
alpha :: Parser Char
alpha = satisfy isAlpha

-- Exercise 11
-- Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
-- ~~~ Use bindParser and value. ~~~
-- ~~~ Optionally use Prelude.foldr. If not, an explicit recursive call. ~~~
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser [] = valueParser []
sequenceParser (p:ps) = p `bindParser` (\k ->
                        sequenceParser ps `bindParser` (\ks ->
                        valueParser (k:ks)))

-- Exercise 12
-- Return a parser that produces the given number of values off the given parser.
-- This parser fails if
--   * The given parser fails in the attempt to produce the given number of values.
-- ~~~ Use sequenceParser and Prelude.replicate. ~~~
thisMany :: Int -> Parser a -> Parser [a]
thisMany n = sequenceParser . replicate n

-- Exercise 13
-- Write a parser for Person.age.
-- * Age: positive integer
-- ~~~ Equivalent to natural. ~~~
ageParser :: Parser Int
ageParser = natural

-- Exercise 14
-- Write a parser for Person.firstName.
-- * First Name: non-empty string that starts with a capital letter
-- ~~~ Use bindParser, value, upper, list and lower. ~~~
firstNameParser :: Parser String
firstNameParser = upper `bindParser` (\first ->
                  list lower `bindParser` (\rest ->
                  valueParser (first:rest)))

-- Exercise 15
-- Write a parser for Person.surname.
-- * Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters
-- ~~~ Use bindParser, value, upper, thisMany, lower and list. ~~~
surnameParser :: Parser String
surnameParser = upper             `bindParser` (\first ->
                thisMany 5 lower  `bindParser` (\middle ->
                list lower        `bindParser` (\rest ->
                valueParser $ first:(middle++rest) )))

-- Exercise 16
-- Write a parser for Person.gender.
-- * Gender: character that must be 'm' or 'f'
  -- ~~~ Use is and (|||). ~~~
genderParser :: Parser Char
genderParser = is 'm' ||| is 'f'

-- Exercise 17
-- Write part of a parser for Person.phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
-- * Phone: string of digits, dots or hyphens ...
-- ~~~ Use list, digit, (|||) and is. ~~~
phoneBodyParser :: Parser String
phoneBodyParser = list (digit ||| is '.' ||| is '-')

-- Exercise 18
-- Write a parser for Person.phone.
-- * Phone: ... but must start with a digit and end with a hash (#)
-- ~~~ Use bindParser, value, digit, phoneBodyParser and is. ~~~
phoneParser :: Parser String
phoneParser = digit `bindParser` (\d ->
              phoneBodyParser `bindParser` (\p ->
              is '#' >>>
              valueParser (d:p)))

-- Exercise 19
-- Write a parser for Person.
-- ~~~ Use bindParser, value, (>>>)
--         ageParser,
--         firstNameParser,
--         surnameParser,
--         genderParser,
--         phoneParser ~~~
personParser :: Parser Person
personParser = 
    ageParser `bindParser` (\age ->
    spaces1 >>>
    firstNameParser `bindParser` (\first ->
    spaces1 >>>
    surnameParser `bindParser` (\last ->
    spaces1 >>>
    genderParser `bindParser` (\gender ->
    spaces1 >>>
    phoneParser `bindParser` (\phone ->
    valueParser (Person age first last gender phone) )))))

-- Exercise 20
-- Make sure all the tests pass!

