module L03.Parser where

import Data.Char
import L01.Validation
import L03.Person


type Input = String

data Parser a = P {
  parse :: Input -> Validation (Input, a)
}

-- Exercise 1
-- Return a parser that always succeeds
-- with the given value and consumes no input.
valueParser :: a -> Parser a
valueParser = \a -> P (\i -> Value (i, a))

-- Exercise 2
-- Return a parser that always fails
-- with the given error.
failed :: Err -> Parser a
failed err = P (\_ -> Error err)

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
bindParser (P p) f = P (\i -> case p i of (Value (i', a)) -> parse (f a) i'
                                          (Error err)       -> Error err)

-- Exercise 5
-- Return a parser that puts its input into the given parser and
--   * if that parser succeeds with a value (a), ignore that value
--     but put the remaining input into the second given parser.
--   * if that parser fails with an error the returned parser fails with that error.
(>>>) :: Parser a -> Parser b -> Parser b
p >>> q = p `bindParser` (\_ -> q)

-- Exercise 6
-- Return a parser that tries the first parser for a successful value.
--   * If the first parser succeeds then use this parser.
--   * If the first parser fails, try the second parser.
(|||) :: Parser a -> Parser a -> Parser a
(P p) ||| (P q) = P (\i -> case p i of v@(Value _) -> v
                                       (Error _)   -> q i)

infixl 3 |||

-- Exercise 7
-- Return a parser that continues producing a list of values from the given parser.
list :: Parser a -> Parser [a]
list p = many1 p ||| valueParser []

-- Exercise 8
-- Return a parser that produces at least one value from the given parser then
-- continues producing a list of values from the given parser (to ultimately produce a non-empty list).
-- The returned parser fails if
--   * The input is empty
many1 :: Parser a -> Parser [a]
many1 p = p `bindParser` (\k ->
          list p `bindParser` (\k' ->
          valueParser (k:k') ))

-- Exercise 9
-- Return a parser that produces a character but fails if
--   * The input is empty.
--   * The character does not satisfy the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = character `bindParser` (\c -> 
                if p c then valueParser c
                else failed $ "unexpected character " ++ [c])

-- Exercise 10.1
-- Return a parser that produces the given character but fails if
--   * The input is empty.
--   * The produced character is not equal to the given character.
is :: Char -> Parser Char
is = satisfy . (==)

-- Exercise 10.2
-- Return a parser that produces a character between '0' and '9' but fails if
--   * The input is empty.
--   * The produced character is not a digit.
digit :: Parser Char
digit = satisfy isDigit

-- Exercise 10.3
-- Return a parser that produces zero or a positive integer but fails if
--   * The input is empty.
--   * The input does not produce a value series of digits
natural :: Parser Int
natural = many1 digit `bindParser` (\ds -> valueParser (read ds))

natural' :: Parser Int
natural' = bindParser (list digit) (\k -> case reads k of []    -> failed "Failed to parse natural"
                                                          ((h,_):_) -> valueParser h)

-- Exercise 10.4
-- Return a parser that produces a space character but fails if
--   * The input is empty.
--   * The produced character is not a space.
space :: Parser Char
space = is ' '

-- Exercise 10.5
-- Return a parser that produces one or more space characters
-- (consuming until the first non-space) but fails if
--   * The input is empty.
--   * The first produced character is not a space.
spaces1 :: Parser String
spaces1 = many1 space

-- Exercise 10.6
-- Return a parser that produces a lower-case character but fails if
--   * The input is empty.
--   * The produced character is not lower-case.
lower :: Parser Char
lower = satisfy isLower

-- Exercise 10.7
-- Return a parser that produces an upper-case character but fails if
--   * The input is empty.
--   * The produced character is not upper-case.
upper :: Parser Char
upper = satisfy isUpper

-- Exercise 10.8
-- Return a parser that produces an alpha character but fails if
--   * The input is empty.
--   * The produced character is not alpha.
alpha :: Parser Char
alpha = satisfy isAlpha

-- Exercise 11
-- Return a parser that sequences the given list of parsers by producing all their results
-- but fails on the first failing parser of the list.
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser [] = valueParser []
sequenceParser (p:ps) = p `bindParser` (\k ->
                            sequenceParser ps `bindParser` (\k' ->
                            valueParser (k:k') ))

-- Exercise 12
-- Return a parser that produces the given number of values off the given parser.
-- This parser fails if
--   * The given parser fails in the attempt to produce the given number of values.
thisMany :: Int -> Parser a -> Parser [a]
thisMany n = sequenceParser . (replicate n)

-- Exercise 13
-- Write a parser for Person.age.
-- * Age: positive integer
ageParser :: Parser Int
ageParser = natural

-- Exercise 14
-- Write a parser for Person.firstName.
-- * First Name: non-empty string that starts with a capital letter
firstNameParser :: Parser String
firstNameParser = upper `bindParser` (\k ->
                  list lower `bindParser` (\k' ->
                  valueParser (k:k') ))

-- Exercise 15
-- Write a parser for Person.surname.
-- * Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters
surnameParser :: Parser String
surnameParser = upper `bindParser` (\k ->
                  thisMany 5 lower `bindParser` (\k' ->
                  list lower `bindParser` (\k'' ->
                  valueParser (k:(k' ++ k'')) )))

-- Exercise 16
-- Write a parser for Person.gender.
-- * Gender: character that must be 'm' or 'f'
genderParser :: Parser Char
genderParser = is 'm' ||| is 'f'

-- Exercise 17
-- Write part of a parser for Person.phoneBody.
-- This parser will only produce a string of digits, dots or hyphens.
-- It will ignore the overall requirement of a phone number to
-- start with a digit and end with a hash (#).
-- * Phone: string of digits, dots or hyphens ...
phoneBodyParser :: Parser String
phoneBodyParser = list (digit ||| is '.' ||| is '-')

-- Exercise 18
-- Write a parser for Person.phone.
-- * Phone: ... but must start with a digit and end with a hash (#)
phoneParser :: Parser String
phoneParser = digit `bindParser` (\d ->
                phoneBodyParser `bindParser` (\d' ->
                is '#' >>> valueParser (d:d') ))

-- Exercise 19
-- Write a parser for Person.
personParser :: Parser Person
personParser = ageParser `bindParser` (\age ->
               space >>>
               firstNameParser `bindParser` (\fn ->
               space >>>
               surnameParser `bindParser` (\sn ->
               space >>>
               genderParser `bindParser` (\gender ->
               space >>>
               phoneParser `bindParser` (\ph ->
               valueParser (Person age fn sn gender ph) )))))

-- Exercise 20
-- Make sure all the tests pass!

