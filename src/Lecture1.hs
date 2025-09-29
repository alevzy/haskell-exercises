{- |
Module                  : Lecture1
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete exercises, you need to complete implementation and add
missing top-level type signatures. You can implement any additional
helper functions. But you can't change the names of the given
functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay if you feel that your implementations are not
perfect. You can return to these exercises after future lectures and
improve your solutions if you see any possible improvements.
-}

module Lecture1
    ( makeSnippet
    , sumOfSquares
    , lastDigit
    , minmax
    , subString
    , strSum
    , lowerAndGreater
    ) where

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Specify the type signature of the following function. Think about
its behaviour, possible types for the function arguments and write the
type signature explicitly.
-}
makeSnippet :: Int -> String -> String
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

{- | Implement a function that takes two numbers and finds sum of
their squares.

>>> sumOfSquares 3 4
25

>>> sumOfSquares (-2) 7
53

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
sumOfSquares :: Num a => a -> a -> a
sumOfSquares x y = (sqr x) + (sqr y)
    where
        sqr :: Num a => a -> a
        sqr n = n * n

{- | Implement a function that returns the last digit of a given number.

>>> lastDigit 42
2
>>> lastDigit (-17)
7

🕯 HINT: use the @mod@ function

-}
lastDigit :: Int -> Int
lastDigit n = mod (abs n) 10

{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

>>> minmax 7 1 4
6

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.
-}
minmax :: Int -> Int -> Int -> Int
minmax x y z = (max3 x y z) - (min3 x y z)
    where
        max3 :: Int -> Int -> Int -> Int
        max3 a b c = max a (max b c)
        min3 :: Int -> Int -> Int -> Int
        min3 a b c = min a (min b c)

{- | Implement a function that takes a string, start and end positions
and returns a substring of a given string from the start position to
the end (including).

>>> subString 3 7 "Hello, world!"
"lo, w"

>>> subString 10 5 "Some very long String"
""

This function can accept negative start and end position. Negative
start position can be considered as zero (e.g. substring from the
first character) and negative end position should result in an empty
string.
-}
subString :: Int -> Int -> String -> String
subString start end str
    | start >= end = ""
    | start < 0 = subString 0 end str
    | otherwise = drop start (take (end + 1) str)

{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.

>>> strSum "100    -42  15"
73

The string contains only spaces and/or numbers.
-}
strSum :: String -> Int
strSum str = go "" str acc
    where
        go :: String -> String -> Int -> Int
        go substr "" acc = acc
        go substr (c:str) acc
            | (c == ' ') = go "" str (acc + (convert substr))
            | otherwise = go (substr:c) str acc
        convert :: String -> Int
        convert s
            | (s == "") = 0
            | otherwise = read s :: Int

{- | Write a function that takes a number and a list of numbers and
returns a string, saying how many elements of the list are strictly
greater than the given number and strictly lower.

>>> lowerAndGreater 3 [1 .. 9]
"3 is greater than 2 elements and lower than 6 elements"

Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
The given number 3 is greater than 2 elements (1 and 2)
and lower than 6 elements (4, 5, 6, 7, 8 and 9).

🕯 HINT: Use recursion to implement this function.
-}
lowerAndGreater :: Int -> [Int] -> String
lowerAndGreater n list = (show n) ++ (greaterStr n list) ++ (lowerStr n list)
    where
        greaterStr :: Int -> [Int] -> String
        greaterStr n list = " is greater than " ++ (show (greater n list 0)) ++ " elements "
        lowerStr :: Int -> [Int] -> String
        lowerStr n list = " and lower than " ++ (show (lower n list 0)) ++ " elements"
        greater :: Int -> [Int] -> Int -> Int
        greater n "" acc = acc
        greater n (a:list) acc
            | n > a = greater n list (acc + 1)
            | otherwise = greater n list acc
        lower :: Int -> [Int] -> Int -> Int
        lower n "" acc = acc
        lower n (a:list) acc
            | n < a = lower n list (acc + 1)
            | otherwise = lower n list acc

