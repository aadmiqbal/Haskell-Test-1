-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1 (checkParity, substitution, largestPrimeBetween, strongPrimes, executeCommands, atmChange) where

import Types
import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
--which takes as input a string of bits and checks that 

--1. the string size is a multiple of 8, and
--1. each byte in the string has even parity.

checkParity :: String -> Bool
checkParity xs | (((length xs) `mod` 8) == 0) && (even (length(filter (=='1') xs))) = True
                | otherwise = False

{- Question 2 -}

substitution :: String -> String -> String
substitution [] _ = []
substitution (plaintext:xs) key | isLetter (plaintext) && isLower (plaintext) = toLower ((!!) key (charLabel plaintext)) : substitution xs key
                                | isLetter (plaintext) && isUpper (plaintext) = ((!!) key (charLabel plaintext)) : substitution xs key
                                | otherwise = plaintext : substitution xs key

{- Question 3 -}

largestPrimeBetween :: Int -> Int
largestPrimeBetween n = maximum [y| y<-[(n+1)..((2*n)-1)], isPrime y ]

strongPrimes :: Int -> [Int]
strongPrimes n = undefined

{- Question 4 -}

executeCommands :: [Command] -> (Int, Int) -> (Int, Int)
executeCommands [] (a,b) = (a,b)
executeCommands ((d, x):xs) (a,b) | d== MoveLeft = executeCommands xs ((a-x),b) 
                                    | d == MoveRight = executeCommands xs ((a+x),b)
                                    | d == MoveUp = executeCommands xs (a,(b+x))
                                    | otherwise = executeCommands xs (a,(b-x))

{- Question 5 -}

atmChange :: Int -> [Int] -> [(Int, Int)]
atmChange y xs = atmChange2 y (reverse xs) 
atmChange2 :: Int -> [Int] -> [(Int, Int)]
atmChange2 _ [] = []
atmChange2 y (x:xs)| ((y `div` x) == 0) = atmChange2 y xs
                   | otherwise = (x,(y `div` x)) : (atmChange2 (y-((y`div`x)*x)) xs)

