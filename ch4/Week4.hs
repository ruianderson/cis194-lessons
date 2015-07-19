{-# OPTIONS_GHC -Wall #-}

module Week4 where

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/=1) . iterate f
    where f = \n -> if odd n then 3 * n + 1 else n `div` 2

xor :: [Bool] -> Bool
xor = foldl (\x y -> not (x == y)) False
