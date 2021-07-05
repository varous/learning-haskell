
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello World"

add a b = a + b

fac n 
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = n * fac (n-1)

facTailRecursion n = aux n 1
    where
        aux n acc
            | n == 1 = acc
            | otherwise = aux (n - 1) (n * acc)


fibonacci n = go n (0,1)
    where
        go n (a,b)
            | n == 0 = a
            | otherwise = go (n - 1) (b, a + b)

ascendList :: Int -> Int -> [Int]
ascendList n m
    | m < n = []
    | m == n = [m]
    | m > n = n : ascendList (n + 1) m

