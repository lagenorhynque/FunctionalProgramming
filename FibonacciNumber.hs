module FibonacciNumber where

import Data.List (foldl')

-- 再帰
fibonacci1 :: Int -> Integer
fibonacci1 0 = 0
fibonacci1 1 = 1
fibonacci1 i = fibonacci1 (i - 2) + fibonacci1 (i - 1)

-- 末尾再帰
fibonacci2 :: Int -> Integer
fibonacci2 i = fib i 0 1
  where
    fib 0 a _ = a
    fib n a b = fib (n - 1) b (a + b)

-- 高階関数
fibonacci3 :: Int -> Integer
fibonacci3 i = fst $ foldl' fib (0, 1) [1..i]
  where
    fib (a, b) _ = (b, a + b)

-- 遅延評価
fibonacci4 :: Int -> Integer
fibonacci4 i = fibs !! i
  where
    fibs = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)

fibonacci5 :: Int -> Integer
fibonacci5 i = fibs !! i
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
