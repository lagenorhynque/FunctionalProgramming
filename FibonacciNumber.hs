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

-- 行列の利用
fibonacci6 :: Int -> Integer
fibonacci6 i = iterate (prod [1, 1, 1, 0]) [1, 0, 0, 1] !! i !! 1
  where
    prod [a11, a12, a21, a22] [b11, b12, b21, b22] =
      [a11 * b11 + a12 * b21, a11 * b12 + a12 * b22,
       a21 * b11 + a22 * b21, a21 * b12 + a22 * b22]

fibonacci6' :: Int -> Integer
fibonacci6' i = pow prod [1, 1, 1, 0] i [1, 0, 0, 1] !! 1
  where
    pow f x n a
      | n == 0    = a
      | even n    = pow f (f x x) (n `div` 2) a
      | otherwise = pow f x (n - 1) (f x a)
    prod [a11, a12, a21, a22] [b11, b12, b21, b22] =
      [a11 * b11 + a12 * b21, a11 * b12 + a12 * b22,
       a21 * b11 + a22 * b21, a21 * b12 + a22 * b22]
