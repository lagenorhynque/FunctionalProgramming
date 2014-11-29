module MaybeInHaskell where

import Control.Applicative ((<$>))

gcd' :: Int -> Int -> Maybe Int
gcd' a b
  | a < 1 || b < 1 = Nothing
  | otherwise      = Just $ divide a b
  where
    divide x y
      | y == 0    = x
      | otherwise = divide y (x `mod` y)

main :: IO ()
main =
  do
    -- Maybe自体を表示
    print $ gcd' 12 18
    print $ gcd' 12 (-18)
    -- Maybeの中身の値を表示
    putStrLn $ maybe "" show $ gcd' 12 18
    putStrLn $ maybe "" show $ gcd' 12 (-18)
    -- Maybeの中身の値を2乗して表示
    print $ fmap (^ 2) $ gcd' 12 18
    print $ fmap (^ 2) $ gcd' 12 (-18)
    -- 関数fmapの代わりに演算子<$>を利用することもできる(アプリカティブスタイル)
    print $ (^ 2) <$> gcd' 12 18
    print $ (^ 2) <$> gcd' 12 (-18)
