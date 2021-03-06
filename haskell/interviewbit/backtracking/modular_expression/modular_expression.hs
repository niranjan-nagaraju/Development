{-
https://www.interviewbit.com/problems/modular-expression/

Modular Expression

Implement pow(A, B) % C.

In other words, given A, B and C,
find (A^B)%C.

Input : A = 2, B = 3, C = 3
Return : 2 
2^3 % 3 = 8 % 3 = 2
-}

{-
Solution Outline:
	(AxB) % C == (A%C * B%C) % C
	=> 
	  A^B % C ==
	   [{A^(B/2) % C} * {A^(B/2) % C}] % C  if B is even
	   [{A^(B/2) % C} * {A^(B/2) % C} * A%C }] % C  if B is odd
	   1 if B == 0


Sample run:
	2^5 % 13
	== {2^2 % 13 * 2^2 % 13 * 2%13} % 13
	== { {2%13 * 2%13}%13 * {2%13 * 2%13}%13 * 2%13 } % 13
	== { 4 * 4 * 2} % 13
	== { { 16 % 13} * {2} } % 13
	== { 3 * 2} % 13
	== 6
-}


import Control.Exception (assert)
import Data.Bits

modulo :: Int -> Int -> Int -> Int
modulo 0 _ _ = 0
modulo _ 0 _ = 1
modulo a b c
  | even b = (x * x) `mod` c
  | otherwise = ( ((x * x) `mod` c) * a)  `mod` c  -- b is odd
  where x = modulo a (shiftR b 1) c



main = do
    putStrLn $ (assert $ modulo 2 3 3 == 2) ""
    putStrLn $ (assert $ modulo 2 3 5 == 3) ""
    putStrLn $ (assert $ modulo 2 5 13 == 6) ""
    putStrLn $ (assert $ modulo 0 5 13 == 0) ""
    putStrLn $ (assert $ modulo (-1) 1 20 == 19) ""
    putStrLn $ "Testcases passed!"

