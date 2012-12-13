{- Write a recursive function fib that takes a positive integer n 
 - as a parameter and calculates Fn.
 -} 

fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = fib (n-2) + fib (n-1)
