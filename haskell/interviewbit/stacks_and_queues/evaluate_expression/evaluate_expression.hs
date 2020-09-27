import Control.Exception (assert)


{-
 - Helper function to reduce a reverse polish notation
 - expression using a stack
 -}
evaluate' :: [[Char]] -> [Int] -> [Int]
evaluate' [] stack = stack
evaluate' (x:expression) stack
  | x == "+" = evaluate' expression ((a+b):rest)
  | x == "-" = evaluate' expression ((a-b):rest)
  | x == "*" = evaluate' expression ((a*b):rest)
  | x == "/" = evaluate' expression ((a `div` b):rest)
  | otherwise = evaluate' expression (operand:stack) -- x is an operand
  where operand = read x :: Int
        [b,a] = take 2 stack
        rest = drop 2 stack



{-
 - Evaluate a postfix expression
 -}
evaluate :: [[Char]] -> Int
evaluate expression = head $ evaluate' expression []



main = do
    putStrLn $ (assert $ (evaluate ["2"]) == 2) ""
    putStrLn $ (assert $ (evaluate ["2", "1", "+", "3", "*"]) == 9) ""
    putStrLn $ (assert $ (evaluate ["4", "13", "5", "/", "+"]) == 6) ""
    putStrLn $ "Testcases passed!"

