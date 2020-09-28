{-
https://www.interviewbit.com/problems/evaluate-expression/

Evaluate Expression


Evaluate the value of an arithmetic expression in Reverse Polish Notation.

Valid operators are +, -, *, /. Each operand may be an integer or another expression.


Input Format
The only argument given is character array A.

Output Format
Return the value of arithmetic expression formed using reverse Polish Notation.

For Example
Input 1:
    A =   ["2", "1", "+", "3", "*"]
Output 1:
    9
Explaination 1:
    starting from backside:
    *: ( )*( )
    3: ()*(3)
    +: ( () + () )*(3)
    1: ( () + (1) )*(3)
    2: ( (2) + (1) )*(3)
    ((2)+(1))*(3) = 9
    
Input 2:
    A = ["4", "13", "5", "/", "+"]
Output 2:
    6
Explaination 2:
    +: ()+()
    /: ()+(() / ())
    5: ()+(() / (5))
    1: ()+((13) / (5))
    4: (4)+((13) / (5))
    (4)+((13) / (5)) = 6
-}


{-
Solution Outline:
	1. Use a stack to reduce expressions as operators are encountered.
	2. The stack stores only operands, and 
		2.1 on encountering an operator,
		2.2 pop right followed by the left operand
		2.3 Apply operator on the operands, and push the reduced result onto the stack.
	3. For each operand, push it onto the stack.
-}



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

