{-
 - https://www.hackerrank.com/challenges/prefix-compression/problem

You are in charge of data transfer between two Data Centers. Each set of data is represented by a pair of strings.
Over a period of time you have observed a trend: most of the times both strings share some prefix.
You want to utilize this observation to design a data compression algorithm which will be used to reduce amount of data to be transferred.

You are given two strings, x and y, representing the data, you need to find the longest common prefix (p) of the two strings.
Then you will send substring p, x' and y', where x' and y' are the substring left after stripping p from them.

For example, if
"abcdefpr" and "abcpqr", then "abc", "defpr", "pqr".

Input Format
The first line contains a single string denoting x.
The second line contains a single string denoting y.

Constraints
x and y will contain only lowercase Latin characters ('a'-'z').
1 <= length(x), length(y) <= 10^5

Output Format
In first line, print the length of substring p, followed by prefix p.
In second line, print the length of substring x', followed by substring x'.
Similary in third line, print the length of substring y', followed by substring y'.

Sample Input 0
abcdefpr
abcpqr

Sample Output 0
3 abc
5 defpr
3 pqr

Sample Input 1
kitkat
kit

Sample Output 1
3 kit
3 kat
0

Sample Input 2
puppy
puppy

Sample Output 2
5 puppy
0
0
 -}


import Control.Exception (assert)

-- return the longest common prefix length of two strings, x and y
common_prefix_len :: [Char] -> [Char] -> Int
common_prefix_len x y = length $ takeWhile (\(a,b) -> a==b) zippedx_y
	where zippedx_y = zip x y


-- print p, x' and y'
print_prefix_compressed_lines x y = do
	printLenStr p
	printLenStr x'
	printLenStr y'
	where
	cpl = common_prefix_len x y
	p = take cpl x
	x' = drop cpl x
	y' = drop cpl y


-- print length of a string followed by the string in a line
-- "hello" -> "5 hello"
printLenStr str = do
	putStrLn $ (show $ length str) ++ " " ++ str

main = do
	x <- getLine
	y <- getLine
	print_prefix_compressed_lines x y
	
