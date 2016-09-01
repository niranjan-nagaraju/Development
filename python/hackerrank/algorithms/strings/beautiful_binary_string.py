'''
https://www.hackerrank.com/challenges/beautiful-binary-string

Alice has a binary string, B, of length n. 
She thinks a binary string is beautiful if and only if it doesn't contain the substring 010.

In one step, Alice can change a 0 to a 1 (or vice-versa). 
Count and print the minimum number of steps needed to make Alice see the string as beautiful.

Input Format
The first line contains an integer, n (the length of binary string B).
The second line contains a single binary string, B, of length n.

Output Format
	Print the minimum number of steps needed to make the string beautiful.

Sample Input 0
	7
	0101010

Sample Output 0
	2

Sample Input 1
	5
	01100
	
Sample Output 1
	0

Sample Input 2
	10
	0100101010

Sample Output 2
	3

'''

n = int(raw_input())
B = raw_input().strip()

steps = 0
i = 0

while i<n-2:
	# Changing '010 -> 011' ensures we don't create a new 
	# '010' substring anywhere in B
	# Simply count the number of '010's to get the steps needed
	# to make the string 'beautiful'
	if (B[i], B[i+1], B[i+2]) == ('0', '1', '0'):
		steps += 1
		i += 2
	
	i += 1

print steps

	


