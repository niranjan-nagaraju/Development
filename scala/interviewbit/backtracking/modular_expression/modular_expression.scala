/*
https://www.interviewbit.com/problems/modular-expression/

Modular Expression

Implement pow(A, B) % C.

In other words, given A, B and C,
find (A^B)%C.

Input : A = 2, B = 3, C = 3
Return : 2 
2^3 % 3 = 8 % 3 = 2
*/

/**
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
*/



class Solution {
  def mod(a: Int, b: Int, c: Int): Int  = {
	if (a == 0)
	  return 0

	if (b == 0)
	  return 1

	var x:Long = mod(a, b/2, c)
	x = (x * x) % c;

	if ((b & 1) == 1) {
	  // Add +C incase A is -ve
	  return ((x * (a%c) + c) % c).toInt
	}

	return x.toInt
  }
}


object Solution {
  def main(args: Array[String]): Unit = {
	val s = new Solution

	assert ( s.mod(2, 3, 3) == 2 )
	assert ( s.mod(2, 3, 5) == 3 )
	assert ( s.mod(2, 5, 13) == 6 )
	assert ( s.mod(0, 5, 13) == 0 )
	assert ( s.mod(-1, 1, 20) == 19 )

  }
}
