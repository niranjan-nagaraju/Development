/*
https://www.interviewbit.com/problems/modular-expression/

Modular Expression

Implement pow(a, b) % c.

In other words, given a, b and c,
find (a^b)%c.

Input : a = 2, b = 3, c = 3
Return : 2 
2^3 % 3 = 8 % 3 = 2
*/

/*
Solution Outline:
	(axb) % c == (a%c * b%c) % c
	=> 
	  a^b % c ==
	   [{a^(b/2) % c} * {a^(b/2) % c}] % c  if b is even
	   [{a^(b/2) % c} * {a^(b/2) % c} * a%c }] % c  if b is odd
	   1 if b == 0


Sample run:
	2^5 % 13
	== {2^2 % 13 * 2^2 % 13 * 2%13} % 13
	== { {2%13 * 2%13}%13 * {2%13 * 2%13}%13 * 2%13 } % 13
	== { 4 * 4 * 2} % 13
	== { { 16 % 13} * {2} } % 13
	== { 3 * 2} % 13
	== 6
*/

struct Solution {}

impl Solution {
    pub fn modulo(a: i32, b: i32, c:i32) -> i32 {
        fn modulo_(a: i64, b: i64, c:i64) -> i64 {
            if b == 0 {
                return 1;
            }

            let mut x: i64 = modulo_(a, b>>1, c);

            x = ((x % c) * (x % c)) % c;
            if (b & 1) == 1 {
                // add +c incase a is -ve
                return (x * a%c + c) % c;
            }

            return x;
        }

        if a == 0 {
            return 0;
        }

        return modulo_(a as i64, b as i64, c as i64) as i32;
    }
}



fn main() {
	assert!(Solution::modulo(2, 3, 3) == 2);
	assert!(Solution::modulo(2, 3, 5) == 3);
	assert!(Solution::modulo(2, 5, 13) == 6);
	assert!(Solution::modulo(0, 5, 13) == 0);
	assert!(Solution::modulo(-1, 1, 20) == 19);
}

