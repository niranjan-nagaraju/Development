/*
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
*/


/*
Solution Outline:
	1. Use a stack to reduce expressions as operators are encountered.
	2. The stack stores only operands, and 
		2.1 on encountering an operator,
		2.2 pop right followed by the left operand
		2.3 Apply operator on the operands, and push the reduced result onto the stack.
	3. For each operand, push it onto the stack.
*/

struct Solution {}

impl Solution {
    pub fn evaluate(expression: Vec<&str>) -> i32 {
		let mut stack: Vec<i32> = vec![];

		for x in expression {
			if vec!["+", "-", "*", "/"].contains(&x) {
				let right:i32 = stack.pop().unwrap();
				let left:i32 = stack.pop().unwrap();	

				// apply 'operator' on the right and left operands
				match x {
					"+" => stack.push(left+right),
					"-" => stack.push(left-right),
					"*" => stack.push(left*right),
					"/" => stack.push(left/right),
					_ => {}
				}
			} else {
				let xi:i32 = x.parse::<i32>().unwrap();
				stack.push(xi);
			}
		}

		// Return top of the stack as the reduced expression value
		// If the epxression is valid,
		// there are no more than one  operands on the stack
		assert!(stack.len() == 1);

		return stack.pop().unwrap();
	}
}


fn main() {
	assert!(Solution::evaluate(vec!["2", "1", "+", "3", "*"]) == 9);
	assert!(Solution::evaluate(vec!["4", "13", "5", "/", "+"]) == 6);
	assert!(Solution::evaluate(vec!["123"]) == 123);
}

