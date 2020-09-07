/**
https://www.interviewbit.com/problems/fraction/

Fraction

Given two integers representing the numerator and denominator of a fraction, return the fraction in string format.

If the fractional part is repeating, enclose the repeating part in parentheses.

Example :

Given numerator = 1, denominator = 2, return "0.5"
Given numerator = 2, denominator = 1, return "2"
Given numerator = 2, denominator = 3, return "0.(6)"
*/


/**
Solution Outline:
    Consider 50/22

        ------
    22)   50  (2.272
          44
        ------
           6 * 10
          60
          44
        ------
          16 * 10
          160
          154
        ------
            6 * 10
          60
          44
        ------
          16

    50/22 == 2.(27)
    when remainder repeats, the quotient repeats from the last time the same remainder was seen
    Use a hash-table to store the remainders to lookup repeats.
*/

#include <string>
#include <unordered_map>
#include <cassert>
#include <vector>
#include <cstdlib>

class Solution {
public:
	std::string get_decimal(int n, int d) {
		int64_t numerator = n;
		int64_t denominator = d;

		if (denominator == 0)
			throw "Divide by zero!";

		if (numerator % denominator == 0)
			return std::to_string(numerator / denominator);

		char sign[2] = "";
		if ( (numerator < 0 && denominator > 0) || (numerator > 0 && denominator < 0) )
			sign[0] = '-';

		numerator = std::abs(numerator);
		denominator = std::abs(denominator);

		std::vector<int> quotients = {(int)(numerator / denominator)};
		int remainder = numerator % denominator;
		std::unordered_map<int, int> remainder_tbl;

		auto toStr = [&quotients](int startIdx, int endIdx) {
			std::string res;
			for (int i=startIdx; i<endIdx; i++) {
				res += quotients[i] + '0';
			}

			return res;
		};

		while (remainder != 0) {
			auto it = remainder_tbl.find(remainder);
			if (it != remainder_tbl.end()) {
				// remainder was previously seen when quotient was quotients[idx]
				int idx = it->second;
				return sign + std::to_string(quotients[0]) + "." +
					toStr(1, idx) + "(" + toStr(idx, quotients.size()) + ")";
			}

			// Store the start of the quotient when we last saw this remainder
			remainder_tbl[remainder] = quotients.size();
			remainder *= 10;

			numerator = remainder / denominator;
			quotients.push_back(numerator);
			remainder = remainder % denominator;
		}

		// At this point, the remainder doesn't repeat
		// and so there are no repeating sequences in the quotients
		// Just join the integer quotient and the decimal parts
		return sign + std::to_string(quotients[0]) + "." + toStr(1, quotients.size());
	}
};


int
main(void)
{
	Solution s;
	assert( s.get_decimal(50, 8) == "6.25" );
	assert( s.get_decimal(50, 16) == "3.125");
	assert( s.get_decimal(50, 22) == "2.(27)");
	assert( s.get_decimal(50, 5) == "10");
	assert( s.get_decimal(11, 6) == "1.8(3)");
	assert( s.get_decimal(1, 2) == "0.5");
	assert( s.get_decimal(2, 1) == "2");
	assert( s.get_decimal(2, 3) == "0.(6)");
	assert( s.get_decimal(22, 7) == "3.(142857)"); // (22/7) is not pi
	assert( s.get_decimal(-1, 2) == "-0.5");
	assert( s.get_decimal(-50, 22) == "-2.(27)" );

	return 0;
}
