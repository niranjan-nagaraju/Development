/**
https://adventofcode.com/2020/day/1

--- Day 1: Report Repair ---

For example, suppose your expense report contained the following:

1721
979
366
299
675
1456
In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?

--- Part Two ---
The Elves in accounting are thankful for your help; one of them even offers you a starfish coin they had left over from a past vacation. They offer you a second one if you can find three numbers in your expense report that meet the same criteria.

Using the above example again, the three entries that sum to 2020 are 979, 366, and 675. Multiplying them together produces the answer, 241861950.

In your expense report, what is the product of the three entries that sum to 2020?
*/


#include <iostream>
#include <vector>
#include <cassert>
#include <algorithm>

class Day1 {
	std::vector<int> &v;

public:	
	Day1(std::vector<int> &v_): v(v_) {
		std::sort(v.begin(), v.end());
	}

	// returns two numbers a,b in `lst[startIdx..]` s.t a+b==target
	// expects `lst` to be sorted.
	static std::pair<int,int>
	find_pairs_for_target_sum(std::vector<int> &v, int target, int startIdx=0) {
		int i = startIdx, j=v.size()-1;
		while (i < j) {
			if (v[i] + v[j] == target)
				return std::make_pair(v[i], v[j]);

			if (v[i] + v[j] < target)
				i += 1;
			else // v[i]+v[j] > target
				j -= 1;
		}
		throw "List does not contain a pair with target sum";
	}


	// Find a triplet (a,b,c) | a+b+c == target
	// For each `a` in lst
	//  Find a pair(b,c) to the right of `a` in lst | a+b+c == target, b+c == target-a
	static std::vector<int>
	find_triplets_for_target_sum(std::vector<int> &v, int target) {
		int i = 0;
		while (i < v.size()) {
			try {
				auto p = Day1::find_pairs_for_target_sum(v, target-v[i], i+1);
				return std::vector<int>{v[i], p.first, p.second};
			} catch (const char *msg) {
				// continue
				i++;
			}
		}
		throw "List does not contain a triplet with target sum";
	}

	// Calculate a*b, s.t a+b==2020
	// a,b, in lst
	// `lst` is expected to be sorted
	int day1_p1(void) {
		try {
			auto p = Day1::find_pairs_for_target_sum(this->v, 2020);
			int a = p.first, b = p.second;
			return a*b;
		} catch (const char *msg) {
			throw msg;
		}
	}


	// Calculate a*b*c, s.t a+b+c==2020
	// a,b,c in lst
	// `lst` is expected to be sorted
	int day1_p2(void) {
		try {
			auto triplet = Day1::find_triplets_for_target_sum(this->v, 2020);
			int a = triplet[0], b = triplet[1], c=triplet[2];
			return a*b*c;
		} catch (const char *msg) {
			throw msg;
		}
	}

	std::pair<int,int> day1_accounting(void) {
		return std::make_pair(day1_p1(), day1_p2());
	}
};




int
main(void)
{
	{
		std::vector<int> v{1721, 979, 366, 299, 675, 1456};
		Day1 d(v);
		assert( d.day1_p1() == 514579);
		assert( d.day1_p2() == 241861950);
	}

	std::vector<int> v;
	int line;
	while (std::cin >> line) {
		v.push_back(line);
	}

	try {
		Day1 d(v);
		auto p = d.day1_accounting();
		std::cout << p.first << ", " << p.second << std::endl;
	} catch (const char *msg) {
		std::cout << "Caught exception: " << msg << std::endl;
	}

	return 0;
}

