#ifndef __PERMUTATIONS_H__
#define __PERMUTATIONS_H__

#include <vector>
#include <algorithm>
#include <cassert>

using VecInt = std::vector<int>;
using VecVecInt = std::vector<VecInt>;

class Solution {
public:
	VecVecInt permute(VecInt &A);

private:
	void permute(VecInt &A, VecInt &prefix, int level, VecVecInt &results);
};

#endif // __PERMUTATIONS_H__
