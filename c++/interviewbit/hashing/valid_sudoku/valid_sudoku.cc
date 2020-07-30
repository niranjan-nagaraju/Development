/**
https://www.interviewbit.com/problems/valid-sudoku/

Determine if a Sudoku is valid, according to: http://sudoku.com.au/TheRules.aspx
The Sudoku board could be partially filled, where empty cells are filled with the character ‘.’.

Example:
["53..7....", "6..195...", ".98....6.", "8...6...3", "4..8.3..1", "7...2...6", ".6....28.", "...419..5", "....8..79"]
A partially filled sudoku which is valid.

 Note:
 A valid Sudoku board (partially filled) is not necessarily solvable. Only the filled cells need to be validated.
 Return 0 / 1 ( 0 for false, 1 for true ) for this problem
*/


/**
Solution Outline:
	1. Maintain lookup tables for the nine rows, columns and mini-3x3 grids
	2. The mini-3x3 grids can be indexed using (row/3*3+col/3) as index.
*/

#include <vector>
#include <string>
#include <cassert>

class Solution
{
	public:
		int isValidSudoku(const std::vector<std::string> &A);
};

int Solution::isValidSudoku(const std::vector<std::string> &A)
{
	int row_lookup[9][10] = {{0}};
	int col_lookup[9][10] = {{0}};
	int mini_grid_lookup[9][10] = {{0}};

	for (int row=0; row<9; row++) {
		for (int col=0; col<9; col++) {
			char x = A[row][col]; 
			if (x == '.')
				continue;

			if (row_lookup[row][x-'0'])
				return 0;

			if (col_lookup[col][x-'0'])
				return 0;

			if (mini_grid_lookup[row/3*3+col/3][x-'0'])
				return 0;

			row_lookup[row][x-'0'] = 1;
			col_lookup[col][x-'0'] = 1;
			mini_grid_lookup[row/3*3+col/3][x-'0'] = 1;
		}
	}
	return 1;
}

int main(void)
{
	Solution s;
	std::vector<std::string> A =
			{
				"..5.....6",
				"....14...",
				".........",
				".....92..",
				"5....2...",
				".......3.",
				"...54....",
				"3.....42.",
				"...27.6.."
			};
	assert(s.isValidSudoku(A) == 1);

	std::vector<std::string> B =
			{
				"..5.....6",
				"....14...",
				".........",
				".....92..",
				"5....2...",
				".......3.",
				"...54....",
				"3.....42.",
				"...27.6.4"
			};
	assert(s.isValidSudoku(B) == 0);

	return 0;
}

