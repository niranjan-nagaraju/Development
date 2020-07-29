/**
https://www.interviewbit.com/problems/sudoku/

Sudoku

Write a program to solve a Sudoku puzzle by filling the empty cells.
Empty cells are indicated by the character '.'
You may assume that there will be only one unique solution.

+===+===+===+===+===+===+===+===+===+
❚ 5 | 3 |   ❚   | 7 |   ❚   |   |   ❚
+---+---+---+---+---+---+---+---+---+
❚ 6 |   |   ❚ 1 | 9 | 5 ❚   |   |   ❚
+---+---+---+---+---+---+---+---+---+
❚   | 9 | 8 ❚   |   |   ❚   | 6 |   ❚ 
+===+===+===+===+===+===+===+===+===+
❚ 8 |   |   ❚   | 6 |   ❚   |   | 3 ❚
+---+---+---+---+---+---+---+---+---+
❚ 4 |   |   ❚ 8 |   | 3 ❚   |   | 1 ❚
+---+---+---+---+---+---+---+---+---+
❚ 7 |   |   ❚   | 2 |   ❚   |   | 6 ❚
+===+===+===+===+===+===+===+===+===+
❚   | 6 |   ❚   |   |   ❚ 2 | 8 |   ❚
+---+---+---+---+---+---+---+---+---+
❚   |   |   ❚ 4 | 1 | 9 ❚   |   | 5 ❚
+---+---+---+---+---+---+---+---+---+
❚   |   |   ❚   | 8 |   ❚   | 7 | 9 ❚
+===+===+===+===+===+===+===+===+===+

A sudoku puzzle,

+===+===+===+===+===+===+===+===+===+
❚ 5 | 3 | 𝟜 ❚ 𝟞 | 7 | 𝟠 ❚ 𝟡 | 𝟙 | 𝟚 ❚
+---+---+---+---+---+---+---+---+---+
❚ 6 | 𝟟 | 𝟚 ❚ 1 | 9 | 5 ❚ 𝟛 | 𝟜 | 𝟠 ❚
+---+---+---+---+---+---+---+---+---+
❚ 𝟙 | 9 | 8 ❚ 𝟛 | 𝟜 | 𝟚 ❚ 𝟝 | 6 | 𝟟 ❚ 
+===+===+===+===+===+===+===+===+===+
❚ 8 | 𝟝 | 𝟡 ❚ 𝟟 | 6 | 𝟙 ❚ 𝟜 | 𝟚 | 3 ❚
+---+---+---+---+---+---+---+---+---+
❚ 4 | 𝟚 | 𝟞 ❚ 8 | 𝟝 | 3 ❚ 𝟟 | 𝟡 | 1 ❚
+---+---+---+---+---+---+---+---+---+
❚ 7 | 𝟙 | 𝟛 ❚ 𝟡 | 2 | 𝟜 ❚ 𝟠 | 𝟝 | 6 ❚
+===+===+===+===+===+===+===+===+===+
❚ 𝟡 | 6 | 𝟙 ❚ 𝟝 | 𝟛 | 𝟟 ❚ 2 | 8 | 𝟜 ❚
+---+---+---+---+---+---+---+---+---+
❚ 𝟚 | 𝟠 | 𝟟 ❚ 4 | 1 | 9 ❚ 𝟞 | 𝟛 | 5 ❚
+---+---+---+---+---+---+---+---+---+
❚ 𝟛 | 𝟜 | 𝟝 ❚ 𝟚 | 8 | 𝟞 ❚ 𝟙 | 7 | 9 ❚
+===+===+===+===+===+===+===+===+===+

and its solution numbers marked in red.

Example :
For the above given diagrams, the corresponding input to your program will be
	[[53..7....], [6..195...], [.98....6.], [8...6...3], [4..8.3..1], [7...2...6], [.6....28.], [...419..5], [....8..79]]
and we would expect your program to modify the above array of array of characters to
	[[534678912], [672195348], [198342567], [859761423], [426853791], [713924856], [961537284], [287419635], [345286179]]
*/

#include <iostream>
#include <vector>
#include <cassert>

class Solution
{
	public:
		// print a sudoku board
		void print(std::vector<std::vector<char>> &grid) {
			for (auto row : grid) {
				for (char c : row) {
					std::cout << " " << c;
				}
				std::cout << std::endl;
			}
		}

		// check if it's safe to put in
		// 'n' in the grid at (x,y)
		bool is_safe(std::vector<std::vector<char>> &grid, int x, int y, int n) {
			// check row x
			for (int j=0; j<9; j++)
				if (grid[x][j] == n)
					return false;

			// check column y
			for (int i=0; i<9; i++)
				if (grid[i][y] == n)
					return false;

			// check the 3x3 grid
			// The 3x3 grids are
			// (0,0)->(2,2), (0,3)->(2,5), (0,6)->(2,8)
			// (3,0)->(5,2), (3,3)->(5,5), (3,6)->(5,8)
			// (6,0)->(8,2), (6,3)->(8,5), (6,6)->(8,8)
			// cell(4,5) is grid (3,3)->(5,5)
			//  (4/3),(5/3) == 1,1 -> *3 = 3,3 (start)
			// cell(7,6) is grid(6,6)->(8,8)
			// (7/3),(6/3) == 2,2 -> *3 == 6,6 (start)
			for (int i=0; i<3; i++)
				for (int j=0; j<3; j++)
					if ( grid[(x/3)*3+i][(y/3)*3+j] == n )
						return false;

			return true;
		}

		// recursive helper to solve the board
		void solveSudokuHelper(std::vector<std::vector<char>> &grid, std::vector<std::vector<char>> &soln) {
			for (int i=0; i<9; i++) {
				for (int j=0; j<9; j++) {
					if (grid[i][j] != '.')
						continue;

					for (int x=1; x<10; x++)  {
						if (is_safe(grid, i, j, x+'0')) {
							grid[i][j] = x+'0';
							solveSudokuHelper(grid, soln);
							grid[i][j] = '.'; // backtrack
						}
					}
					return;
				}
			}
			// We have a complete board
			// print(grid);
			std::copy(grid.begin(), grid.end(), std::back_inserter(soln));
		}


		void solveSudoku(std::vector<std::vector<char>> &grid) {
			std::vector<std::vector<char>> soln;
			solveSudokuHelper(grid, soln);

			for (int i=0; i<9; i++)
				grid[i] = soln[i];
		}
};


int main(void)
{
	Solution s;
	std::vector<std::vector<char>> grid = 
			{
				{'5', '3', '.', '.', '7', '.', '.', '.', '.'},
				{'6', '.', '.', '1', '9', '5', '.', '.', '.'},
				{'.', '9', '8', '.', '.', '.', '.', '6', '.'},
				{'8', '.', '.', '.', '6', '.', '.', '.', '3'},
				{'4', '.', '.', '8', '.', '3', '.', '.', '1'},
				{'7', '.', '.', '.', '2', '.', '.', '.', '6'},
				{'.', '6', '.', '.', '.', '.', '2', '8', '.'},
				{'.', '.', '.', '4', '1', '9', '.', '.', '5'},
				{'.', '.', '.', '.', '8', '.', '.', '7', '9'}
			};

	std::vector<std::vector<char>> completed = 
			{
				{'5', '3', '4', '6', '7', '8', '9', '1', '2'},
				{'6', '7', '2', '1', '9', '5', '3', '4', '8'},
				{'1', '9', '8', '3', '4', '2', '5', '6', '7'},
				{'8', '5', '9', '7', '6', '1', '4', '2', '3'},
				{'4', '2', '6', '8', '5', '3', '7', '9', '1'},
				{'7', '1', '3', '9', '2', '4', '8', '5', '6'},
				{'9', '6', '1', '5', '3', '7', '2', '8', '4'},
				{'2', '8', '7', '4', '1', '9', '6', '3', '5'},
				{'3', '4', '5', '2', '8', '6', '1', '7', '9'}
			};

	assert( s.is_safe(grid, 0, 2, '6') == false);
	assert( s.is_safe(grid, 0, 2, '1') == true);
	assert( s.is_safe(grid, 6, 8, '1') == false);
	assert( s.is_safe(grid, 6, 8, '1') == false);
	assert( s.is_safe(grid, 6, 8, '3') == false);

	s.solveSudoku(grid);
	s.print(grid);
	assert (grid == completed);

	return 0;
}


