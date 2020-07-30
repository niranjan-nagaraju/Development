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

/**
Solution Outline:
    1. Scan for empty slots row-wise, left-right and fill one at a time with numbers
        that are allowed.
    2. Any time, any of 1-9 cannot be placed at a particular cell, backtrack to the previous position that was filled,
        and try to place +1 in its place, and repeat 2. until we are done with all the empty cells.
    3. As a slight optimization, each recursive call need not scan for empty slots from the top-left cell.
        Since we fill cells top-bottom, left-right, we can start from the last empty cell that was filled. 
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/* print a sudoku board */
void print(char **grid)
{
	int row=0, col=0;
	for (row=0; row<9; row++) {
		for (col=0; col<9; col++) {
			printf("%c ", ((char (*)[9])grid)[row][col]);
		}
		printf("\n");
	}
}

/**
 * check if it's safe to put in
 * 'n' in the grid at (x,y)
 */
int is_safe(int x, int y, int n,
			int row_lookup[9][10],
			int col_lookup[9][10],                                                                                                                     
			int mini_grid_lookup[9][10])
{
	// check row x
	if (row_lookup[x][n]) {
		return 0;
	}

	// check column y
	if (col_lookup[y][n]) {
		return 0;
	}

	// check the 3x3 grid
	if (mini_grid_lookup[x/3*3+y/3][n]) {
		return 0;
	}

	return 1;
}

// recursive helper to solve the board
void solveSudokuHelper(char **pgrid,
		char soln[9][9],
		int startrow,
		int startcol,
		int row_lookup[9][10],
		int col_lookup[9][10],                                                                                                                     
		int mini_grid_lookup[9][10])
{

	char (*grid)[9] = (char (*)[9])pgrid;
	int i=startrow, j=startcol, x;

	// Start with scanning from (startrow, startcol)
	for (j=startcol; j<9; j++) {
		if (grid[i][j] == '.') {
			goto StopIteration;
		}
	}
																																				
	for (i=startrow+1; i<9; i++) {
		for (j=0; j<9; j++) {
			if (grid[i][j] == '.') {
				goto StopIteration;
			}
		}
	}

	// Couldn't find any empty cells
	// We have a complete board
	// print((char **)grid);
	for (i=0; i<9; i++)
		for (j=0; j<9; j++)
			soln[i][j] = grid[i][j];
	return;

StopIteration:
	for (x=1; x<10; x++)  {
		if (is_safe(i, j, x, row_lookup, col_lookup, mini_grid_lookup)) {
			grid[i][j] = x+'0';

			row_lookup[i][x] = 1;
			col_lookup[j][x] = 1;
			mini_grid_lookup[i/3*3+j/3][x] = 1;

			// We are filling cells top-bottom
			// skip checking all previous rows for empty cells
			// we would have filled them at this point
			solveSudokuHelper((char **)grid, soln, i, j+1, row_lookup, col_lookup, mini_grid_lookup);
			grid[i][j] = '.'; // backtrack

			row_lookup[i][x] = 0;
			col_lookup[j][x] = 0;
			mini_grid_lookup[i/3*3+j/3][x] = 0;
		}
	}
	// Either we couldn't fit in any number in the current cell (i,j)
	// or one of the lower levels couldn't
	// return back to upper levels to retry with the next number
	// print(grid);
	return;
}


/**
 * @input A : 2D char array 
 * @input n11 : char array's ( A ) rows
 * @input n12 : char array's ( A ) columns
 * 
 * @Output Void. Just modifies the args passed by reference 
 */
void solveSudoku(char **pgrid, int n11, int n12)
{
	char (*grid)[9] = (char (*)[9])pgrid;
	char soln[9][9];
	int i, j;
	int row_lookup[9][10] = {{0}};
	int col_lookup[9][10] = {{0}};
	int mini_grid_lookup[9][10] = {{0}};
	int row, col;

	for (row=0; row<9; row++) {
		for (col=0; col<9; col++) {
			if (grid[row][col] >= '1' && grid[row][col] <= '9') {
				row_lookup[row][grid[row][col]-'0'] = 1;
				col_lookup[col][grid[row][col]-'0'] = 1;
				// The 3x3 grids are
				// (0,0)->(2,2), (0,3)->(2,5), (0,6)->(2,8)
				// (3,0)->(5,2), (3,3)->(5,5), (3,6)->(5,8)
				// (6,0)->(8,2), (6,3)->(8,5), (6,6)->(8,8)
				// cell(4,5) is grid (3,3)->(5,5) -> grid 4
				//  4/3*3 + 5/3 = 3+1 = 4
				// cell(7,6) is grid(6,6)->(8,8) -> grid 8
				//  7/3*3+6/3 = 6+2 == 8
				mini_grid_lookup[row/3*3+col/3][grid[row][col]-'0'] = 1;
			}
		}
	}

	solveSudokuHelper(pgrid, soln, 0, 0, row_lookup, col_lookup, mini_grid_lookup);

	for (i=0; i<9; i++)
		for (j=0; j<9; j++)
			grid[i][j] = soln[i][j];
}


int main(void)
{
	char completed[9][9] = 
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


	{
		char grid[9][9] = 
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

		solveSudoku((char **)grid, 9, 9);
		print((char **)grid);

		for (int i=0; i<9; i++)
			for (int j=0; j<9; j++)
				assert (grid[i][j] == completed[i][j]);
	} 
	{
		char grid[9][9] = 
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

		/** Initialize using malloc */
		char (*pgrid)[9] = (char (*)[9])malloc(9*9*sizeof(char));
		for (int i=0; i<9; i++)
			for (int j=0; j<9; j++)
				 pgrid[i][j] = grid[i][j];

		solveSudoku((char **)pgrid, 9, 9);
		// print((char **)pgrid);
		for (int i=0; i<9; i++)
			for (int j=0; j<9; j++)
				assert (pgrid[i][j] == completed[i][j]);
		free(pgrid);
	}

	return 0;
}


