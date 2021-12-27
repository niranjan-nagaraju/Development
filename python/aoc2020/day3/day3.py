'''
https://adventofcode.com/2020/day/3

--- Day 3: Toboggan Trajectory ---

With the toboggan login problems resolved, you set off toward the airport. While travel by toboggan might be easy, it's certainly not safe: there's very minimal steering and the area is covered in trees. You'll need to see which angles will take you near the fewest trees.

Due to the local geology, trees in this area only grow on exact integer coordinates in a grid. You make a map (your puzzle input) of the open squares (.) and trees (#) you can see. For example:

..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#

These aren't the only trees, though; due to something you read about once involving arboreal genetics and biome stability, the same pattern repeats to the right many times:

..##.........##.........##.........##.........##.........##.......  --->
#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........#.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...##....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

You start on the open square (.) in the top-left corner and need to reach the bottom (below the bottom-most row on your map).

The toboggan can only follow a few specific slopes (you opted for a cheaper model that prefers rational numbers); start by counting all the trees you would encounter for the slope right 3, down 1:

From your starting position at the top-left, check the position that is right 3 and down 1. Then, check the position that is right 3 and down 1 from there, and so on until you go past the bottom of the map.

The locations you'd check in the above example are marked here with O where there was an open square and X where there was a tree:

..##.........##.........##.........##.........##.........##.......  --->
#..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
.#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
.#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
.#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
.#........#.#........X.#........#.#........#.#........#.#........#
#.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
#...##....##...##....##...#X....##...##....##...##....##...##....#
.#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->

In this example, traversing the map using this slope would cause you to encounter 7 trees.

Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter? 274

--- Part Two ---

Time to check the rest of the slopes - you need to minimize the probability of a sudden arboreal stop, after all.

Determine the number of trees you would encounter if, for each of the following slopes, you start at the top-left corner and traverse the map all the way to the bottom:

    Right 1, down 1.
    Right 3, down 1. (This is the slope you already checked.)
    Right 5, down 1.
    Right 7, down 1.
    Right 1, down 2.

In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.

What do you get if you multiply together the number of trees encountered on each of the listed slopes?
'''
import sys
import pprint
class Day3:
	def __init__( self, trees_map ):
		self.trees_map = trees_map
		self.rows = len( trees_map )
		self.cols = len( trees_map[0] )
	
	# return '#' or '.' at (x,y)
	def get( self, x, y ):
		x = x % self.rows
		y = y % self.cols
		return self.trees_map[x][y]

	# part 1
	def num_trees_in_path( self, right=3, down=1 ):
		x, y = 0, 0
		trees_count = 0
		while x < self.rows:
			if self.get(x, y) == '#':
				trees_count += 1
			y += right
			x += down
		return trees_count

	# part 2
	def count_aggregate_trees_across_slopes( self ):
		product = lambda lst: reduce( lambda acc, x: acc*x, lst, 1 )
		slopes = ( (1,1), (3,1), (5,1), (7,1), (1,2) )
		return product( (self.num_trees_in_path(x, y) for x,y in slopes) )


def test_driver():
	test_in = [
		'..##.......',
		'#...#...#..',
		'.#....#..#.',
		'..#.#...#.#',
		'.#...##..#.',
		'..#.##.....',
		'.#.#.#....#',
		'.#........#',
		'#.##...#...',
		'#...##....#',
		'.#..#...#.#',
	]

	expanded_grid = [
		'..##.........##.........##.........##.........##.........##.......',
		'#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..',
		'.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.',
		'..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#',
		'.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.',
		'..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....',
		'.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#',
		'.#........#.#........#.#........#.#........#.#........#.#........#',
		'#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...',
		'#...##....##...##....##...##....##...##....##...##....##...##....#',
		'.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#',
	]

	d = Day3( test_in )
	assert d.get( 1, 2 ) == expanded_grid[1][2]
	assert d.get( 10, 6 ) == expanded_grid[10][6]
	assert d.get( 10, 25 ) == expanded_grid[10][25]
	assert d.get( 5, 18 ) == expanded_grid[5][18]
	assert d.num_trees_in_path() == 7
	assert d.count_aggregate_trees_across_slopes() == 336


if __name__ == '__main__':
	test_driver()
	
	# read stdin
	trees_map = [ l.strip() for l in sys.stdin ]
	d = Day3( trees_map )
	print d.num_trees_in_path() # part 1
	print d.count_aggregate_trees_across_slopes() # part 2

