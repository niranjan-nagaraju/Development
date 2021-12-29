/**
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
*/

use std::io::{self, Read};
#[derive(Debug, PartialEq)]
struct TreesMap {
	rows: usize,
	cols: usize,
	grid: Vec<String>,
}


impl TreesMap {
	pub fn new( v : Vec<String> ) -> Self {
		let rows = v.len();
		let cols = v[0].len();

		Self{ rows, cols, grid: v }
	}

	// get tree/blank-space at row `idx` per scheme
	// scheme
    //  1 -> Right 1, down 1.
    //  2 -> Right 3, down 1.
    //  3 -> Right 5, down 1.
    //  4 -> Right 7, down 1.
    //  5 -> Right 1, down 2.
	pub fn get(&self, idx: usize, row_: &String, scheme : u32) -> char {
		let x = idx % self.rows;
		let row = row_.as_bytes();
		let y = idx;
		match scheme {
			1 => row[y % self.cols] as char,
			2 => row[y*3 % self.cols] as char,
			3 => row[y*5 % self.cols] as char,
			4 => row[y*7 % self.cols] as char,
			5 => { if x % 2 == 1 { '.' } else { row[y/2 % self.cols] as char } },
			_ => '.',
		}
	}

	pub fn p1(&self) -> usize {
		self.grid
			.iter()
			.enumerate()
			.filter(|(idx, row)| (self.get(*idx, row, 2) == '#' ) )
			.count()
	}

	pub fn p2(&self) -> usize {
		(1..=5)
			.map(|x| self.grid
				.iter()
				.enumerate()
				.filter(|(idx, row)| (self.get(*idx, row, x) == '#' ) )
				.count()
			).product::<usize>()
	}
}


fn main() {
	let mut trees_map = String::new();
	io::stdin().read_to_string(&mut trees_map)
		.ok()
		.expect("failed reading trees map");
	let trees_map : Vec<String> = trees_map
		.lines()
		.map( |l| l.to_string() )
		.collect();

	let trees_map = TreesMap::new( trees_map );
	println!("p1: {}, p2: {}", trees_map.p1(), trees_map.p2());
}


#[cfg(test)]
mod tests_day3 {
	use super::*;

	#[test]
	fn test_get() {
		let test_in = vec![
			"..##.......".to_string(),
			"#...#...#..".to_string(),
			".#....#..#.".to_string(),
			"..#.#...#.#".to_string(),
			".#...##..#.".to_string(),
			"..#.##.....".to_string(),
			".#.#.#....#".to_string(),
			".#........#".to_string(),
			"#.##...#...".to_string(),
			"#...##....#".to_string(),
			".#..#...#.#".to_string(),
		];

		let expanded_grid : Vec<&str> = vec![
			"..##.........##.........##.........##.........##.........##.......",
			"#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..",
			".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.",
			"..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#",
			".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.",
			"..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....",
			".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#",
			".#........#.#........#.#........#.#........#.#........#.#........#",
			"#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...",
			"#...##....##...##....##...##....##...##....##...##....##...##....#",
			".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#",
		];

		let tm = TreesMap::new( test_in );
		assert_eq!( tm.cols, 11 );
		assert_eq!( tm.rows, 11 );
		assert_eq!( tm.get(1, &tm.grid[1], 1 ), expanded_grid[1].as_bytes()[1] as char);
		assert_eq!( tm.get(1, &tm.grid[1], 2 ), expanded_grid[1].as_bytes()[3] as char);
		assert_eq!( tm.get(1, &tm.grid[1], 3 ), expanded_grid[1].as_bytes()[5] as char);
		assert_eq!( tm.get(1, &tm.grid[1], 4 ), expanded_grid[1].as_bytes()[7] as char);
		assert_eq!( tm.get(1, &tm.grid[1], 5 ), '.' );

		assert_eq!( tm.get(6, &tm.grid[6], 1 ), expanded_grid[6].as_bytes()[6] as char);
		assert_eq!( tm.get(6, &tm.grid[6], 2 ), expanded_grid[6].as_bytes()[18] as char);
		assert_eq!( tm.get(6, &tm.grid[6], 3 ), expanded_grid[6].as_bytes()[30] as char);
		assert_eq!( tm.get(6, &tm.grid[6], 4 ), expanded_grid[6].as_bytes()[35] as char);
		assert_eq!( tm.get(6, &tm.grid[6], 5 ), expanded_grid[6].as_bytes()[3] as char);
	}

	#[test]
	fn test_day3() {
		let test_in = vec![
			"..##.......",
			"#...#...#..",
			".#....#..#.",
			"..#.#...#.#",
			".#...##..#.",
			"..#.##.....",
			".#.#.#....#",
			".#........#",
			"#.##...#...",
			"#...##....#",
			".#..#...#.#",
		];
		let tm = TreesMap::new( test_in
								.iter()
								.map(|x| x.to_string())
								.collect()
							);
		assert_eq!( tm.p1(), 7 );
		assert_eq!( tm.p2(), 336 );
	}


}


