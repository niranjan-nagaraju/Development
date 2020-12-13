use std::collections::HashSet;
use std::io::{self, BufRead};

struct Day1 {}


impl Day1 {
	/* 
	 * Iterate over a generic iterable of i32 items, and
	 * return a pair that adds up to a target sum
	 */
	pub fn sum_exists<I>(lst: &I, target: i32) -> (i32, i32)
		where for<'a> &'a I: IntoIterator<Item = &'a i32>
	{
		let mut lookup = HashSet::<i32>::new();

		for &x in lst {
			if !lookup.contains(&(target-x)) {
				lookup.insert(x);
			} else {
				return (target-x, x);
			}
		}

		(-1, -1)
	}


	/*
	 * Return indices(i,j) s.t A[i]+A[j] == target sum
	 * The input list, A[lh..] is assumed to be sorted
	 * so the target sum can be found in O(n)
	 */
	pub fn pair_target_sum(lst: &Vec<i32>, lh: usize, target: i32)
			-> Option<(i32, i32)> {
		let mut l = lh;
		let mut r = lst.len()-1;

		while l < r {
			let curr_sum = lst[l] + lst[r];
			if curr_sum == target {
				return Some((lst[l], lst[r]));
			}
			else if curr_sum < target {
				l+=1;
			}
			else { // curr_sum > target
				r-=1;
			}
		}

		None
	}


	/*
	 * Find a triplet that add upto a target sum
	 */
	pub fn triplet_sum(lst: &mut Vec<i32>, target: i32) -> Option<(i32, i32, i32)> {
		lst.sort();
		for i in 0..lst.len()-2 {
			match Day1::pair_target_sum(&lst, i+1, target-lst[i]) {
				None => {}, /* Couldn't find target sum containing lst[i], move to i+1 */
				Some((x, y)) => return Some((lst[i], x, y)),
			}    
		}
		None
	}
}






fn main() {
  let stdin = io::stdin();
  let mut lst: Vec<i32> = stdin
	  .lock()
	  .lines()
	  .filter_map(|line_result| line_result.ok())
	  .filter_map(|line| line.parse::<i32>().ok())
	  .collect();

	let (x,y) = Day1::sum_exists(&lst, 2020);
	println!("Pair {} {}", x, y);
	println!("Part I: {}", x*y);

	// PART 2
	match Day1::triplet_sum(&mut lst, 2020) {
		Some((x,y,z)) => {
			println!("Triplet: {} {} {}", x, y, z);
			println!("Part II: {}", x * y * z);

		},
		None => println!("No such triplet"),
	}
}



#[cfg(test)]
mod tests_day1 {
    #[test]
    fn basic_tests_day1() {
        let a = vec![1,5,8,9,2,3,7];
        assert_eq!(crate::Day1::sum_exists(&a, 15), (8,7));
        assert_eq!(crate::Day1::sum_exists(&a, 14), (5,9));
        assert_eq!(crate::Day1::sum_exists(&a, 18), (-1,-1));
    }

	/* Test part 2 */
    #[test]
    fn basic_tests_day1p2() {
        let mut a = vec![1,5,8,9,2,3,7];
		a.sort();
        assert_eq!(crate::Day1::pair_target_sum(&a, 0, 15), Some((7,8)));
        assert_eq!(crate::Day1::pair_target_sum(&a, 0, 14), Some((5,9)));
        assert_eq!(crate::Day1::pair_target_sum(&a, 0, 18), None);

        let mut b = vec![1,5,8,9,3,7];
        assert_eq!(crate::Day1::triplet_sum(&mut b, 15), Some((1,5,9)));
        assert!(b == [1,3,5,7,8,9]);
        assert_eq!(crate::Day1::triplet_sum(&mut b, 22), Some((5,8,9)));
        assert_eq!(crate::Day1::triplet_sum(&mut b, 24), Some((7,8,9)));
        assert_eq!(crate::Day1::triplet_sum(&mut b, 50), None);
    }
}


