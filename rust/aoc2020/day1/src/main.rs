#[cfg(test)]
mod tests_day1 {
    #[test]
    fn basic_tests_day1() {
        let a = vec![1,5,8,9,2,3,7];
        assert_eq!(crate::sum_exists(&a, 15), (8,7));
        assert_eq!(crate::sum_exists(&a, 14), (5,9));
        assert_eq!(crate::sum_exists(&a, 18), (-1,-1));
    }
}


use std::collections::HashSet;
use std::io::{self, BufRead};


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


fn main() {
  let stdin = io::stdin();
  let lst: Vec<i32> = stdin
	  .lock()
	  .lines()
	  .filter_map(|line_result| line_result.ok())
	  .filter_map(|line| line.parse::<i32>().ok())
	  .collect();

	let (x,y) = sum_exists(&lst, 2020);
	println!("{} {}", x, y);
	println!("{}", x*y);
}

