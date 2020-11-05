// Find all prime numbers <= n
fn sieve(n: u32) -> Vec<u32> {
    fn build_sieve(current_sieve: Vec<u32>) -> Vec<u32> {
        if current_sieve.len() == 0 {
            return vec![];
        }

        [
            vec![current_sieve[0]], 
            build_sieve(
                current_sieve[1..].iter()
                .filter(|x| *x % current_sieve[0] != 0)
                .map(|x| *x)
                .collect()
            )
        ].concat()
    }
    
    build_sieve([
                vec![2], 
                (3 .. n+1).step_by(2).collect()
            ].concat())
}



fn main() {
	// println!("Sieve: {:#?}: ", sieve(23));
    assert!( sieve(23) == vec![2,3,5,7,11,13,17,19,23] );
    assert!( sieve(25) == vec![2,3,5,7,11,13,17,19,23] );
    assert!( sieve(29) == vec![2,3,5,7,11,13,17,19,23,29] );
}
