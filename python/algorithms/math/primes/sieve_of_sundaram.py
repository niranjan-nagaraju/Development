#!/usr/bin/python

# Generate a sieve of Sundaram - a list of primes <= n


'''
1. Start with marking all numbers (i+j+2ij)
   i: 1 to n
   j: i until (i+j+2ij <= n)

2. For all unmarked numbers between i: 1 to n,
   2i+1 is a prime.

3. The number 2 is an edge-case, add manually after the sieve.
'''


'''
Sample run:
    n = 20

    Step 1:
        i: 1
            j: 1, k: i+j+2ij = 1+1+2(1)(1) = 4, mark: [4]
            j: 2, k: i+j+2ij = 1+2+2(1)(2) = 7, mark: [4,7]
            j: 3, k: i+j+2ij = 1+3+2(1)(3) = 10, mark: [4,7,10]
            j: 4, k: i+j+2ij = 1+4+2(1)(4) = 13, mark: [4,7,10,13]
            j: 5, k: i+j+2ij = 1+5+2(1)(5) = 16, mark: [4,7,10,13,16]
            j: 6, k: i+j+2ij = 1+6+2(1)(6) = 19, mark: [4,7,10,13,16,19]
            j: 7, k: i+j+2ij = 1+7+2(1)(7) = 22 > n
        i: 2
            j: 2, k: i+j+2ij = 2+2+2(2)(2) = 12, mark: [4,7,10,12,13,16,19]
            j: 3, k: i+j+2ij = 2+3+2(2)(3) = 17, mark: [4,7,10,12,13,16,17,19]
            j: 4, k: i+j+2ij = 2+4+2(2)(4) = 22 > n
        i: 3
            j: 3, k: i+j+2ij = 3+3+2(3)(3) = 26

   
   Step 2:
        i: 1, 2i+1 = 2(1)+1 = 3 => primes: [3]
        i: 2, 2i+1 = 2(2)+1 = 5 => primes: [3,5]
        i: 3, 2i+1 = 2(3)+1 = 7 => primes: [3,5,7]
        i: 4, marked, SKIP
        i: 5, 2i+1 = 2(5)+1 = 11 => primes: [3,5,7,11]
        i: 6, 2i+1 = 2(6)+1 = 13 => primes: [3,5,7,11,13]
        i: 7, marked, SKIP
        i: 8, 2i+1 = 2(8)+1 = 17 => primes: [3,5,7,11,13,17]
        i: 9, 2i+1 = 2(9)+1 = 19 => primes: [3,5,7,11,13,17,19]
        i: 10, marked: SKIP
        i: 11, 2i+1 = 2(11)+1 = 23 => primes: [3,5,7,11,13,17,19,23]
        i: 12, marked, SKIP
        i: 13, marked, SKIP
        i: 14, 2i+1 = 2(14)+1 = 29 => primes: [3,5,7,11,13,17,19,23,29]
        i: 15, 2i+1 = 2(15)+1 = 31 => primes: [3,5,7,11,13,17,19,23,29,31]
        i: 18, 2i+1 = 2(18)+1 = 37 => primes: [3,5,7,11,13,17,19,23,29,31,37]
        i: 20, 2i+1 = 2(20)+1 = 41 => primes: [3,5,7,11,13,17,19,23,29,31,37,41]
'''
def sieve(n):
    primes = [2]
    marked_numbers = set()
    for i in range(1, n+1):
        #print '{}'.format(i)
        j = i

        if (i + j + (2 * i * j)) > n:
            # Break if the very first i+j+2ij in the loop is already > n
            break

        while True:
            k = i + j + (2 * i * j)
            if k > n:
                break
            marked_numbers.add(k)
            #print '  {} {}'.format(j, k)
            j += 1

    for i in range(1, n+1):
        if 2*i+1 > n:
            break
        if i not in marked_numbers:
            primes.append(2 * i + 1)

    return primes


if __name__ == '__main__':
    assert (sieve(10) ==  [2,3,5,7])
    assert (sieve(20) ==  [2,3,5,7,11,13,17,19])
    assert (sieve(53) ==  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53])
    assert (sieve(100) ==  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97])




