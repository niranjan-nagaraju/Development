
'''
Next Permutation

Implement the next permutation, which rearranges numbers into the numerically next greater permutation of numbers for a given array A of size N.
If such arrangement is not possible, it must be rearranged as the lowest possible order i.e., sorted in an ascending order.

Input Format:
	The first and the only argument of input has an array of integers, A.
Output Format:
	Return an array of integers, representing the next permutation of the given array.


Examples:

Input 1:
    A = [1, 2, 3]
Output 1:
    [1, 3, 2]

Input 2:
    A = [3, 2, 1]
Output 2:
    [1, 2, 3]

Input 3:
    A = [1, 1, 5]
Output 3:
    [1, 5, 1]

Input 4:
    A = [20, 50, 113]
Output 4:
    [20, 113, 50]
'''

'''
Solution Outline:
	Consider a sequence x1, x2, ... xk, ..., xn
    Let xk ... xn be a non-increasing sequence (xk >= xk+1 >= ... >= xn)
	Then xk-1 < xk
     Find the successor to xk-1 in xk, ... xn (ie the smallest number in {xk.,,, xn} > xk-1), xs
     Swap xk-1 with xs (so xk-1 now is replaced with the next number immediately greater than it)
     Reverse the entire sequence {xk, ... xk-1, ... xn} so its non-decreasing
	Next permutation: x1, x2, .. xs, xn, ... xk-1, .... xk


Sample run:
   A: [1, 3, 5, 4, 2]
   Non-increasing sequence from the right: [5,4,2]
   Find successor to 3 in [5,4,2] == 4
   Swap [3] with [4] =>
     A: [1, 4, 5, 3, 2]
   Reverse the non-increasing sequence
    A: [1, 4, 2, 3, 5]
'''

def next_permutation(A):
	n = len(A)
	if n <= 1:
		return A

	# Find a non-decreasing sequence from the right
	i = n-1
	while i>0 and A[i] <= A[i-1]:
		i -= 1

	# Entire array is in decreasing order
	# There's no next permutation
	# return reverse of current array
	if i == 0:
		reverse(A, 0, n-1)
		return A

	x = A[i-1]
	nge_idx = find_next_greater_in_reverse_sorted(A, x, i, n-1)
	A[i-1], A[nge_idx] = A[nge_idx], x
	reverse(A, i, n-1)
	return A


# find next greater number of 'key' in reverse-sorted 'lst'
# and return its index
def find_next_greater_in_reverse_sorted(lst, key, l=0, h=None):
	if h == None:
		h = len(lst)-1
	nge_idx = -1
	while l <= h:
		mid = (l+h)/2
		if lst[mid] > key:
			nge_idx = mid
			# Found a candidate for NGE
			# Look to the right so if we can find a better match
			l = mid+1
		else:
			# lst[mid] <= key
			# Look to the left
			h = mid-1

	return nge_idx



# reverse array A[l..h] in-place
def reverse(A, l, h):
	while l <= h:
		A[l], A[h] = A[h], A[l]
		l+=1
		h-=1



if __name__ == '__main__':
	assert next_permutation([]) == []
	assert next_permutation([5]) == [5]
	assert next_permutation([5,4,3,2,1])  == [1,2,3,4,5]
	assert next_permutation([1,3,5,4,2]) == [1,4,2,3,5]
	assert next_permutation([1, 2, 3]) == [1,3,2]
	assert next_permutation([3, 2, 1]) == [1,2,3]
	assert next_permutation([1, 1, 5]) == [1,5,1]
	assert next_permutation([20, 50, 113]) == [20, 113, 50]

	# with repititions
	s = "aabbcc"
	permutations = [
"aabbcc",
"aabcbc",
"aabccb",
"aacbbc",
"aacbcb",
"aaccbb",
"ababcc",
"abacbc",
"abaccb",
"abbacc",
"abbcac",
"abbcca",
"abcabc",
"abcacb",
"abcbac",
"abcbca",
"abccab",
"abccba",
"acabbc",
"acabcb",
"acacbb",
"acbabc",
"acbacb",
"acbbac",
"acbbca",
"acbcab",
"acbcba",
"accabb",
"accbab",
"accbba",
"baabcc",
"baacbc",
"baaccb",
"babacc",
"babcac",
"babcca",
"bacabc",
"bacacb",
"bacbac",
"bacbca",
"baccab",
"baccba",
"bbaacc",
"bbacac",
"bbacca",
"bbcaac",
"bbcaca",
"bbccaa",
"bcaabc",
"bcaacb",
"bcabac",
"bcabca",
"bcacab",
"bcacba",
"bcbaac",
"bcbaca",
"bcbcaa",
"bccaab",
"bccaba",
"bccbaa",
"caabbc",
"caabcb",
"caacbb",
"cababc",
"cabacb",
"cabbac",
"cabbca",
"cabcab",
"cabcba",
"cacabb",
"cacbab",
"cacbba",
"cbaabc",
"cbaacb",
"cbabac",
"cbabca",
"cbacab",
"cbacba",
"cbbaac",
"cbbaca",
"cbbcaa",
"cbcaab",
"cbcaba",
"cbcbaa",
"ccaabb",
"ccabab",
"ccabba",
"ccbaab",
"ccbaba",
"ccbbaa"]
	n = s
	i = 0
	while True:
		assert n == permutations[i]
		i += 1

		n = ''.join(next_permutation(list(n)))
		if n == s:
			break


