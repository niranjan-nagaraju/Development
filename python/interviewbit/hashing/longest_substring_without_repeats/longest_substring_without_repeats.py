'''
https://www.interviewbit.com/problems/longest-substring-without-repeat/

Longest Substring Without Repeat

Given a string,
find the length of the longest substring without repeating characters.

Example:
	The longest substring without repeating letters for "abcabcbb" is "abc", which the length is 3.
	For "bbbbb" the longest substring is "b", with the length of 1.
'''


'''
Solution outline:
	1. Use a hash-table with a sliding window.
		1.1. Store last-seen index of a character in the hash table
		1.2. Two pointers keep track of the current window,
				a window will always contain only distinct characters.
		1.3. The maximum-width window at any given time is the length of the longest substring without repeats.
	2. Start with adding the first character to the hash-table, along with its index as 0.
		2.0.  Start-of-window : 0, end-of-window: 0, max-window: 1
		2.1. For every succeeding character, c, in the string,
			2.1.1.  If the character already is in the hash table, check if its in the current window [start-of-window, end-of-window]
					If it is, Adding c will introduce a duplicate to the window.
					Get c's index from the hash-table and slide window to its next character.
					Add c to current window, update hash-table entry for c to new index.
					  start-of-window: [prev c index +1]
					  end-of-window: [current c index]
			2.1.2. If c is either not in the hash-table or if its index is outside the current window (to the left of current window, specifically),
					current window can be extended to include c
					end-of-window: [current index]
			2.1.3. If the current window length > max-window, update max-window
	3. return max-window
		

Sample run:
    A: "abcdbacea"
    hash-table: {
        'a': 0    
    }
    window: 'a'
    max-window: 1

    1: 'b', not in hash-table
    hash-table: {
        'a': 0
        'b': 1
    }
    window: 'ab'
    cur-window: 2 > max-window
    max-window: 2

    2: 'c', not in hash-table
    hash-table: {
        'a': 0
        'b': 1
        'c': 2
    }
    window: 'abc'
    cur-window: 3 > max-window
    max-window: 3

    3: 'd', not in hash-table
    hash-table: {
        'a': 0
        'b': 1
        'c': 2
        'd': 3
    }
    window: 'abcd'
    cur-window: 4 > max-window
    max-window: 4

    4: 'b' found in hash-table, and is part of current window
    shrink current window to start from right of 'b'
    window: 'cd'
    Add 'b' to current window
    hash-table: {
        'a': 0
        'b': 4
        'c': 2
        'd': 3
    }
    window: 'cdb'
    curr-window: 3
    max-window: 4

    5: 'a' found in hash-table, but outside of current window
    Add 'a' to current window
    hash-table: {
        'a': 5
        'b': 4
        'c': 2
        'd': 3
    }
    window: 'cdba'
    curr-window: 4 == max-window
    max-window: 4

    6: 'c' found in hash-table, and is part of current window
    Shrink current window to start from right of 'c'
    window: 'dba'
    Add 'c' to current window
    hash-table: {
        'a': 5
        'b': 4
        'c': 6
        'd': 3
    }
    window: 'dbac'
    curr-window: 4 == max-window
    max-window: 4

    7. 'e' is not in hash-table
    Add 'e' to current window
    hash-table: {
        'a': 5
        'b': 4
        'c': 6
        'd': 3
        'e': 7
    }
    window: 'dbace'
    curr-window: 5 == max-window
    max-window: 5

    8. 'a' found in hash-table at 5
    Shrink current window to start from right of 'a'
    window: 'ce'
    Add 'a' to current window
    hash-table: {
        'a': 8
        'b': 4
        'c': 6
        'd': 3
        'e': 7
    }
    window: 'cea'
    curr-window: 3
    max-window: 5

return 5
'''
class Solution:
	def find_longest_distinct_substring(self, A):
		if not A:
			return 0

		index_lookup = {A[0]: 0}
		sow = eow = 0
		max_window_len = 1
		for i in xrange(1, len(A)):
			idx = index_lookup.get(A[i])
			if idx is not None:
				# Found A[i] in hash-table
				if idx >= sow:
					# A[i] is already in current window
					# shrink current window to start from idx+1
					sow = idx+1

			# Add A[i] to current window
			eow = i
			index_lookup[A[i]] = i
			if (eow-sow+1) > max_window_len:
				max_window_len = eow-sow+1

		return max_window_len



if __name__ == '__main__':
	s = Solution()
	assert s.find_longest_distinct_substring("abcdbacea") == 5
	assert s.find_longest_distinct_substring("abcabcbb") == 3
	assert s.find_longest_distinct_substring("bbbbb") == 1
					




