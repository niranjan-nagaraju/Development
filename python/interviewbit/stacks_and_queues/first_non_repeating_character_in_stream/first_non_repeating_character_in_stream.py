'''
https://www.interviewbit.com/problems/first-non-repeating-character-in-a-stream-of-characters/

First non-repeating character in a stream of characters

Problem Description
Given a string A denoting a stream of lowercase alphabets. You have to make new string B.
B is formed such that we have to find first non-repeating character each time a character is inserted to the stream and append it at the end to B.
If no non-repeating character is found then append '#' at the end of B.

Problem Constraints
1 <= length of the string <= 100000

Input Format
The only argument given is string A.

Output Format
Return a string B after processing the stream of lowercase alphabets A.

Example Input
Input 1:
 A = "abadbc"
Input 2:
 A = "abcabc"

Example Output
Output 1:
 "aabbdd"
Output 2:
 "aaabc#"

Example Explanation
Explanation 1:
    "a"      -   first non repeating character 'a'
    "ab"     -   first non repeating character 'a'
    "aba"    -   first non repeating character 'b'
    "abad"   -   first non repeating character 'b'
    "abadb"  -   first non repeating character 'd'
    "abadbc" -   first non repeating character 'd'
Explanation 2:
    "a"      -   first non repeating character 'a'
    "ab"     -   first non repeating character 'a'
    "abc"    -   first non repeating character 'a'
    "abca"   -   first non repeating character 'b'
    "abcab"  -   first non repeating character 'c'
    "abcabc" -   no non repeating character so '#'
'''

'''
Solution Outline:
	0. Use a SLL-based queue (with a dummy tail so a specific node can be deleted in O(1))
	   0.1 The Queue only stores unique elements seen so far in the order they are seen.
	1. Use a lookup table of characters to their nodes in the queue.
	2. For each unique character in the stream, enqueue the character to the back of the queue.
		2.1 If the current character is not unique, remove it from the queue.
		2.2 Store the current queue's front into the FNRC-result[] list, if the queue is empty (front contains dummy tail), store a '#' instead.

Sample run:
	Stream: "abccab"
	lookup: []
	Queue: <dummy>
	FNRC: []

	"a"
	lookup[a]: 'a'
	Queue: a -> <dummy>
	FNRC: [a]

	"b"
	lookup[b]: 'b'
	Queue: a -> b -> <dummy>
	FNRC: [a, a]

	"c"
	lookup[c]: 'c'
	Queue: a -> b -> c -> <dummy>
	FNRC: [a, a, a]

	"c"
	lookup[c] is not empty => 'c' was seen earlier
	remove 'c' from the queue, using dummy tail node
	Queue: a -> b -> <dummy> -> <c>
	Queue: a -> b -> <dummy>
	FNRC: [a, a, a, a]

	"a"
	lookup[a] is not empty => 'a' was seen earlier
	remove 'a' from the queue
	Queue: b -> <dummy>
	FNRC: [a, a, a, a, b]

	"b"
	lookup[b] is not empty => 'b' was seen earlier
	remove 'b' from the queue
	Queue: <dummy>
	FNRC: [a, a, a, a, b, #]
'''

toInt = lambda x: (ord(x)-ord('a'))

class SLLQueue:
	class Node:
		def __init__(self, val=None):
			self.val = val
			self.next = None

	def __init__(self):
		# Initialize SLL with a dummy node
		self.head = self.tail = SLLQueue.Node()
		self.lookup = [None]*26 # lowercase alphabets
	
	def __str__(self):
		tmp = self.head
		qstr = []
		while tmp:
			qstr.append(tmp.val if tmp.val else '<None>')
			tmp = tmp.next
		return str(qstr)


	# Add 'x' to the back of the queue
	def enqueue(self, x):
		# Create a new dummy node for tail, and append it
		new = SLLQueue.Node()
		node = self.tail
		node.next = new

		# Use current dummy tail node to store 'x'
		node.val = x
		self.lookup[toInt(x)] = node

		# Make new node the dummy tail node
		self.tail = new


	# return the front of the queue
	def front(self):
		if self.head == self.tail:
			# Queue is empty: return '#'
			# to be stored as fnrc for the current character
			# in the stream
			return '#'
		return self.head.val


	def remove(self, x):
		if self.lookup[toInt(x)] == True:
			# x was seen earlier multiple times
			# There is no longer a reference for 'x' in the queue
			# to remove
			return

		node = self.lookup[toInt(x)]

		# Store x's lookup as 'True' instead of actual node reference
		# so it can still hint that 'x' was seen earlier
		# while making way for the node's storage to be removed
		self.lookup[toInt(x)] = True

		# copy node.next val into node
		# and remove node.next from the list
		node.val = node.next.val
		node.next = node.next.next

		if node.val is not None:
			# node that previously contained x
			# now contains next node's value
			# update lookup table
			self.lookup[toInt(node.val)] = node
		else:  # node.val == None:
			# 'node' was the last 'valid' node in the SLL
			# make it the new dummy tail
			self.tail = node


class Solution:
	def first_non_repeating_character(self, A):
		fnrc = []

		q = SLLQueue()

		for x in A:
			if not q.lookup[toInt(x)]:
				# First time seeing this character in the stream
				# Enqueue it, and store the enqueued node's reference
				# in the lookup table for x
				q.enqueue(x)
			else:
				# x was already seen in the stream
				# remove it from the queue
				q.remove(x)

			# Store current front of the queue into result fnrc
			fnrc.append(q.front())
	
		return ''.join(fnrc)


if __name__ == '__main__':
	s = Solution()
	assert s.first_non_repeating_character("") == ""
	assert s.first_non_repeating_character("abcabc") == "aaabc#"
	assert s.first_non_repeating_character("abccab") == "aaaab#"
	assert s.first_non_repeating_character("abadbc") == "aabbdd"
	assert s.first_non_repeating_character("abcabcdd") == "aaabc#d#"
	assert s.first_non_repeating_character("aabc") == "a#bb"
