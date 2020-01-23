'''
Forward Binary search on a sorted list

Usually performed when the length of a list is unknonwn before hand / a  semi-infinite stream of sorted items,
A forward binary search is essentially a binary search but in reverse, it starts at items[0:1],
then expands search horizon to [2:4], [5:8], [9:16], [17:32], etc until it finds a proper sub-array to match.
If it finds a plausible range, say [x:y], it'll then perform traditional binary search on items [x:y]

If one of the plausible range [l,h] exceeds stream length, then backtrack until a valid upperbound, h, is found
  and run binary search over [l,h]
'''


from binary_search import binary_search
from binary_search_unknown_size import binary_search_unknown_size

'''
Solution #1 outline:
	Start with a [0,1] window and double the search window [l,h] until a[h] > stream size or a[h] > key
	If a[h] > key, we now have a valid range [l, h] within which a classical binary search for key can be performed.
	If a[h] > stream size, backtrack back until a safe ub, l <= ub <= h, is found so a binary search [l, ub] can find the key.
	  if no such ub can be found, then the last window's 'h' was the upper bound, and the key isn't in the stream.
'''
def forward_binary_search_r1(lst, key):
	# Find a safe upper-bound for stream, lst,
	# within [l..h]
	# if l itself is out of the stream's range, return -1
	# Else, backtrack until stream's length is found
	def find_safe_ub(lst, l, h):
		last_safe = -1
		try:
			if lst[l]:
				last_safe = l
		except IndexError:
			return -1

		mid = (l+h)/2
		while l<=h:
			mid = (l+h)/2
			try:
				lst[mid]
				last_safe = mid
				l = mid+1
			except IndexError:
				h = mid-1
			
		return last_safe
				
	
	# Recursive helper to do forward binary search
	def forward_binary_search_helper(l=0, h=1):
		try:
			if key <= lst[h]: # key is in current window
				# Start a traditional binary search within the current search window, lst[l:h]
				return binary_search(lst, key, l, h)
			elif key > lst[h]: # key > current window, double window for next search
				return forward_binary_search_helper(h+1, h*2)
		except IndexError:
			# We have hit an IndexError at some index h==2^y
			# backtrack by half until we find an index that's actually valid
			# NOTE: It could very well be that [l, h] both are out of stream-length
			h = find_safe_ub(lst, l, h)

			# The entire [l,h] range exceeds stream length
			# and we haven't found the key yet => key doesn't exist in the stream
			if h == -1:
				return -1

			return binary_search(lst, key, l, h)

		# Couldn't find 'key'
		return -1

	# search key is less than the first item
	# in the stream.
	# since the stream is sorted in non-decreasing order,
	# it'll not be found later
	if key < lst[0]:
		return -1

	return forward_binary_search_helper()






'''
Solution #2 outline:
	Proceed as earlier,
	Start with a [0,1] window and double the search window [l,h] until a[h] > stream size or a[h] > key
	If a[h] > key, we now have a valid range [l, h] within which a classical binary search for key can be performed.
	If a[h] > stream size, Start a modified classical binary search that accounts for h being out of stream-size
	  find mid == (l+h)/2, if a[mid] is EOF, then reduce window to a[l.. mid-1], and repeat.
'''
def forward_binary_search_r2(lst, key):
	# Recursive helper to do forward binary search
	def forward_binary_search_helper(l=0, h=1):
		try:
			if key <= lst[h]: # key is in current window
				# Start a traditional binary search within the current search window, lst[l:h]
				return binary_search(lst, key, l, h)
			else: # key > lst[h]
				# key > current window, double window for next search
				return forward_binary_search_helper(h+1, h*2)
		except IndexError:
			# We have hit an IndexError at some index h==2^y
			# Start a modified version of a classic binary search that narrows window-size when 'mid' is over stream-length
			# NOTE: It could very well be that [l, h] both are out of stream-length
			return binary_search_unknown_size(lst, key, l, h)

		# Couldn't find 'key'
		return -1

	# search key is less than the first item
	# in the stream.
	# since the stream is sorted in non-decreasing order,
	# it'll not be found later
	if key < lst[0]:
		return -1

	return forward_binary_search_helper()




def test_forward_binary_search(searchfn):
	a = range(32)
	assert searchfn(a, 16) == 16
	assert searchfn(a, 31) == 31
	assert searchfn(a, 32) == -1
	
	a = range(0, 32, 2)
	assert searchfn(a, 11) == -1
	assert searchfn(a, 2) == 1
	assert searchfn(a, 8) == 4
	assert searchfn(a, 10) == 5
	assert searchfn(a, 9) == -1
	assert searchfn(a, 64) == -1
	assert searchfn(a, -1) == -1

	a = range(5, 64, 2)
	assert searchfn(a, 4) == -1
	assert searchfn(a, 64) == -1
	assert searchfn(a, 63) == 29
	assert searchfn(a, 5) == 0
	assert searchfn(a, 11) == 3
	assert searchfn(a, 19) == 7
	assert searchfn(a, 27) == 11
	assert searchfn(a, 37) == 16
	assert searchfn(a, 53) == 24
	assert searchfn(a, 55) == 25
	assert searchfn(a, 63) == 29


if __name__ == "__main__":
	test_forward_binary_search(forward_binary_search_r1)
	test_forward_binary_search(forward_binary_search_r2)

	# Binary search unknown size should be able to give us the same results
	# provided the range guesstimate is big enough
	assert forward_binary_search_r2(range(64), 63) == binary_search_unknown_size(range(64), 63, 0, 128) == 63



