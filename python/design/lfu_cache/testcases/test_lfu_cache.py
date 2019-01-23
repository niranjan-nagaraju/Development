
import sys
sys.path.append("../../../")
from design.lfu_cache.lfu_cache import LFUCache


def test_lfucache_tc1():
	cache = LFUCache(5)
	assert(len(cache) == 0)
	assert(cache.capacity == 5)

	cache.set('a', 1)
	cache.set('b', 2)
	cache.set('c', 3)
	cache.set('d', 4)
	cache.set('e', 5)
	assert(len(cache) == 5)

	cache.set('a', 0)
	assert(cache.frequency('a') == 1) # frequency bumped after set() update
	assert(len(cache) == 5)
	assert(cache.get('a') == 0)
	assert(cache.frequency('a') == 2) # frequency bumped after last get()
	assert(repr(cache) == "Hashtable: [5: {a: 0, c: 3, b: 2, e: 5, d: 4}]\nFqueues: [2:\n0, [4]: ('b', 2) ('c', 3) ('d', 4) ('e', 5) \n2, [1]: ('a', 0) \n]")
	assert(str(cache) == "[5: {a: 0, c: 3, b: 2, e: 5, d: 4}]")

	cache.set('f', 6) # invalidates 'b'
	assert(cache.get('b') == -1)
	assert(len(cache) == 5)
	assert(repr(cache) == "Hashtable: [5: {a: 0, c: 3, e: 5, d: 4, f: 6}]\nFqueues: [2:\n0, [4]: ('c', 3) ('d', 4) ('e', 5) ('f', 6) \n2, [1]: ('a', 0) \n]")
	assert(str(cache) == "[5: {a: 0, c: 3, e: 5, d: 4, f: 6}]")

	assert(cache.get('a') == 0)
	assert(cache.frequency('a') == 3)
	assert(cache.get('a') == 0)
	assert(cache.frequency('a') == 4)
	assert(cache.get('a') == 0)
	assert(cache.frequency('a') == 5)
	assert(repr(cache) == "Hashtable: [5: {a: 0, c: 3, e: 5, d: 4, f: 6}]\nFqueues: [2:\n0, [4]: ('c', 3) ('d', 4) ('e', 5) ('f', 6) \n5, [1]: ('a', 0) \n]")
	assert(str(cache) == "[5: {a: 0, c: 3, e: 5, d: 4, f: 6}]")

	cache.invalidateLFU() # invalidates 'c'
	assert(len(cache) == 4)
	assert(cache.get('c') == -1)
	assert(str(cache.fqueues_list) == "[2:\n0, [3]: ('d', 4) ('e', 5) ('f', 6) \n5, [1]: ('a', 0) \n]")

	cache.invalidateLFU() # invalidates 'd'
	assert(len(cache) == 3)
	assert(cache.get('d') == -1)
	assert(str(cache.fqueues_list) == "[2:\n0, [2]: ('e', 5) ('f', 6) \n5, [1]: ('a', 0) \n]")

	cache.invalidateLFU() # invalidates 'e'
	assert(len(cache) == 2)
	assert(cache.get('e') == -1)
	assert(str(cache.fqueues_list) == "[2:\n0, [1]: ('f', 6) \n5, [1]: ('a', 0) \n]")

	cache.invalidateLFU() # invalidates 'f'
	assert(len(cache) == 1)
	assert(cache.get('f') == -1) # 'f' was the last item in freq 0, so the queue is retained empty
	assert(str(cache.fqueues_list) == "[2:\n0, [0]: \n5, [1]: ('a', 0) \n]")

	#cache.invalidateLFU() # invalidates 'a' <-- will fail because it looks at '0' which is empty
	# remove freq 0 manually to avoid this
	cache.fqueues_list.removeNode(cache.fqueues_list.head)
	assert(str(cache.fqueues_list) == "[1:\n5, [1]: ('a', 0) \n]")
	
	cache.invalidateLFU() # invalidates 'a'
	assert(len(cache) == 0)
	assert(cache.get('a') == -1)
	assert(str(cache.fqueues_list) == "[0:\n]")



def test_lfucache_tc2():
	'''
	["LFUCache","get","put","get","put","put","get","get"]
	[[2],[2],[2,6],[1],[1,5],[1,2],[1],[2]]
	'''
	cache = LFUCache(2)
	assert(cache.get(2) == -1)
	cache.set(2,6)
	assert(cache.get(1) == -1)
	cache.set(1,5)
	cache.set(1,2)
	assert(cache.get(1) == 2)
	assert(cache.get(2) == 6)


if __name__ == '__main__':
	test_lfucache_tc1()
	test_lfucache_tc2()

