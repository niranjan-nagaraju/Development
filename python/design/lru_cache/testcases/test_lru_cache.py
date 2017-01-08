from design.lru_cache.lru_cache import *

cache = LRUCache(2)
cache.set(1,2)
cache.set(2,3)
assert(cache.get(1) == 2)
cache.set(3,4) # Invalidates (2,3)
assert(cache.get(2) == -1)
assert(cache.get(3) == 4)
cache.set(4,5) # Invalidates (1,2)
assert(cache.get(1) == -1)
assert(cache.get(4) == 5)
assert(cache.get(3) == 4)


cache2 = LRUCache(2)
cache2.set(2,1)
cache2.set(2,2)
assert(cache2.get(2) == 2)
