'''
Implement a LFU cache that supports the 'set()' and 'get()' operations.

  + get(key) - Get the value (will always be positive) of the key if the key exists in the cache, otherwise return -1.

  + put(key, value) - Set or insert the value if the key is not already present. 
    When the cache reached its capacity, it should invalidate the least frequently used item before inserting a new item. 

  + get(), set() are O(1)

Solution:
	Use a doubly-linked list of queues(based on DLLs again), each node in the top-level list contains a queue, 
	which in turn contains (key, value) pairs corresponding to the cache entries.
	The top-level DLL is ordered by frequency.

	e.g. (3 frequency queues, queue #1 represents 0 frequency,  queue #2 represents frequency 1, queue #2 represents frequency 3
	     NOTE: there can be gaps in frequencies, in the example, no entries exist with a used-frequency of 2,
		 Each frequency queue in turn contains entries with the corresponding used-frequency)
	[3:
	 0, [3: (a,1), (b,2), (c,3)]
	 1, [1: (d,4)]
	 3, [2: (e,5), (f,6)]
	]

	Use a hash-table that associates cache lookup keys to nodes in a list of (frequency) queues.
	Hash-table entries store [key] -> (top-level node containing frequency list of queues, 'fl_node', node inside frequency queue, 'node')

	+ get(key): 
	  find key in hash-table, (fl_node, node)
	  get the corresponding node in the top-level list, 'fl_node'
	  get the actual 'node' containing (key, value)  -> capture 'value' to be returned.
	  using key.
	  To update current (key, value) entry's frequency, 
		fl_node.frequency contains the current usage-frequency of the 'key' entry.
		fetch frequency queue 'fl' from fl_node.queue
		check to see if (frequency + 1) is the frequency of the next node (fl_node.next) -> 
		  if not, this is the first item to be added with that frequency => so insert a new node (frequency+1, empty queue)
		Remove (key, value) from current frequency queue and add it to next node's frequency queue with (frequency+1) 
		(NOTE: The relative order of the items within a frequency queue is based on their recency of access
		  so while invalidating an entry in a certain frequency queue, 
		  the one that was least-recently-used (LRU) will be considered for evicting from the cache.
		(ALSO NOTE: when we remove the last entry from a frequency queue, the queue (currently empty), and the node containing it 
		is removed from the top-level list)


	+ set(key, value):
	  New entries added to the cache start with 0 frequency, 
	  and as such can be the ripe candidate for culling the next time we need to remove an entry.
      if the cache has not reached capacity,
	    enqueue node(key,value) into the frequency queue at node 0.
		if the first node does not represent frequency 0 (cache.head.value.frequency is not 0)
		  then make a new node with freq 0, and an empty queue first
		map [key]  to (node containing freq 0, node)
	  Otherwise, 
	    If the cache has reached capacity -> time to flush the least frequently entry in the cache.
		the least frequently used 'key_' is the front node in the queue found in the front of the top-level list of queues.
		dequeue  node(key_) from node0.queue, remove said 'key_' from the hash-table as well.
		    if node0 becomes 'empty' (ie node0.queue is empty) -> remove node0 as well.
		Now, add new (key,value) into the cache as usual.
'''

import sys
sys.path.append("../../")
from data_structures.dll.dll import DLL
from data_structures.dll.queue import Queue


'''
A queue of cache entries with a usage frequency,'f'
Each FrequencyQueue object contains a frequency value, 
and a queue that stores entries that have the same usage-frequency
denoted by the queue's frequency value.

The entries within a frequency queue are ordered by LRU
so the front of the queue always contains the LRU entry for the
frequency corresponding to this queue.

The queue is based off a DLL to facilitate O(1) re-enqueue and 
remove (a specific node) in the queue.

FrequencyQueue is a derived class of Queue as it is essentially a Queue 
with an additional 'frequency' element tacked on
'''
class FrequencyQueue(Queue):
	def __init__(self, frequency=0):
		Queue.__init__(self)
		self.frequency = frequency


	def __str__(self):
		return "%d, %s" %(self.frequency, Queue.__str__(self))



'''
Top-level List of frequency queues
The list is implemented using a DLL, each item in the list
will be a frequency queue object

FrequencyQueuesList derives from DLL, carrying forward DLL's list implementation
and adding methods assuming items are specific to the FrequencyQueue type 
'''
class FrequencyQueuesList(DLL):
	def __init__(self):
		DLL.__init__(self)

		# Helper functions to fetch queue given a FrequencyQueuesList node
		self.queue = lambda fql_node: fql_node.value

	def __str__(self):
		lstr = "[%d:\n" %(self.size)
		trav = self.head
		while trav:
			lstr += "%s\n" %(trav.value)
			trav = trav.next
		return lstr + ']'


	def front(self):
		return self.head.value

	def back(self):
		return self.tail.value


	# Add qn entry to the FrequencyQueuesList
	# New entries are always added at frequency 0
	# therefore try to add it to the frequencyqueue (should be at the front of the queue)
	# with frequency 0 (if it exists),
	# if it doesn't, create a new FrequencyQueue with frequency 0, and add the entry to that frequency queue
	def add(self, key, value):
		fqueue = self.front()

		# The lowest usage frequency is not 0,
		# Create a new queue with frequency of 0
		# and prepend it to the list
		if fqueue.frequency != 0:
			fqueue = FrequencyQueue()
			self.push_front(fqueue)

		# The list has a queue with frequency 0
		# Enqueue item(key, value) to the back of the queue
		fqueue.enqueue((key, value))



	# Update frequency of entry at 'fq_node' with frequency 'f' to 'f+1'
	# by moving 'fq_node' to the next queue with frequency 'f+1'
	# If the next queue doesnt correspond to 'f+1'
	#   either update current queue's frequency to 'f+1' if this is the only entry in the current frequency queue
	#   or, create a new frequency queue with frequency 'f+1', and add it right next to the current 'fqlist_node'
	# Then move 'fq_node' to the next freq queue with frequency 'f+1'
	#
	# returns a new node in the top-level list of freq-queues where the entry now resides
	def promote(self, fqlist_node, fq_node):
		next_fqlist_node = fqlist_node.next
		current_frequency = fqlist_node.value.frequency

		if not next_fqlist_node or next_fqlist_node.value.frequency != (current_frequency + 1):
			# This queue has only 1 item and,
			# either it's currently the last queue in the list
			# or the next queue's frequency is not (f+1)
			# We can 
			# 1. create a queue with frequency with (f+1), move this 1 item from this queue f
			#    to the next queue (f+1), and delete the 'f' queue
			# 2. just update current queue's frequency to (f+1) and return
			#    -> this is functionally identical to 1. and more efficient
			if len(self.queue(fqlist_node)) == 1:
				self.queue(fqlist_node).frequency += 1
				return fqlist_node

			# Create a new frequency queue with (current freq + 1)
			fqueue = FrequencyQueue(current_frequency+1)
			self.add_next_to(fqlist_node, fqueue)
			# we just added next to fqlist_node, so fetch that next node reference
			# containing the (f+1) frequency queue
			next_fqlist_node = fqlist_node.next

		# At this point, we have a queue with frequency (f+1) where we can move our entry into
		self.queue(fqlist_node).removeNode(fq_node)
		self.queue(next_fqlist_node).enqueue(fq_node)

		# frequency queue where entry was moved from is now empty
		# remove it from the top-list
		if not self.queue(fqlist_node):
			self.removeNode(fqlist_node)

		return next_fqlist_node

		


	# invalidate LFU entry from the list,
	# and make room for a new entry
	def invalidate(self):
		pass





#TODO: Implement
'''
LFU Cache implementation
class LFUCache(object):
	def __init__(self, capacity):
		"""
		:type capacity: int
		"""
		self.flist_queues = DLL()
		self.table = {}
		self.capacity = capacity


	def get(self, key):
		"""
		:rtype: int
		"""

		try:
			# Lookup key in the table
			node = self.table[key]
		except KeyError:
			return -1

		# Remove the node from the queue and 
		# enqueue it back to the queue to mark it
		# as MRU
		self.queue.reEnqueueNode(node)

		return node.value[1]


	# TODO: Placeholder: implement later
	def set(self, key, value):
		"""
		:type key: int
		:type value: int
		:rtype: nothing
		"""

		# if key already exists, 
		# update with new value and return
		if (self.table.has_key(key)):
			fl_node, node = self.table[key]
			node.value = (key, value)

			# Move 'key' to the back of the queue
			# corresponding to its current frequency
			# so it becomes MRU in its queue
			fl_node.value.queue.reEnqueueNode(node)
			return

		if (self.queue.length() == self.capacity):
			# Dequeue least frequently used item from the queue
			# Remove it from the table as well
			fqueue = self.flist_queues.head.value[1]
			key_ = fqueue.dequeue()[0]
			self.table.pop(key_) # remove key from the table

		# Cache has enough capacity to accomodate the new entry
		item = (key, value)
		freq, fqueue = self.flist_queues.head.value
		if freq != 0:
			fqItem = FrequencyQueue(0)
			self.flist_queues.push_front(fqItem)
			fqueue = fqNode.Item

		# Add to the end of the queue
		# Add a reference of the DLL node (encapsulating the item)
		# into the hash table
		fqueue.enqueue(item)
		self.table[key] = self.flist_queues.head, fqueue.tail
        
'''
