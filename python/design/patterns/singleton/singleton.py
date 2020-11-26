'''
Implement a singleton

WARNING: *NOT* thread-safe.
'''

class Singleton(object):
	__instance__ = None

	@staticmethod
	def getInstance():
		if Singleton.__instance__ == None:
			Singleton()
		return Singleton.__instance__


	def __init__(self):
		if Singleton.__instance__ is not None:
			# Don't let the constructor to be called
			# when an instance already exists
			raise Exception("Singleton instance already exists!")
		else:
			self.x = 0
			self.y = 0
			Singleton.__instance__ = self
	   


if __name__ == '__main__':
	r = Singleton() # not strictly necessary we call the constructor first
	s = Singleton.getInstance()
	t = Singleton.getInstance()

	assert s == t == r
	assert id(s) == id(t) == id(r)

	assert s.x == t.x == r.x == 0
	assert s.y == t.y == r.y == 0

	s.x = 10
	s.y = 20

	assert s.x == t.x == r.x == 10
	assert s.y == t.y == r.y == 20

