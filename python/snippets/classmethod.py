class Test:
	def __init__(self, r, c, m):
		self.r = r
		self.c = c
		self.m = m

	@classmethod
	def fromValues(cls, r, c, m):
		return cls(r,c,m)

'''
 Trial run
 [14:48:15 Niranjan snippets]$ python -i classmethod.py 
 >>> 
 >>> a = Test(1,2,3)
 >>> a
 <__main__.Test instance at 0x101f4eb90>
 >>> b = Test.fromValues(4,5,6)
 >>> b
 <__main__.Test instance at 0x101f4eab8>
 >>> c = a.fromValues(4,5,6)
 >>> c
 <__main__.Test instance at 0x101f4ebd8>
'''
