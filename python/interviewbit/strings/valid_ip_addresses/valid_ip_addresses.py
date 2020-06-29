#encoding: utf-8
'''
https://www.interviewbit.com/problems/valid-ip-addresses/

Valid Ip Addresses

Given a string containing only digits, restore it by returning all possible valid IP address combinations.
A valid IP address must be in the form of A.B.C.D, where A,B,C and D are numbers from 0-255. The numbers cannot be 0 prefixed unless they are 0.

Example:
Given “25525511135”,
return [“255.255.11.135”, “255.255.111.35”]. (Make sure the returned strings are sorted in order)
'''

'''
Solution Outline:
	1. IP addresses have 4 parts separated by 3 '.'s
	2. Fix slot for each '.', and check if the resulting partition around the dots make a valid IP.
	   2.1 Add all such valid IPs and return
'''

class Solution:
	def enumerate_valid_ip_addresses(self, A):
		# check if a list of 4 elements [a,b,c,d]
		# can be a valid IP address when transformed into a.b.c.d
		# a,b,c,d should be in [0-255]
		# cannot have preceeding 0s unless it has a single 0
		def is_ip_valid(ip):
			for x in ip:
				if not x or (x[0] == '0' and len(x) > 1):
					return False
				if int(x) > 255:
					return False
			return True

		if len(A) > 12 or len(A) < 4:
			return []

		valid_ip_addresses = []
		# First '.' can be placed at slots 1, 2 and 3
		# '1.x.x.x', '10.x.x.x', '100.x.x.x'
		for i in xrange(1, 4):
			for j in xrange(i+1, i+4):
				for k in xrange(j+1, j+4):
					candidate_ip = [A[:i],A[i:j], A[j:k], A[k:]]
					if is_ip_valid(candidate_ip):
						valid_ip_addresses.append('.'.join(candidate_ip))
	
		return valid_ip_addresses


if __name__ == '__main__':
	s = Solution()
	assert s.enumerate_valid_ip_addresses("1234") == ['1.2.3.4']
	assert s.enumerate_valid_ip_addresses("25525511135") == ['255.255.11.135', '255.255.111.35']
	assert s.enumerate_valid_ip_addresses('0100100') == ['0.10.0.100', '0.100.10.0'] 
	assert s.enumerate_valid_ip_addresses('1921682040') == ['192.168.20.40', '192.168.204.0']

