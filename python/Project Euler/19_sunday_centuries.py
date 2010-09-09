#!/usr/bin/python

normal_year = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leap_year =   [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

def is_leap_year (year):
	if ((year % 4 == 0) and (year % 100 != 0)) or (year % 400 == 0):
		return True
	return False

def get_num_sundays ():
	start_year = 1901
	end_year = 2000
	days_since_start = sum(normal_year) + 1 #Start date is 1901, 1900 Jan 1 was a monday, So count from December 31, 1899

	num_sundays = 0	
	for this_year in range(start_year, (end_year+1)):
		for month in range(0, 12):
			if (days_since_start % 7 == 0):
				num_sundays += 1
				# print [this_year, (month+1)]
			if (is_leap_year(this_year)):
				days_since_start += leap_year[month]
			else:
				days_since_start += normal_year[month]

	return num_sundays

print get_num_sundays ()
