'''
https://www.hackerrank.com/challenges/time-conversion

Sample Input
07:05:45PM

Sample Output
19:05:45

'''

def convert_time(short_time):
	hours = short_time[:2]
	pm_or_am = short_time[-2:]

	if pm_or_am == "PM":
		if (hours != "12"):
			hours = str(int(hours) + 12)
	else:
		if (hours == "12"):
			hours = "00"

	return hours + short_time[2:-2] # remove hh, AM/PM from hh:mm:ssAM/PM

print convert_time(raw_input().strip())
	

