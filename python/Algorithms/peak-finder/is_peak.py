# Check if a[i] is a peak
def is_peak_1d(a, i, n):
	return (((i == 0) or a[i-1] <= a[i]) and ((i == n-1) or a[i] >= a[i+1]))
	
