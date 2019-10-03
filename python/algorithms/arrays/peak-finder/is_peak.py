# Check if a[i] is a peak in a 1-D array
def is_peak_1d(a, i, n):
	return (((i == 0) or a[i-1] <= a[i]) and ((i == n-1) or a[i] >= a[i+1]))
	

# Check if a[i] is a peak in a 2-D array
def is_peak_2d(a, i, n):
	pass
