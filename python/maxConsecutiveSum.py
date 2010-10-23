
def maxConsecutiveSum (list):
	currsum = 0
	maxsum = 0
	for i in list:
		# if current element increases our current max sum, include it into the list
		if (currsum + i) > currsum:
			currsum = currsum + i
			if maxsum < currsum: 
				maxsum = currsum # we found a new set that beats the prev maximum
		else:
			currsum = 0  # start over to find new maximum sum
		print "Processing element: ", i, "Maxsum: ", maxsum, "Currsum: ", currsum
	return maxsum
	
num_input = int(input())
inputList = []
while (num_input > 0):
	number = int(input())
	inputList.append(number)
	num_input = num_input - 1

for i in inputList:
	print i

maxSum = maxConsecutiveSum (inputList)
print maxSum

