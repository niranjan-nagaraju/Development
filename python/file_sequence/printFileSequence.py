#!/usr/bin/python

seq = 0
i = 0
seqCount = 0
nFiles = 100

while (seqCount < nFiles): 
	seq = seq + 1
	if (i % 10 == 0):
		tmp = i / 10
		seq = 0
		while ( tmp > 0):
			seq = seq * 10 + 9
			tmp = tmp - 1
		seq = seq * 10
	print seq, i
	i = i + 1
	seqCount = seqCount + 1
