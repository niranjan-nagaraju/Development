#!/usr/bin/env python
     
def process(l,numcol):
	col = 0
	l_great = 0
	for col in range(0,numcol):
		l_numrows = numcol-col
		pval = 0
		for l_rows in range(col,numcol):
			if col==0:
				l[l_rows][0] = pval+l[l_rows][0]
			else:
				if pval+l[l_rows][col]>l[l_rows-1][col-1]+l[l_rows][col]:
				    l[l_rows][col] = pval+l[l_rows][col]
				else:
					l[l_rows][col] = l[l_rows-1][col-1]+l[l_rows][col]
			pval = l[l_rows][col]
		if l[-1][col] > l_great:
		    l_great = l[-1][col]
	return l_great
     
l=[]
answer=[]
ncases=int(raw_input())
for nc in range(0,ncases):
	nrows=int(input())
	for nr in range(0,nrows):
		l.append(map(int,raw_input().split()))
	answer.append(process(l,nrows))
	l=[]
     
for i in answer:
		print i

