#!/bin/bash

echo > ospd4.txt

while read -r line
do
	if [[ `echo $line | wc -c` -eq 6 ]] 
	then
		echo $line >> ospd5.txt
	fi
done < ospd.txt

###
# VIM: Convert a file with word/line to a python list of strings
#
# 1. Quotes around each word
# :%s/^/"/gc
# :%s/$/"/gc
#
# 2. Concatenate all words separated by ','
# :%s/\n/,/gc
###



