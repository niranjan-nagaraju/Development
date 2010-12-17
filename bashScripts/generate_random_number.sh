#!/bin/bash

# Generate a random number between the specified range
# Source: http://linux.byexamples.com/archives/128/generating-random-numbers/

min=$1
max_min=$(( $2-$1+1 ))

# od -An (Specifies no address to be printed) -N2 (Extract 2 bytes) -i (Integer)
echo $(( $min+(`od -An -N2 -i /dev/random` )%($max_min) )) 
