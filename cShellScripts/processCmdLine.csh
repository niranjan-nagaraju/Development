#!/bin/csh

while ($#argv > 0)
	set var = $argv[1]
	echo $var
	shift
end
