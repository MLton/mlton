#!/bin/bash
# Author: Matt Surawski (mjs9585@rit.edu)
#
if [ -z "$1" ]
then
	echo "Usage: $0 file.dot"
	exit 1
fi

dot -T svg $1 > "$1.svg"
