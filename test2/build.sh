#!/bin/bash
# Author: Matt Surawski (mjs9585@rit.edu)
#
MLTON='../build/bin/mlton'
OPTS='-diag-pass loopUnswitch'
OPTS="$OPTS -keep-pass loopUnswitch"
OPTS="$OPTS -keep dot"

SRCS='unswitch1.sml unswitchFail.sml'

case $1 in
clean)
	rm -f *.dot *.svg *.ssa *.diagnostic
	for FILE in $SRCS
	do
		rm $(basename $FILE .sml)
	done
	exit 0
	;;
esac

for FILE in $SRCS
do
	CMD="$MLTON $OPTS $FILE"
	echo $CMD
	$CMD
done
