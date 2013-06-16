#!/bin/sh

prefix="$2"
csplit -f ${prefix} -n 2 -s $1 '%\[specialsections\]%'
split -a 2 -p '\[[a-z]+\]' ${prefix}00 ${prefix}00
(cat ${prefix}00aa | sed 's/^\(.*\)=[a-z]\{1,\}$/\1=/') > $2
rm ${prefix}00 ${prefix}00[a-z][a-z]
