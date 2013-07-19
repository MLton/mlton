#!/bin/sh

prefix="$2"
csplit -f ${prefix} -n 2 -s $1 '%\[specialsections\]%'
if [ $(grep '\[[a-z][a-z]*\]' ${prefix}00 | wc -l) -gt 1 ]; then
    csplit -f ${prefix}00 -n 2 -s ${prefix}00 '%\[specialsections\]%' '/\[[a-z][a-z]*\]/'
else
    cp ${prefix}00 ${prefix}0000
fi
(cat ${prefix}0000 | sed 's/^\(.*\)=[a-z]\{1,\}$/\1=/') > $2
rm ${prefix}00 ${prefix}00[0-9][0-9]
