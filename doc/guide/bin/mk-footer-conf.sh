#!/bin/sh

prefix="$2"
csplit -f ${prefix} -n 2 -s $1 '%\[footer\]%' '/ifdef::icons\[\]/' '%endif::icons\[\]%+1' '/<\/html>/+1'
(cat ${prefix}00 | sed 's/XHTML/HTML5/'; cat ${prefix}01) > $2
rm ${prefix}00 ${prefix}01 ${prefix}02
