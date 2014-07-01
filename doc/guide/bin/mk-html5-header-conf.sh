#!/bin/sh

prefix="$2"
csplit -f ${prefix} -n 2 -s $1 '%\[header\]%' '/<body.*>/+1' '/<div id="content">/+1'
(cat ${prefix}00; echo "<div id=\"banner\">"; echo "template::[banner-body]"; echo "</div>"; cat ${prefix}01) > $2
rm ${prefix}00 ${prefix}01 ${prefix}02
