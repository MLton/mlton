#!/bin/sh

prefix="$2"
csplit -f ${prefix} -n 2 -s $1 '%\[footer\]%' '/<div id="footer">/' '%<\/body>%' '/<\/html>/+1'
(cat ${prefix}00; echo "<div id=\"footer\">"; echo "template::[footer-body]"; echo "</div>"; cat ${prefix}01) > $2
rm ${prefix}00 ${prefix}01 ${prefix}02
