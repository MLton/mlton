#!/bin/sh

csplit -s $1 '%\[header\]%' '/<body.*>/+1' '/<div id="content">/+1'
(cat xx00 ; echo "<div id=\"banner\">"; echo "template::[banner-body]"; echo "</div>"; cat xx01) > $2
rm xx00 xx01 xx02
