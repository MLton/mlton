#!/bin/sh

csplit -s $1 '%\[footer\]%' '/<.html>/+1'
(cat xx00 | sed 's/XHTML/HTML5/') > $2
rm xx00 xx01
