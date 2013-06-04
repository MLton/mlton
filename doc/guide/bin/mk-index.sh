#!/bin/sh

echo "Index"
echo "====="
echo ""
for f in $(cd txt; ls *.txt | sort -f); do
    n="$(basename $f .txt)"
    echo "* <:$n:>"
done
