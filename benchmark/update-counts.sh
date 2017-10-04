#!/usr/bin/env bash

# set -e

die () {
        echo "$1" >&2
        exit 1
}

if $(which gtime) --version >/dev/null 2>&1; then
        time=$(which gtime)
elif $(which gnutime) --version >/dev/null 2>&1; then
        time=$(which gnutime)
elif $(which time) --version 2>&1 | grep -q GNU; then
        time=$(which time)
else
        die 'Can'\''t find GNU time'
fi


minTime="75.0"

bench="string-concat vector-concat"

cd tests
for prog in $bench; do
    case "$prog" in
        "fxp")
            continue ;;
    esac

    ( cat $prog.sml ; echo "val _ = Main.doit (valOf (Int.fromString (hd (CommandLine.arguments ()))))" ) > $prog.main.sml
    mlton -output $prog $prog.main.sml 1>/dev/null 2>/dev/null

    n=0
    t=0

    while [ "$(echo "$t < $minTime" | bc)" = "1" ]; do
        if [ $n -eq 0 ]; then
            n=1
            m=1
            k=1
        elif [ $n -lt $m ]; then
            n=$(($n+$k))
        else
            m=$((2*$m))
            if [ $m -lt 8 ]; then
                n=$m
            else
                k=$(( ($m-$n) / 2 ))
                n=$(($n+$k))
            fi
        fi
        $time -o $prog.time --format "%U + %S" ./$prog $n 1>/dev/null 2>/dev/null
        t=$(cat $prog.time | grep -v "Command exited" | bc)
        s=$(grep "Command exited" $prog.time)
        if [ ! -z "$s" ]; then
            s="; $s "
            break
        fi
    done

    echo "(\"$prog\", $n):: (* $t sec $s*)"

    rm $prog $prog.main.sml $prog.time

done
