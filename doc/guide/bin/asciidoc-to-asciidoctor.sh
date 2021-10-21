#!/usr/bin/env bash

set -e
name=$(basename "$0")
dir=$(dirname "$0")

die () {
    echo "$1" >&2
    exit 1
}

usage () {
    die "usage: $name in.adoc out.adoc"
}

case "$#" in
    2)
        f=$1
        g=$2
        ;;
    *)
        usage
        ;;
esac

tmp=$(mktemp -p .)

title=$(cat $f | head -n 1)

pagere="(?<page>[a-zA-Z0-9]+)"
idre="(?<id>[a-zA-Z0-9_]+)"
filere="[-a-zA-Z0-9_.]+"
pathre="(?<path>(?<dir>(${filere}/)*)(?<base>${filere}))"
repore="(?<repo>[-a-zA-Z0-9_]+)"
commitre="(?<commit>[-a-zA-Z0-9_]+)"

(echo "= $title ="; tail -n +3 $f) | \
    perl -0777 -p \
        \
        -e 's/= ([^\n]*) =\n/= \1\n/g ;' \
        -e 's/\n== ([^\n]*) ==\n/\n== \1\n/g ;' \
        -e 's/\n=== ([^\n]*) ===\n/\n=== \1\n/g ;' \
        -e 's/\n==== ([^\n]*) ====\n/\n==== \1\n/g ;' \
        -e 's/\n===== ([^\n]*) =====\n/\n===== \1\n/g ;' \
        -e 's/\n====== ([^\n]*) ======\n/\n====== \1\n/g ;' \
        \
        -e 's/\n\|===(=*)\n/\n|===\n/g ;'\
        -e 's/\n----(-*)\n/\n----\n/g ;'\
        \
        -e "s/<:${pagere}:>/<<$+{page}#>>/gs ;" \
        -e "s/<:${pagere}#${idre}:>/<<$+{page}#$+{id},$+{id}>>/gs ;" \
        -e "s/<:#${idre}:>/<<#$+{id}>>/gs ;" \
        \
        -e "s/<:${pagere}:[ ]*(?<text>.*?)>/<<$+{page}#,$+{text}>>/gs ;" \
        -e "s/<:${pagere}#${idre}:[ ]*(?<text>.*?)>/<<$+{page}#$+{id},$+{text}>>/gs ;" \
        -e "s/<:#${idre}:[ ]*(?<text>.*?)>/<<#$+{id},$+{text}>>/gs ;" \
        \
        -e "s/\n(?<sect>=+) <\!Anchor\(${idre}\)>[ ]*/\n[#$+{id}]\n$+{sect} /gs ;" \
        -e "s/<\!Anchor\(${idre}\)>/[[$+{id}]]/gs ;" \
        \
        -e "s/<\!Cite\(${idre}\)>/<<References#$+{id},$+{id}>>/gs ;" \
        -e "s/<\!Cite\(${idre},[ ]*(?<text>.*?)\)>/<<References#$+{id},$+{text}>>/gs ;" \
        \
        -e "s;<\!Attachment\(${pagere},${pathre}\)>;link:$+{page}.attachments/$+{path}\[\`$+{base}\`\];gs ;" \
        -e "s;<\!Attachment\(${pagere},${pathre},[ ]*(?<text>.*?)\)>;link:$+{page}.attachments/$+{path}\[$+{text}\];gs ;" \
        \
        -e "s;<\!ViewSVNRev\((?<rev>[0-9]+)\)>;https://github.com/MLton/mlton/search?q=SVN+r$+{rev}&type=commits\[\`r$+{rev}\`\];gs ;" \
        \
        -e "s;<\!ViewGitProj\(${repore}\)>;https://github.com/MLton/$+{repo}\[\`$+{repo}\`\];gs ;" \
        -e "s;<\!ViewGitCommit\(${repore},${commitre}\)>;https://github.com/MLton/$+{repo}/commit/$+{commit}\[\`$+{commit}\`\];gs ;" \
        -e "s;<\!ViewGitDir\(${repore},${commitre},${pathre}\)>;https://github.com/MLton/$+{repo}/tree/$+{commit}/$+{path}\[\`$+{base}\`\];gs ;" \
        -e "s;<\!ViewGitFile\(${repore},${commitre},${pathre}\)>;https://github.com/MLton/$+{repo}/blob/$+{commit}/$+{path}\[\`$+{base}\`\];gs ;" \
        -e "s;<\!RawGitFile\(${repore},${commitre},${pathre}\)>;https://raw.github.com/MLton/$+{repo}/$+{commit}/$+{path}\[\`$+{base}\`\];gs ;" \
        \
        -e "s;sys::\[\./bin/InclGitFile\.py ${repore} ${commitre} ${pathre} (?<start>-?[0-9]+):(?<end>-?[0-9]+)\];include::https://raw.github.com/MLton/$+{repo}/$+{commit}/$+{path}\[indent=0,lines=$+{start}..$+{end}\];gs ;" \
        -e "s;sys::\[\./bin/InclGitFile\.py ${repore} ${commitre} ${pathre} (?<start>-?[0-9]+):\];include::https://raw.github.com/MLton/$+{repo}/$+{commit}/$+{path}\[indent=0,lines=$+{start}..-1\];gs ;" \
        -e "s;sys::\[\./bin/InclGitFile\.py ${repore} ${commitre} ${pathre}\];include::https://raw.github.com/MLton/$+{repo}/$+{commit}/$+{path}\[indent=0\];gs ;" \
        \
        -e 's/\+\+(.*?)&grave;(.*?)&grave;(.*?)\+\+/`\1\{backtick\}\2\{backtick\}\3`/g ;' \
        -e 's/\+\+(.*?)&grave;(.*?)\+\+/`\1\{backtick\}\2`/g ;' \
        -e 's/\+(.*?)&grave;(.*?)\+/`\1\{backtick\}\2`/g ;' \
        \
        -e "s/\`'s/\`&rsquo;s/g ;" \
        -e "s/>'s/>&rsquo;s/g ;" \
        -e 's/"`([^`"\n]*)`"/`\1`/g ;' \
        -e 's/`"\\U001F0000` and `\\U40000000"`/`"\\U001F0000"` and `"\\U40000000"`/g ;' \
        \
        -e 's;http://www.standardml.org;https://smlfamily.github.io;g ;' \
        \
        > $tmp

csplit -q -n 3 -f ${tmp}_xx $tmp '/^----/' '{*}'

for xx in ${tmp}_xx*; do
    n=${xx#${tmp}_xx};
    if [ $(echo "${n} % 2" | bc) == "0" ]; then
        cat $xx | \
            perl -0777 -p \
                 \
                 -e 's/(?<pre>[^\\`])\+\+(?<text>[^\n+]*?)\+\+/$+{pre}``$+{text}``/g ;' \
                 -e 's/(?<pre>[^\\`])\+(?<text>[^\n+]*?)\+/$+{pre}`$+{text}`/g ;' \
                 -e 's/`(?<pre>[^\n`]+[^\n`\\])->(?<post>[^\n`][^\n`]+)`/`$+{pre}\\->$+{post}`/g ;' \
                 -e 's/`(?<pre>[^\n`]+[^\n`\\])->(?<post>[^\n`][^\n`]+)`/`$+{pre}\\->$+{post}`/g ;' \
                 -e 's/`(?<pre>[^\n`]+[^\n`\\])->(?<post>[^\n`][^\n`]+)`/`$+{pre}\\->$+{post}`/g ;' \
                 -e 's/`(?<pre>[^\n`]+[^\n`\\])->(?<post>[^\n`][^\n`]+)`/`$+{pre}\\->$+{post}`/g ;' \
                 -e 's/`(?<pre>[^\n`]+[^\n`\\])->(?<post>[^\n`][^\n`]+)`/`$+{pre}\\->$+{post}`/g ;' \
                 -e 's/`(?<pre>[^\n`]+[^\n`\\])=>(?<post>[^\n`][^\n`]+)`/`$+{pre}\\=>$+{post}`/g ;' \
                 -e 's/`(?<pre>[^\n`]+[^\n`\\])=>(?<post>[^\n`][^\n`]+)`/`$+{pre}\\=>$+{post}`/g ;' \
                 -e 's/`(?<pre>[^\n`]+[^\n`\\])=>(?<post>[^\n`][^\n`]+)`/`$+{pre}\\=>$+{post}`/g ;' \
                 \
                 > ${tmp}_yy${n}
    else
        cp $xx ${tmp}_yy${n}
    fi
done


cat ${tmp}_yy* > $g

rm ${tmp}*
