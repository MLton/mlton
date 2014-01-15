#!/bin/sh

dir=$(dirname "$0")
root=$(cd "$dir/../../.." && pwd)

pages=$(echo $@ | sort -f)

for page in ${pages}; do
    for vgd in $(cat src/${page}.adoc | sed -E -n 's|.*(<!ViewGitDir\(mlton,[-a-zA-Z0-9_]+,(([-a-zA-Z0-9_.]+/)*[-a-zA-Z0-9_.]+)\)>).*|\1|p'); do
        rev=$(echo "$vgd" | sed -E -n 's|<!ViewGitDir\(mlton,([-a-zA-Z0-9_]+),(([-a-zA-Z0-9_.]+/)*[-a-zA-Z0-9_.]+)\)>|\1|p')
        path=$(echo "$vgd" | sed -E -n 's|<!ViewGitDir\(mlton,([-a-zA-Z0-9_]+),(([-a-zA-Z0-9_.]+/)*[-a-zA-Z0-9_.]+)\)>|\2|p')
        if (cd ${root}; git ls-tree ${rev} ${path} | grep -E -q '^[0-9]+[[:space:]]tree[[:space:]][0-9a-f]+[[:space:]]'${path}'$'); then
            :
        else
            echo "*** ${page}: ${vgd}"
        fi
    done
    for vgf in $(cat src/${page}.adoc | sed -E -n 's|.*(<!ViewGitFile\(mlton,[-a-zA-Z0-9_]+,(([-a-zA-Z0-9_.]+/)*[-a-zA-Z0-9_.]+)\)>).*|\1|p'); do
        rev=$(echo "$vgf" | sed -E -n 's|<!ViewGitFile\(mlton,([-a-zA-Z0-9_]+),(([-a-zA-Z0-9_.]+/)*[-a-zA-Z0-9_.]+)\)>|\1|p')
        path=$(echo "$vgf" | sed -E -n 's|<!ViewGitFile\(mlton,([-a-zA-Z0-9_]+),(([-a-zA-Z0-9_.]+/)*[-a-zA-Z0-9_.]+)\)>|\2|p')
        if (cd ${root}; git ls-tree ${rev} ${path} | grep -E -q '^[0-9]+[[:space:]]blob[[:space:]][0-9a-f]+[[:space:]]'${path}'$'); then
            :
        else
            echo "*** ${page}: ${vgf}"
        fi
    done
    for rgf in $(cat src/${page}.adoc | sed -E -n 's|.*(<!RawGitFile\(mlton,[-a-zA-Z0-9_]+,(([-a-zA-Z0-9_.]+/)*[-a-zA-Z0-9_.]+)\)>).*|\1|p'); do
        rev=$(echo "$rgf" | sed -E -n 's|<!RawGitFile\(mlton,([-a-zA-Z0-9_]+),(([-a-zA-Z0-9_.]+/)*[-a-zA-Z0-9_.]+)\)>|\1|p')
        path=$(echo "$rgf" | sed -E -n 's|<!RawGitFile\(mlton,([-a-zA-Z0-9_]+),(([-a-zA-Z0-9_.]+/)*[-a-zA-Z0-9_.]+)\)>|\2|p')
        if (cd ${root}; git ls-tree ${rev} ${path} | grep -E -q '^[0-9]+[[:space:]]blob[[:space:]][0-9a-f]+[[:space:]]'${path}'$'); then
            :
        else
            echo "*** ${page}: ${rgf}"
        fi
    done
done
