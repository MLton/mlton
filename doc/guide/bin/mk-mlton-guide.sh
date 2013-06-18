#!/bin/sh

pages=$(echo $@ | sort -f)

xform () {
    page=$1
    echo ":mlton-guide-page: ${page}"
    echo "[[${page}]]"
    # echo "include::${page}.txt[]"
    cat txt/${page}.txt |
    # sed -e '1 { s/\(.*\)/[['${page}']]\1/; h; N; g; s/./=/g; x; G; }' |
    sed -e '/^:toc:$/ { d; }' |
    cat
}

echo "MLton Guide ({mlton-version})"
echo "============================="
echo ":toc:"
echo ":mlton-guide-page!:"
echo ""
echo "[abstract]"
echo "--"
echo "This is the guide for MLton, an open-source, whole-program, optimizing Standard ML compiler."
echo ""
echo "This guide was generated automatically from the MLton website, available online at http://mlton.org. It is up to date for MLton {mlton-version}."
echo "--"
echo ""
echo ""
echo ":leveloffset: 1"
for page in Home; do
    echo ""
    xform ${page}
    echo ""
    echo "<<<"
done
for page in ${pages}; do
    case ${page} in
        Home) continue;;
    esac
    echo ""
    xform ${page}
    echo ""
    echo "<<<"
done
