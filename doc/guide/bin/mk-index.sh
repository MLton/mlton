#!/bin/sh

pages=$(echo $@ | sort -f)

echo "Index"
echo "====="
echo ""
for ul in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z; do
    echo "<:#${ul}${ul}${ul}:${ul}>"
done
for ul in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z; do
    echo ""
    ll=$(echo ${ul} | tr "[:upper:]" "[:lower:]")
    echo "== <!Anchor(${ul}${ul}${ul})>${ul} =="
    echo ""
    for page in ${pages}; do
        case ${page} in
            ${ul}*) ;;
            ${ll}*) ;;
            *) continue;;
        esac
        echo "* <:${page}:>"
    done
done
