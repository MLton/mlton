#!/bin/sh

if [ -n "$(which git)" ]; then
    git log -n 1 --format="--attribute=git-commit-hash='%H' --attribute=git-author-date='%ad' --attribute=git-author-email='%ae' --attribute=git-author-name='%an'" "$1" 2>/dev/null
fi
