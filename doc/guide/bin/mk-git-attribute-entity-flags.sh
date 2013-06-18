#!/bin/sh

if [ -n "$(which git)" ]; then
    git log -n 1 --format="-a git-commit-hash='%H' -a git-author-date='%ad' -a git-author-email='%ae' -a git-author-name='%an'" "$1" 2>/dev/null
fi
