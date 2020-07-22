#!/usr/bin/env python3

import sys
import urllib.request
import re

i = 1
repo = sys.argv[i]
i += 1
branch = sys.argv[i]
i += 1
src = sys.argv[i]
i += 1
url = 'https://raw.github.com/MLton/' + repo + '/' + branch + '/' + src
response = urllib.request.urlopen(url)
buff = response.readlines()

if len(sys.argv) > i:
    newbuff = []
    while len(sys.argv) > i:
        lines = sys.argv[i]
        match = re.compile(r"^\s*(?P<start>-?[0-9]+)?:(?P<end>-?[0-9]+)?\s*$").match(lines)
        start = match.group('start')
        if start:
            start = int(start)
        end = match.group('end')
        if end:
            end = int(end)
        newbuff.extend(buff[start:end])
        i += 1
    buff = newbuff

sys.stdout.buffer.writelines(buff)
sys.stdout.flush()
