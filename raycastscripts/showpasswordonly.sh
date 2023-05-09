#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Passwords Only
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 🔑

killall -q choose
password=$(~/repos/bin/pa/pa list |
               awk -F':' '
BEGIN { max_len = 0 }
{
  left=$1; right=$2;
  if (length(left) > max_len) {
    max_len = length(left);
  }
  lines[NR] = left ":" right;
}
END {
  for (i = 1; i <= NR; i++) {
    split(lines[i], parts, ":");
    left = parts[1]; right = parts[2];
    printf "%-" max_len+1 "s%s\n", left, right;
  }
}' | choose | gsed -E 's/^(\S+)\s+(\S+)$/\1:\2/')


if [ -n "$password" ]; then
	~/repos/bin/pa/pa show "$password" | pbcopy
	(~/.local/bin/pbclear 30 &) &> /dev/null
fi
