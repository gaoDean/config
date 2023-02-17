#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Update All Gitdirs
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📄

while read dir
do
	git -C ~/"$dir" add -A
	git -C ~/"$dir" commit -m 'update'
	git -C ~/"$dir" push -q
done < ~/.config/shell/gitdirs
echo "All gitdirs updated"
