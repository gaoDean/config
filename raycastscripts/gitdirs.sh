#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Update all gitdirs
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📄

while read file
do
	cd $(dirname file);
	git add -A;
	git commit -m 'update';
	git push;
done < ~/.config/shell/gitdirs
