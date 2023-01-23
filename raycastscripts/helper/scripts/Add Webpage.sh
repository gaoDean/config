#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Add Webpage
# @raycast.mode compact
# @raycast.packageName Choose
#
# Optional parameters:
# @raycast.icon ➕

cd ..
osascript -e 'tell application "System Events" to keystroke "c" using {command down}'
pbpaste >> src/Webpage
sh gen.sh
