#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Caffeinate
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon ☕

caffeinate -s -t $(bc <<< "60 * 60 * 2")
echo "Caffeinated for 2 hours"
