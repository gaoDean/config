#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Synonym
# @raycast.mode compact
#
# Optional parameters:
# @raycast.icon 📕
# @raycast.argument1 { "type": "text", "placeholder": "Word" }

echo $(syn "$1")
