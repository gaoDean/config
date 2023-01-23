#!/bin/sh

find scripts -mindepth 1 -delete
cd src
for f in *; do
	echo "#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Get ${f}
# @raycast.mode silent
# @raycast.packageName Choose
#
# Optional parameters:
# @raycast.icon 🔎

cd ..
sh choose.sh src/${f}" > "../scripts/Get ${f}.sh";
	echo "#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Add ${f}
# @raycast.mode compact
# @raycast.packageName Choose
#
# Optional parameters:
# @raycast.icon ➕

cd ..
osascript -e 'tell application \"System Events\" to keystroke \"c\" using {command down}'
pbpaste >> src/${f}
sh gen.sh
" > "../scripts/Add ${f}.sh";
done
