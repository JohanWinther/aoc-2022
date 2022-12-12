#!/usr/bin/env bash
amount=$1
from=$2
to=$3
echo "move $1 from $2 to $3"
git checkout $to
for ((i=1;i<=amount;i++)); do
    git cherry-pick --allow-empty $from
    git branch -f $from $from~1
done
