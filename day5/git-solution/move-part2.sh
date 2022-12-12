#!/usr/bin/env bash
amount=$1
from=$2
to=$3
echo "move $1 from $2 to $3"
git checkout $to
git cherry-pick --allow-empty $from~$amount..$from
git branch -f $from $from~$amount
