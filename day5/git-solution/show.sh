#!/usr/bin/env bash
for i in {1..9}
do
  git log -1 --pretty=format:%s $i 2>/dev/null
done
