#!/usr/bin/env bash

# Init
git checkout -b temp
git branch -D stacks 1 2 3 4 5 6 7 8 9
git checkout --orphan stacks
git branch -D temp
git commit --allow-empty -m "stacks"

git checkout -b 1 stacks
git commit --allow-empty -m "L"
git commit --allow-empty -m "N"
git commit --allow-empty -m "W"
git commit --allow-empty -m "T"
git commit --allow-empty -m "D"

git checkout -b 2 stacks
git commit --allow-empty -m "H"
git commit --allow-empty -m "P"
git commit --allow-empty -m "C"

git checkout -b 3 stacks
git commit --allow-empty -m "W"
git commit --allow-empty -m "P"
git commit --allow-empty -m "H"
git commit --allow-empty -m "N"
git commit --allow-empty -m "D"
git commit --allow-empty -m "G"
git commit --allow-empty -m "M"
git commit --allow-empty -m "J"

git checkout -b 4 stacks
git commit --allow-empty -m "C"
git commit --allow-empty -m "W"
git commit --allow-empty -m "S"
git commit --allow-empty -m "N"
git commit --allow-empty -m "T"
git commit --allow-empty -m "Q"
git commit --allow-empty -m "L"

git checkout -b 5 stacks
git commit --allow-empty -m "P"
git commit --allow-empty -m "H"
git commit --allow-empty -m "C"
git commit --allow-empty -m "N"

git checkout -b 6 stacks
git commit --allow-empty -m "T"
git commit --allow-empty -m "H"
git commit --allow-empty -m "N"
git commit --allow-empty -m "D"
git commit --allow-empty -m "M"
git commit --allow-empty -m "W"
git commit --allow-empty -m "Q"
git commit --allow-empty -m "B"

git checkout -b 7 stacks
git commit --allow-empty -m "M"
git commit --allow-empty -m "B"
git commit --allow-empty -m "R"
git commit --allow-empty -m "J"
git commit --allow-empty -m "G"
git commit --allow-empty -m "S"
git commit --allow-empty -m "L"

git checkout -b 8 stacks
git commit --allow-empty -m "Z"
git commit --allow-empty -m "N"
git commit --allow-empty -m "W"
git commit --allow-empty -m "G"
git commit --allow-empty -m "V"
git commit --allow-empty -m "B"
git commit --allow-empty -m "R"
git commit --allow-empty -m "T"

git checkout -b 9 stacks
git commit --allow-empty -m "W"
git commit --allow-empty -m "G"
git commit --allow-empty -m "D"
git commit --allow-empty -m "N"
git commit --allow-empty -m "P"
git commit --allow-empty -m "L"
