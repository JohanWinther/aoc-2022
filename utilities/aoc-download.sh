#!/usr/bin/env bash
session="$1"
year=2022
domain='https://adventofcode.com'
today=$(TZ='EST' date '+%-d')

for ((day=1; day <= today; day++))
do
    {
        mkdir "day$day";
        mkdir "day$day/site";
        wget -nc --header="Cookie: session=$session" -O "day$day/input.txt" "$domain/$year/day/$day/input" ;
    } || true
done
