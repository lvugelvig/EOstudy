#!/bin/bash
for year in {03..17}
do
  for month in "Aug" "Sep" "Oct" "Nov" "Dec" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
  do
    git filter-branch -f --index-filter 'git rm -r --cached --ignore-unmatch SympInTime/Evoldir/evoldir_${year}_${month}.pdf' HEAD
done
done
