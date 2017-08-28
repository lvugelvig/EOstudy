#!/bin/bash
for year in {03..17}
do
  for month in "Aug" "Sep" "Oct" "Nov" "Dec" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
  do
echo ${year}-${month}
  # Download the evoldir webpage
  curl "http://evol.mcmaster.ca/~brian/evoldir/Archive/Mnth_Review_${month}_${year}.pdf" > evoldir_${year}_${month}.pdf
done
done
curl "http://evol.mcmaster.ca/~brian/evoldir/Archive/Mnth_Review_Aug_17.pdf"

# Combine in a single pdf
pdftk *.pdf cat output evoldirArchive.pdf

# Convert the pdf to text
pdftotext evoldirArchive.pdf test.txt

# Extract the lines with Symposia
grep -i "Symposi" test.txt > test_Symposia.txt

# Extract the lines with SSE
grep "SSE" test_Symposia.txt > test_Symposia-SSE.txt grep "SSE" test_Symposia.txt > test_Symposia-SSE.txt

# Extract the lines with ESEB
grep "ESEB" test_Symposia.txt > test_Symposia-ESEB.txt
