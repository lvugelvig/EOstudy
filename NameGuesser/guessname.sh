#!/bin/bash

# Argument 1: first name

# Check arguments
if [[ $# -eq 0 ]] ; then
    echo 'No argument supplied! You need to enter a first name'
    exit 1
fi

FNAME="$1"

#curl -s --compress "http://www.gpeters.com/names/baby-names.php?name=${FNAME}" | grep "Based on popular usage" > guessname.html
# Extract the gender
GENDER=$(sed -e 's/!<\/.*//' -e "s/<b>It's a //" guessname.html)

# Extract the ratio
RATIO=$(sed -e 's/^.* it is <b>//' -e 's/ times .*//' guessname.html)

# Output: gender and ratio
echo "${GENDER}", "${RATIO}"
