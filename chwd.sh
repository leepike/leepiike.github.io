#!/bin/bash

# Lee Pike
# Reads each non-hidden file in the current directory, finding and replacing
# some string in each file.

# only do this for files in the current dir ending w/ a .html extension.
for FILE in *.html
do
    echo $FILE
	sed -e s/lepike@/leepike@/g $FILE | 
	sed -e s/'galois.com'/'indiana.edu'/g > ${FILE}tmp
	mv ${FILE}tmp $FILE
  cat $FILE
done
