#!/bin/bash
set -e # exit on error

# Lee Pike
# Generate the directory listings
# and generate the RSS file
# BE CAREFUL NOT TO OVERWRITE THE MAIN index.html file!

cd pubs/
makedirindex> index.html
cd ../talks/
makedirindex> index.html
cd ..

perl genRSSpubs.pl
