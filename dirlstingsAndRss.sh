#!/bin/bash
set -e # exit on error

# Lee Pike
# Generate the directory listings
# and generate the RSS file
# BE CAREFUL NOT TO OVERWRITE THE MAIN index.html file!

perl genRSSpubs.pl
