#!/bin/bash

# run script
cd reddit_kotw
Rscript subreddit.R

# Nightly push to github

# Set some variables
depdir=/home/pi/reddit_kotw
jetzt=`date +%Y%m%d`

# update rep
git pull

# Now add any changes
git add .
# Now commit
git commit -m "$jetzt data push"
git push -u origin master

cd ..
