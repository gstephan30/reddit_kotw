#!/bin/bash

# Nightly push to github
# Set some variables
depdir=/home/pi/reddit_kotw
jetzt=`date +%Y%m%d`

# update rep
cd reddit_kotw
git pull

# run scripts
## download reddit
Rscript subreddit.R
## download twitter
sudo Rscript subtwitter.R

# Now add any changes
git add .
# Now commit
git commit -m "$jetzt data push"
git push -u origin master

cd ..
