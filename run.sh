#!/usr/bin/env bash

# Compile
ghc --make ./src/words_tweeted.hs -o ./words_tweeted
ghc --make ./src/median_unique.hs -o ./median_unique

# I'll execute my programs, with the input directory tweet_input and output the files in the directory tweet_output
./words_tweeted ./tweet_input/tweets.txt ./tweet_output/ft1.txt
./median_unique ./tweet_input/tweets.txt ./tweet_output/ft2.txt
