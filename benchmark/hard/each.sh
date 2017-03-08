#!/bin/bash

var=0
while read url; do
  ./scrape.sh $url > code/snippet_$var
  var=$((var+1))
done
