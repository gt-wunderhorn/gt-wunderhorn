#!/bin/bash

sed "/<code>/,/<\/code>/!d;/<\/code>/q" $1 \
  | sed 's/<code>/<code>\n/' \
  | sed '1d;$d;$d' \
  | sed 's/&#123;/{/g' \
  | sed 's/&#125;/}/g' \
  | sed "s/&#39;/\'/g" \
  | sed 's/&amp;/\&/g' \
  | sed 's/&lt;/</g' \
  | sed 's/&gt;/>/g'
