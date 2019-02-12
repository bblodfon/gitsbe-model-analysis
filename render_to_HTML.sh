#!/bin/bash

# finds all R notenooks (.Rmd) in current dir
# and renders them to HTML

rmd_files=`ls | grep Rmd`

for file in ${rmd_files}; do
  echo "Rendering "$file"..."
  Rscript -e "library(rmarkdown); rmarkdown::render(\"./$file\",\"html_document\")"
done
