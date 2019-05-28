#!/bin/bash

# finds all R notenooks (.Rmd) in current dir
# and renders them to HTML

if [ "$#" != 1 ]; then
  echo "Give one parameter: 'all' or 'some'!"
  exit 1
else
  arg=$1
fi

if [ $arg == "all" ]; then
  rmd_files=`ls | grep Rmd`
elif [ $arg == "some" ]; then
  rmd_files=`ls | grep Rmd | grep -v biomarker | grep -v data_prepro | grep -v consensus | grep -v fitness | grep -v prolif`
else
  echo "Write: 'all' or 'some'!"
  exit 1
fi

echo $rmd_files

for file in ${rmd_files}; do
  echo "Rendering "$file"..."
  Rscript -e "library(rmarkdown); rmarkdown::render(\"./$file\",\"html_document\")"
done
