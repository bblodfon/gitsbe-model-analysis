#!/bin/bash

echo Running with the AGS-Gold training data:

start=`date +%s`
java -cp ~/repos/druglogics-synergy/target/synergy-1.0.3-jar-with-dependencies.jar eu.druglogics.synergy.Launcher --inputDir=sim_files --project=training_ags_gold > /dev/null
runtime=$(($(date +%s)-$start))
echo Execution Time: "$(($runtime / 60)) minutes and $(($runtime % 60)) seconds"

# Flipped training data files

training_files=`ls training-data-files` 
files_num=`ls training-data-files | wc -l`
count=0

for file in ${training_files}; do
  count=$((count + 1))
  echo Using flipped training file No. $count/$files_num: $file
  cat training-data-files/$file > sim_files/training
  
  start=`date +%s`
  java -cp ~/repos/druglogics-synergy/target/synergy-1.0.3-jar-with-dependencies.jar eu.druglogics.synergy.Launcher --inputDir=sim_files --project=$file > /dev/null

  runtime=$(($(date +%s)-$start))
  echo Execution Time: "$(($runtime / 60)) minutes and $(($runtime % 60)) seconds"
done
