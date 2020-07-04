#!/bin/bash

sample_size=50
sample_size_half=$((sample_size/2))

for ss in ss0 ss1 ss2
  do
  for sample_num in {1..20}
  do
      models_dir=models_${ss}_sample${sample_num}
      # create directory to store the models
      mkdir $models_dir
      # copy sampled models
      ls models_${ss} | sort -R | tail -$sample_size | while read file; do
        cp models_${ss}/$file $models_dir
      done

      # run Drabme
      java -cp ../target/synergy-1.2.0-jar-with-dependencies.jar eu.druglogics.drabme.Launcher --project=cascade_2.0_random_hsa_${ss}_sample${sample_num} --modelsDir=${models_dir} --drugs=drugpanel --perturbations=perturbations --config=config --modeloutputs=modeloutputs > /dev/null 2>&1
  done
done


# mix: ss0 and ss1
for sample_num in {1..20}
do
  # create directory to store the models
  models_dir=models_ss01_sample${sample_num}
  mkdir $models_dir
  # copy sampled models
  ls models_ss0 | sort -R | tail -$sample_size_half | while read file; do
    cp models_ss0/$file $models_dir
  done
  ls models_ss1 | sort -R | tail -$sample_size_half | while read file; do
    cp models_ss1/$file $models_dir
  done

  # run Drabme
  java -cp ../target/synergy-1.2.0-jar-with-dependencies.jar eu.druglogics.drabme.Launcher --project=cascade_2.0_random_hsa_ss01_sample${sample_num} --modelsDir=${models_dir} --drugs=drugpanel --perturbations=perturbations --config=config --modeloutputs=modeloutputs > /dev/null 2>&1
done

# mix: ss0 and ss2
for sample_num in {1..20}
do
  # create directory to store the models
  models_dir=models_ss02_sample${sample_num}
  mkdir $models_dir
  # copy sampled models
  ls models_ss0 | sort -R | tail -$sample_size_half | while read file; do
    cp models_ss0/$file $models_dir
  done
  ls models_ss2 | sort -R | tail -$sample_size_half | while read file; do
    cp models_ss2/$file $models_dir
  done

  # run Drabme
  java -cp ../target/synergy-1.2.0-jar-with-dependencies.jar eu.druglogics.drabme.Launcher --project=cascade_2.0_random_hsa_ss02_sample${sample_num} --modelsDir=${models_dir} --drugs=drugpanel --perturbations=perturbations --config=config --modeloutputs=modeloutputs > /dev/null 2>&1
done

# mix: ss1 and ss2
for sample_num in {1..20}
do
  # create directory to store the models
  models_dir=models_ss12_sample${sample_num}
  mkdir $models_dir
  # copy sampled models
  ls models_ss1 | sort -R | tail -$sample_size_half | while read file; do
    cp models_ss1/$file $models_dir
  done
  ls models_ss2 | sort -R | tail -$sample_size_half | while read file; do
    cp models_ss2/$file $models_dir
  done

  # run Drabme
  java -cp ../target/synergy-1.2.0-jar-with-dependencies.jar eu.druglogics.drabme.Launcher --project=cascade_2.0_random_hsa_ss12_sample${sample_num} --modelsDir=${models_dir} --drugs=drugpanel --perturbations=perturbations --config=config --modeloutputs=modeloutputs > /dev/null 2>&1
done

# mix all! (ss0, ss1 and ss2)
for sample_num in {1..20}
do
  # create directory to store the models
  models_dir=models_ss012_sample${sample_num}
  mkdir $models_dir
  # copy sampled models
  ls models_ss0 | sort -R | tail -$sample_size_half | while read file; do
    cp models_ss0/$file $models_dir
  done
  ls models_ss1 | sort -R | tail -$sample_size_half | while read file; do
    cp models_ss1/$file $models_dir
  done
  ls models_ss2 | sort -R | tail -$sample_size_half | while read file; do
    cp models_ss2/$file $models_dir
  done

  # run Drabme
  java -cp ../target/synergy-1.2.0-jar-with-dependencies.jar eu.druglogics.drabme.Launcher --project=cascade_2.0_random_hsa_ss012_sample${sample_num} --modelsDir=${models_dir} --drugs=drugpanel --perturbations=perturbations --config=config --modeloutputs=modeloutputs > /dev/null 2>&1
done


