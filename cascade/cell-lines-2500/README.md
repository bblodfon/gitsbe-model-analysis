# How to run

- rbbt image: rbbt.SINTEF.img 
- image md5sum: 0a0167890384542abb5d4fae29ec5461 
- command: `rbbt workflow server SINTEF -p 1900 --export_all -W DrugLogics,Paradigm,CLSS,CombinationIndex --log 0 `
- doc: https://tinyurl.com/y57lglfw3 

# Job hash

http://localhost:1900/SINTEF/ROC_all/Default_20254c6fefd2f84d445bd515a34a7e69

# DOI

https://doi.org/10.5281/zenodo.3520699

# models dir 

In each of the directories: *A498, AGS, DU145, MDA-MB-468, SF295, SW620, UACC62, colo205, random*, a `models` dir should be extracted from the compressed file before running the analysis, using the follow command: `tar czvf models.tar.gz`. After that, the `data_preprocessing.Rmd` must be executed to create the `models_stable_state` and `models_link_operator` files in each cell-line directory.
