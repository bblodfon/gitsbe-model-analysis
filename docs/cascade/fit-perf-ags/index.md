---
title: "Fitness vs Performance Analysis (AGS paper I)"
author: "[John Zobolas](https://github.com/bblodfon)"
date: "Last updated: 05 June, 2020"
description: "An investigation analysis"
url: 'https\://bblodfon.github.io/gitsbe-model-analysis/cascade/fit-perf-ags/index.html'
github-repo: "bblodfon/gitsbe-model-analysis"
link-citations: true
site: bookdown::bookdown_site
---

# Intro {-}

:::{.green-box}
Main question: is there a correlation between models fitness to the steady state and their performance as measured by the MCC score?
:::

All boolean model simulations were done using the `druglogics-synergy` Java module, version `1.2.0`: `git checkout v1.2.0`.

This analysis was done after I concluded the ROC and PR analysis for the AGS paper I (see [here](https://bblodfon.github.io/ags-paper-1/index.html)).
Since the results of this analysis are negative, I put them here as an investigation and a reference guide for future endeavors!

# Input {-}

Load libraries:

```r
library(xfun)
library(emba)
library(usefun)
library(dplyr)
library(tibble)
library(stringr)
```

Load the AGS steady state:

```r
# get the AGS steady state
steady_state_file = "data/steadystate"
lines = readLines(steady_state_file)
ss_data = unlist(strsplit(x = lines[8], split = "\t"))
ss_mat = stringr::str_split(string = ss_data, pattern = ":", simplify = TRUE)
colnames(ss_mat) = c("nodes", "states")
ss_tbl = ss_mat %>% as_tibble() %>% mutate_at(vars(states), as.integer)

steady_state = ss_tbl %>% pull(states)
names(steady_state) = ss_tbl %>% pull(nodes)

usefun::pretty_print_vector_names_and_values(vec = steady_state)
```

> CASP3: 0, CASP8: 0, CASP9: 0, FOXO_f: 0, RSK_f: 1, CCND1: 1, MYC: 1, RAC_f: 1, JNK_f: 0, MAPK14: 0, AKT_f: 1, MMP_f: 1, PTEN: 0, ERK_f: 1, KRAS: 1, PIK3CA: 1, S6K_f: 1, GSK3_f: 0, TP53: 0, BAX: 0, BCL2: 1, CTNNB1: 1, TCF7_f: 1, NFKB_f: 1

Load observed synergies:

```r
# get observed synergies for Cascade 2.0
observed_synergies_file = "data/observed_synergies_cascade_2.0"
observed_synergies = emba::get_observed_synergies(observed_synergies_file)

pretty_print_vector_values(vec = observed_synergies, vector.values.str = "observed synergies")
```

> 6 observed synergies: AK-BI, 5Z-PI, PD-PI, BI-D1, PI-D1, PI-G2

# Flip training data analysis {-}

The idea here is to generate many training data files from the steady state, where some of the nodes will have their states *flipped* to the opposite state ($0$ to $1$ and vice versa).
That way, we can train models to different steady states, ranging from ones that differ to just a few nodes states up to a steady state that is the complete *reversed* version of the one used in the simulations.

Using the [gen_training_data.R](TODO add link) script, we first chose a few number of flips ($11$ flips) ranging from $1$ to $24$ (all nodes) in the steady state.
Then, for each such *flipping-nodes* value, we generated $20$ new steady states with a randomly chosen set of nodes whose value is going to flip.
Thus, in total, $205$ training data files were produced ($205 = 9 \times 20 + 24 + 1$, where from the $11$ number of flips, the one flip happens for every node ($24$ different steady states) and flipping all the nodes generates $1$ completely reversed steady state). 

Running the script [run_druglogics_synergy_training.sh](TODO add link) from the `druglogics-synergy` repository root (version `1.2.0`: `git checkout v1.2.0`), we get the simulation results for each of these training data files.
Note that in the CASCADE 2.0 configuration file (`config`) we changed the number of simulations to ($15$) for each training data file, the attractor tool used was `biolqm_stable_states` and the `synergy_method: hsa`.

The generated training data files and the results from the simulations are stored here [`training-data-files` DOI ZENODO].

To load the data, download the file of interest (`fit-vs-performance-results.tar.gz`) and extract it to a directory of your choice with the following commands: `mkdir fit-vs-performance-results` and `tar -C fit-vs-performance-results/ -xzvf fit-vs-performance-results.tar.gz`.
Then use the next R code to create the `res` data.frame (I have already saved the result):





# Reverse SS vs SS method {-}

- 5000 simulations => `15000` models
- `HSA` synergy assessement (Drabme)
- res_link: `biolqm_stable_states`
- res_topo: `bnet_reduction_reduced`
- res_link_and_topo: `bnet_reduction_reduced`

Fit to steady state, to reverse steady state and proliferation profile








# R session info {-}


```r
xfun::session_info()
```

```
R version 3.6.3 (2020-02-29)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 18.04.4 LTS

Locale:
  LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
  LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
  LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
  LC_PAPER=en_US.UTF-8       LC_NAME=C                 
  LC_ADDRESS=C               LC_TELEPHONE=C            
  LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

Package version:
  assertthat_0.2.1    base64enc_0.1.3     BH_1.72.0.3        
  bibtex_0.4.2.2      bookdown_0.19       Ckmeans.1d.dp_4.3.2
  cli_2.0.2           clipr_0.7.0         compiler_3.6.3     
  crayon_1.3.4        digest_0.6.25       dplyr_0.8.5        
  ellipsis_0.3.1      emba_0.1.5          evaluate_0.14      
  fansi_0.4.1         gbRd_0.4-11         glue_1.4.1         
  graphics_3.6.3      grDevices_3.6.3     grid_3.6.3         
  highr_0.8           hms_0.5.3           htmltools_0.4.0    
  htmlwidgets_1.5.1   igraph_1.2.5        jsonlite_1.6.1     
  knitr_1.28          lattice_0.20.41     lifecycle_0.2.0    
  magrittr_1.5        markdown_1.1        Matrix_1.2.18      
  methods_3.6.3       mime_0.9            pillar_1.4.4       
  pkgconfig_2.0.3     plogr_0.2.0         purrr_0.3.4        
  R6_2.4.1            Rcpp_1.0.4.6        Rdpack_0.11-1      
  readr_1.3.1         rje_1.10.15         rlang_0.4.6        
  rmarkdown_2.1       stats_3.6.3         stringi_1.4.6      
  stringr_1.4.0       tibble_3.0.1        tidyr_1.1.0        
  tidyselect_1.1.0    tinytex_0.23        tools_3.6.3        
  usefun_0.4.7        utf8_1.1.4          utils_3.6.3        
  vctrs_0.3.0         visNetwork_2.0.9    xfun_0.14          
  yaml_2.2.1         
```
