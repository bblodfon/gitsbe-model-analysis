---
title: "Random model predictions (CASCADE 2.0) vs Number of Stable States"
author: "[John Zobolas](https://github.com/bblodfon)"
date: "Last updated: 04 July, 2020"
description: "An investigation"
url: 'https\://bblodfon.github.io/gitsbe-model-analysis/cascade/random-model-ss/index.html'
github-repo: "bblodfon/gitsbe-model-analysis"
link-citations: true
site: bookdown::bookdown_site
---

# Intro {-}

:::{.green-box}
Main question: is there a relation between random models stable state number and their performance as measured by the MCC score or AUC ROC?
:::

# Input {-}

The *random link-operator mutated* models were generated from the **CASCADE 2.0** topology, using the [abmlog software](https://github.com/druglogics/abmlog), version `1.6.0`.
Their prediction performance was assessed by the `Drabme` software module, via the `druglogics-synergy` Java module, version `1.2.0`.

I run the following command to get the random models from the `abmlog` repo root:

```r
java -cp target/abmlog-1.6.0-jar-with-dependencies.jar eu.druglogics.abmlog.RandomBooleanModelGenerator --file=test/cascade_2_0.sif --num=50000
```

I splitted the models to **3 groups**: those that have no stable state, 1 or 2 (there were no models with 3 or more stable states).
The percentages in each category were (use the [count_ss.sh](https://raw.githubusercontent.com/bblodfon/gitsbe-model-analysis/master/cascade/random-model-ss/data/count_ss.sh) script).

From the `ags_cascade_2.0` directory of the `druglogics-synergy` module I ran the [run.sh](https://raw.githubusercontent.com/bblodfon/gitsbe-model-analysis/master/cascade/random-model-ss/data/run.sh) script.
This script samples $50$ models ($20$ times in total) from each category and runs the Drabme with those.
So $50$ models $\times \text{ }20$ times with 0 stable states, $50$ models $\times \text{ }20$ times with 1 stable state, etc.
We also try all pair-wise combinations: {($25$ models with 0 stable states) + ($25$ models with 1 stable state)} $\times\text{ }20$ times, etc.
Lastly, we merge all of the different stable state models together in a pool of $25\times3=75$ models (again $20$ such samples).

# Libraries {-}


```r
library(emba)
library(xfun)
```

# Analysis {-}




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
  bibtex_0.4.2.2      bookdown_0.20       Ckmeans.1d.dp_4.3.2
  cli_2.0.2           clipr_0.7.0         compiler_3.6.3     
  crayon_1.3.4        digest_0.6.25       dplyr_1.0.0        
  ellipsis_0.3.1      emba_0.1.5          evaluate_0.14      
  fansi_0.4.1         gbRd_0.4-11         generics_0.0.2     
  glue_1.4.1          graphics_3.6.3      grDevices_3.6.3    
  grid_3.6.3          highr_0.8           hms_0.5.3          
  htmltools_0.5.0     htmlwidgets_1.5.1   igraph_1.2.5       
  jsonlite_1.7.0      knitr_1.29          lattice_0.20.41    
  lifecycle_0.2.0     magrittr_1.5        markdown_1.1       
  Matrix_1.2.18       methods_3.6.3       mime_0.9           
  pillar_1.4.4        pkgconfig_2.0.3     purrr_0.3.4        
  R6_2.4.1            Rcpp_1.0.4.6        Rdpack_1.0.0       
  readr_1.3.1         rje_1.10.16         rlang_0.4.6        
  rmarkdown_2.3       stats_3.6.3         stringi_1.4.6      
  stringr_1.4.0       tibble_3.0.1        tidyr_1.1.0        
  tidyselect_1.1.0    tinytex_0.24        tools_3.6.3        
  usefun_0.4.7        utf8_1.1.4          utils_3.6.3        
  vctrs_0.3.1         visNetwork_2.0.9    xfun_0.15          
  yaml_2.2.1         
```

# References {-}
