# data-raw/

This folder creates data and scripts used to construct the data in the data/
folder. To create these data use 

    source("run_all.R")
    
This file will source other R scripts that read data and create rda files 
via `usethis::usedata()`. If you want to add more data, add the data to this
folder (preferably in a sub-folder) and create a script, e.g. `conductivity.R`.
