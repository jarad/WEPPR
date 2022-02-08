# WEPPR

## Overview

WEPPR an R package to read and write WEPP input-output files and run WEPP  using the statistical software R. 

The [water erosion prediction project](https://www.fs.usda.gov/ccrc/tool/watershed-erosion-prediction-project-wepp) (WEPP) is a physically-based soil erosion computer model  supported by the United States Department of Agriculture (USDA).  

This package should allow those who are familiar with R a more convenient interface for running a large set of WEPP runs. 


## Installation

``` r
# install.packages("devtools")
devtools::install_github("jarad/WEPPR")
```

## Development

Development uses the [WEPP documentation extensively](https://www.ars.usda.gov/midwest-area/west-lafayette-in/national-soil-erosion-research/docs/wepp/wepp-model-documentation/). In particular, the  [user summary document](https://www.ars.usda.gov/ARSUserFiles/50201000/WEPP/usersum.pdf) provides a succinct summary.

## Usage

### Reading Input Files

- `read_cli()` : Climate Input File
- `read_slp()` : Slope Input File
- `read_man()` : Plant/Management Input File
- `read_run()` : WEPP run
- `read_sol()` : Soil Input File

### Reading Output files
- `read_env()` : Events (precipitation)
- `read_wb()` : Water balance
- `read_yld()` : Yield


### Code
``` r
library(WEPPR)

## Input files
read_cli("inst/extdata/092.63x040.90.cli")
read_slp("inst/extdata/071000090603_2.slp")
read_man("inst/extdata/071000090603_2.man")
read_run("inst/extdata/071000090603_2.run")
read_sol("inst/extdata/071000090603_2.sol")

## Output files
read_env("inst/extdata/071000090603_2.env")
read_wb("inst/extdata/071000090603_2.wb")
read_yld("inst/extdata/071000090603_2.yld")
```

## Acknowledgements

The following individuals have contributed to this project:

- Jarad Niemi
- Gulzina Kuttubekova
- Aditya Ranade
- Benedict Neo
