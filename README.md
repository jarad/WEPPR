# WEPPR

## Overview

WEPPR an R package to read and write WEPP input-output files and run WEPP  using the statistical software R. 

The [water erosion prediction project](https://www.fs.usda.gov/ccrc/tool/watershed-erosion-prediction-project-wepp) (WEPP) is a physically-based soil erosion computer model  supported by the United States Department of Agriculture (USDA).  

This package should allow those who are familiar with R a more convenient interface for running a large set of WEPP runs. 

For running WEPP with R, check out [DEPR](https://github.com/jarad/DEPR).

Also check out [WEPPemulator](https://github.com/jarad/WEPPemulator), an R package that has functionality to construct a statistical emulator for WEPP.

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
- `read_wb()` : Water balance
- `read_yld()` : Yield


### Code
``` r
library(WEPPR)

## get file path to data files
fpath_cli <- system.file("extdata", "092.63x040.90.cli", package="WEPPR")
fpath_slp <- system.file("extdata", "071000090603_2.slp", package="WEPPR")
fpath_man <- system.file("extdata", "071000090603_2.man", package="WEPPR")
fpath_run <- system.file("extdata", "071000090603_2.run", package="WEPPR")
fpath_sol <- system.file("extdata", "071000090603_2.sol", package="WEPPR")
fpath_wb <- system.file("extdata", "071000090603_2.wb", package="WEPPR")
fpath_yld <- system.file("extdata", "071000090603_2.yld", package="WEPPR")

## read input files
read_cli(fpath_cli)
read_slp(fpath_slp)
read_man(fpath_man)
read_run(fpath_run)
read_sol(fpath_sol)

## read output files
read_wb(fpath_wb)
read_yld(fpath_yld)
```

## Acknowledgements

The following individuals have contributed to this project:

- Jarad Niemi
- Gulzina Kuttubekova
- Aditya Ranade
- Benedict Neo
