library(curl)               # Check availability of internet for install of remote libraries
if (has_internet()) devtools::install_github("crsh/papaja", ref = "devel")
library(papaja)             # APA formatted ms

library(tidyverse)
library(magrittr)           # pipes
library(rlang)              # quosures (in functions)
library(assertthat)         # asserts (in functions)

library(data.table)

library(linguisticsdown)    # IPA symbols
library(latexdiffr)         # track changes
library(cowplot)            # combining plots
library(gganimate)          # animations
library(plotly)             # 3D plots
library(processx)           # interface to orca (for plotly)
library(tufte)              # for quotes
library(kableExtra)         # for tables

library(lme4)               # for C-CuRE normalization through regression
library(mixtools)           # for drawing ellipses in the 3D plots
library(modelr)
library(diptest)            # test for bimodality

if (has_internet()) devtools::install_github("hlplab/MVBeliefUpdatr")
library(MVBeliefUpdatr)     # for ideal observers and adaptors 
library(LaplacesDemon)      # for additional density distributions (e.g., inverse-Wishart, W^-1)