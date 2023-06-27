all_installed_packages <- installed.packages()[,1]

install_and_load_package <- function (name) {
  if (!is.element(name, all_installed_packages)) {
    install.packages(name)
  }

  cat(paste("Loading", name, "\n"))
  library(name, character.only = TRUE)
}

install_and_load_package("remotes")            # facilitate installation from github
install_and_load_package("curl")               # Check availability of internet for install of remote libraries

install_and_load_package("tidyverse")
install_and_load_package("magrittr")           # pipes
install_and_load_package("rlang")              # quosures (in functions)
install_and_load_package("assertthat")         # asserts (in functions)

install_and_load_package("data.table")

install_and_load_package("linguisticsdown")    # IPA symbols
install_and_load_package("phonR")
install_and_load_package("latexdiffr")         # track changes
install_and_load_package("ggh4x")              # sizing plot panels
install_and_load_package("cowplot")            # combining plots
install_and_load_package("gganimate")          # animations
install_and_load_package("plotly")             # 3D plots
install_and_load_package("processx")           # interface to orca (for plotly)
install_and_load_package("tufte")              # for quotes
install_and_load_package("kableExtra")         # for tables

install_and_load_package("lme4")               # for C-CuRE normalization through regression
install_and_load_package("mixtools")           # for drawing ellipses in the 3D plots
install_and_load_package("modelr")
install_and_load_package("diptest")            # test for bimodality
install_and_load_package("NMOF")               # for grid search
install_and_load_package("parallel")           # use multicore to speed up grid search and optimization
install_and_load_package("LaplacesDemon")      # for additional density distributions (e.g., inverse-Wishart, W^-1)


if (has_internet()) install_github("crsh/papaja", ref = "devel")
library(papaja)
if (has_internet()) install_github("hlplab/MVBeliefUpdatr", INSTALL_opts = "--no-lock", upgrade = "never")
library(MVBeliefUpdatr)
