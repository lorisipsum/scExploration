# scExplorer
A shiny tool that enables flow cytometry-like analysis of scRNA-seq and CITE-seq data

## Overview
`scExplorer` is a shiny dashboard that enables a flow cytometry-like analysis scRNA-seq and CITE-seq data. The user can plot up to three dimensions on a biplot and "gate" on cells for display on the umap. Alternatively, the user can select cells on the umap to be displayed on the biplot. Such analyses can reveal novel insights that may be difficult to elucidate with other bioinformatic techniques.

## How to Install of scExplorer
`scExplorer` can be installed from this Github repository by simply copying the following lines of code into your R terminal, which will install everything that you need to use `scExplorer` on your computer: 
```{r}
if (!require("devtools")) 
  install.packages("devtools")

devtools::install_github("lorisipsum/scExploration")
library(scExplorer)
```

## Usage


## Description
Package: scExplorer

Title: Flow cytometry-like analysis of scRNA-seq and CITE-seq data

Version: 0.0.0.2000

Authors@R: 
        person(given = "Brian", family = "Thompson", email = "dr.brianthompson1@gmail.com", role = c("aut", "cre"))
        
Description: scExplorer is a shiny dashboard that enables a flow cytometry-like analysis of scRNA-seq and CITE-seq data.
    The scExplorer package was created by Brian Thompson in the Spring of 2024.

License: MIT + file LICENSE

Encoding: UTF-8

Roxygen: list(markdown = TRUE)

RoxygenNote: 7.3.1
Imports: 
    dplyr,
    ggplot2,
    Matrix,
    methods,
    plotly,
    scCustomize,
    Seurat,
    shiny,
    shinydashboard,
    tidyr,
    viridis,
    magrittr,
    tibble
Depends: 
    R (>= 2.10)
Suggests: 
    testthat (>= 3.0.0)
Config/testthat/edition: 3
