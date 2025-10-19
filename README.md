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
library(scExploration)
```

## Usage
Here is a simple demonstration that comes from the 10X Genomics PBMC dataset

```{r}
pbmc10k.data <- Read10X(data.dir = "filtered_feature_bc_matrix/")
rownames(x = pbmc10k.data[["Antibody Capture"]]) <- gsub(pattern = "_[control_]*TotalSeqB", replacement = "",
    x = rownames(x = pbmc10k.data[["Antibody Capture"]]))

pbmc10k <- CreateSeuratObject(counts = pbmc10k.data[["Gene Expression"]], min.cells = 3, min.features = 200)
pbmc10k <- NormalizeData(pbmc10k)
pbmc10k[["ADT"]] <- CreateAssayObject(pbmc10k.data[["Antibody Capture"]][, colnames(x = pbmc10k)])
pbmc10k <- NormalizeData(pbmc10k, assay = "ADT", normalization.method = "CLR")

plot1 <- FeatureScatter(pbmc10k, feature1 = "adt_CD19", feature2 = "adt_CD3", pt.size = 1)
plot2 <- FeatureScatter(pbmc10k, feature1 = "adt_CD4", feature2 = "adt_CD8a", pt.size = 1)
plot3 <- FeatureScatter(pbmc10k, feature1 = "adt_CD3", feature2 = "CD3E", pt.size = 1)
(plot1 + plot2 + plot3) & NoLegend()


# Note that all operations below are performed on the RNA assay Set and verify that the
# default assay is RNA
DefaultAssay(pbmc10k) <- "RNA"
DefaultAssay(pbmc10k)



# perform visualization and clustering steps
pbmc10k <- NormalizeData(pbmc10k)
pbmc10k <- FindVariableFeatures(pbmc10k)
pbmc10k <- ScaleData(pbmc10k)
pbmc10k <- RunPCA(pbmc10k, verbose = FALSE)
pbmc10k <- FindNeighbors(pbmc10k, dims = 1:30)
pbmc10k <- FindClusters(pbmc10k, resolution = 0.8, verbose = FALSE)
pbmc10k <- RunUMAP(pbmc10k, dims = 1:30)
DimPlot(pbmc10k, label = TRUE)
```

```{r}
RNA_Checker(pbmc10k)
```
scExplorer contains very simple to use and self-explanatory tools. It opens a shiny dashboard that enables the user to select a series of markers for the initial biplot. 
![Image_1](/images/image_1.png)

Once opened, the tool then enables the user to select cells on the biplot with either the box or lasso tools from the plotly package. 
![Image_2](/images/image_2.png)

Selected cells are then highlighted on the umap and a table created using a selected column from the meta data. The selected cells can then be exported back into the R environment with the **Export Selected Cells** button.
![Image_3](/images/image_3.png)

This process can also proceed forward from the umap via the Cluster Selection tab. 
![Image_4](/images/image_4.png)
![Image_5](/images/image_5.png)

One can also look at multimodal (i.e., CITE-seq) data. This process is very similar to the RNA modality analysis but now enables simultaneous plotting of both ADT and RNA data on the biplots. 

```{r}
ADT_Checker(pbmc10k)
```

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
