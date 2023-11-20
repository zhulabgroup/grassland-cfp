# Grassland manuscript

Use this R package _grassland_ to reproduce key steps in the manuscript "Rapid shifts in grassland communities driven by climate change."

## Installation
To install the latest version of our package from GitHub, you can use the devtools package. First, you need to install devtools using the following command in R:
```R
install.packages("devtools")
```

Then you can install _grassland_ with:
```R
devtools::install_github("zhulabgroup/grassland", build_vignettes = TRUE)
```

## Vignettes
To reproduce the analysis, you can read the package vignette. It is like a long form documentation or usage guide for the package.

After installation, you can view the vignette using:
```R
browseVignettes(package = "grassland")
```

Then, click on the HTML link of the vignette to open it in your browser.

## Additional files
Some functions are available but not directly demonstrated in the vignette. Please explore the help files:
```R
help(package = grassland)
```

You can find pre-loaded intermediate data files in the data/ folder.

You can find main and supplementary display items in the inst/figures/ and inst/tables/ folders.


If you run into any issues or have questions, feel free to open an issue on this GitHub repository. 

Thank you for using _grassland_!
