# Grassland manuscript

Use this R package _grassland_ to reproduce key steps in the manuscript "Rapid shifts in grassland communities driven by climate change."

## Installation
### Option 1: permanent version
Please install the permanent version of our package following the steps below.

1. Download the zip folder on Zenodo. Remember the location where you've downloaded it to.
2. Unzip the downloaded zip file. You'll get a folder named grassland-cfp.
3. Open RStudio or your preferred R environment.
4. Open the R project grassland-cfp/grassland.Rproj.
5. Install devtools using the following command in R
```R
install.packages("devtools")
```
6. Install _grassland_ with
```R
devtools::install()
```

### Option 2: latest version
Alternatively, to install the latest version of our package from GitHub, follow the steps below.

1. Open RStudio or your preferred R environment.
2. Install devtools using the following command in R
```R
install.packages("devtools")
```
3. Install _grassland_ with
```R
devtools::install_github("zhulabgroup/grassland-cfp@release", auth_token = "[your token]")
```

## Vignettes
To reproduce the analysis, you can build and read the package vignette. It is like a long form documentation or usage guide for the package.

You may build the vignette as you install the package, by setting `build_vignettes = TRUE` in
```R
devtools::install(build_vignettes = TRUE)
```
or
```R
devtools::install_github("zhulabgroup/grassland-cfp@release", build_vignettes = TRUE, auth_token = "[your token]")
```

You can now view the vignette using
```R
browseVignettes(package = "grassland")
```

Then, click on the HTML link of the vignette to open it in your browser.

You can run the code chunks in the vignette in your R environment and explore the functions demonstrated.

## Additional files
Some functions are available but not directly demonstrated in the vignette. Please explore the help files.
```R
help(package = grassland)
```

You can find pre-loaded intermediate data files in the data/ folder.

You can find main and supplementary display items in the inst/figures/ and inst/tables/ folders.


If you run into any issues or have questions, feel free to open an issue on this GitHub repository. 

Thank you for using _grassland_!
