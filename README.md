# Grassland manuscript

## Nomenclature
Name of functions explained:
Key functions are named in the fashion of action_object_suffix.
Types of actions: down, tidy, read, calc, plot, test.
There can be multiple suffix.

Names of R objects explained:
Key R objects are named in the fashion of prefix_class_object_suffix.
Types of classes (example): dat, v, ras, df, gg.
Prefix "ls" means list.
Suffix can be used to refer to a subset on that level.

## Reproduce the whole analysis (slow)
0. Clone this GitHub repository and go into the grassland R project.

1. Install the grassland package.
```R
devtools::install(build_vignettes = FALSE, build = FALSE)
```

2. Set up "alldata" folder for input, intermediate, and output files in a clean base directory. Make "alldata" a symbolic link pointing to sym_dir you specify. 
```R
grassland::util_setup_dir(sym_dir = "/nfs/turbo/seas-zhukai/grassland/")
```
3. Install required dependencies.
```R
grassland::util_install_dependency()
```
A full list of required dependencies are included in the [DESCRIPTION](https://github.com/zhulabgroup/grassland/blob/5dd3b55c63894f3bd72a163f3e8c948342af0f99/DESCRIPTION) file.

4. Run steps in vignettes in the "vignettes" folder. Run the command below to make sure you are running the commands from project directory.
```R
knitr::opts_knit$set(root.dir = here::here())
```
Note that some steps are marked as not evaluated in the vignettes because they are very slow or require additional computational resources.

## Generate vignettes from intermediate files (fast)
0. Clone this GitHub repository and go into the grassland R project.

1. Install the grassland package.
```R
devtools::install(build_vignettes = FALSE, build = FALSE)
```
2. Install required dependencies.
```R
grassland::util_install_dependency()
```
A full list of required dependencies are included in the [DESCRIPTION](https://github.com/zhulabgroup/grassland/blob/5dd3b55c63894f3bd72a163f3e8c948342af0f99/DESCRIPTION) file.

3. Manually knit individual vignettes (or only the ones with changes). Run the command below to make sure you are knitting from project directory.
```R
knitr::opts_knit$set(root.dir = here::here())
```
Vignettes are set to be cached. Manually clear cache if needed (if changes are made outside the markdown files).

4. Rebuild manually knitted vignettes and load them into the package.
```R
grassland::util_build_vignettes_to_inst(clean = FALSE)
devtools::install(build_vignettes = TRUE, build = FALSE)
```

5. View these vignettes using different methods.
```R
browseVignettes(package = "grassland")
help(package = "grassland")
vignette(package = "grassland")
```
