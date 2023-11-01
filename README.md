# Grassland manuscript

Name of functions explained:
Key functions are named in the fashion of action_object_suffix.
Types of actions: down, tidy, read, calc, plot, test.
There can be multiple suffix.

Names of R objects explained:
Key R objects are named in the fashion of prefix_class_object_suffix.
Types of classes (example): dat, v, ras, df, gg.
Prefix "ls" means list.
Suffix can be used to refer to a subset on that level.

Steps to generate vignettes:

1. Install the package.

```R
devtools::install(build_vignettes = FALSE, build = FALSE)
```

2. Manually knit individual vignettes (or only the ones with changes) from project directory. Vignettes are set to be cached. Manually clear cache if needed (if changes are made outside the markdown files).

```R
library(grassland)
build_vignettes_to_inst(clean = FALSE)
devtools::install(build_vignettes = TRUE, build = FALSE)
```

3. View these vignettes using different methods.

```R
browseVignettes(package = "grassland")
help(package = "grassland")
vignette(package = "grassland")
```

Required packages are included in the [DESCRIPTION](https://github.com/zhulabgroup/grassland/blob/5dd3b55c63894f3bd72a163f3e8c948342af0f99/DESCRIPTION) file.
