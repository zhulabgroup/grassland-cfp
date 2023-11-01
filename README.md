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

```R
devtools::install(build_vignettes = F, build = F)
```

Manually knit individual vignettes (or only the ones with changes) from project directory. Vignettes are set to be cached. Manually clear cache if needed (if changes are made outside the markdown files).

```R
library(grassland)
```

```R
build_vignettes_to_inst(clean = F)
```

```R
devtools::install(build_vignettes = T, build = F)
```

```R
browseVignettes("grassland")
```

Required packages: ***
