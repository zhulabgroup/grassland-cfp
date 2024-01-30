#' @export
calc_trait_thin <- function(path_occ_thin = NULL, dat_trait, option = "spatial") {
  if (is.null(path_occ_thin)) {
    path_occ_thin <- str_c("alldata/intermediate/climate-niche/occ_thin_", option, ".rds")
  }

  dat_occ_thin <- read_rds(path_occ_thin)
  dat_trait_thin <- dat_trait %>%
    right_join(
      dat_occ_thin %>%
        select(key, species = consolidatedName),
      by = c("key", "species")
    )

  return(dat_trait_thin)
}

#' @export
calc_thin_occ <- function(dat_occ, dat_trait, outdir = "alldata/intermediate/climate-niche/", option = "spatial", num_cores = 2) {
  cl <- makeCluster(num_cores, outfile = "")
  registerDoSNOW(cl)

  v_species <- dat_occ$consolidatedName %>% unique()

  ls_dat_occ_thin <-
    foreach(
      species = v_species,
      .packages = c("tidyverse", "sf"),
      .export = c("calc_thin_occ_sp", "thin_clim", "thin.algorithm_clim")
    ) %dopar% {
      print(species)
      thin <- dat_occ %>%
        filter(consolidatedName == species) %>%
        mutate(
          lon = unlist(map(.$geometry, 1)),
          lat = unlist(map(.$geometry, 2))
        ) %>%
        left_join(dat_trait, by = c("key", "consolidatedName" = "species")) %>%
        mutate(
          tmp = (tmp - mean(tmp, na.rm = T)) / sd(tmp, na.rm = T),
          ppt = (ppt - mean(ppt, na.rm = T)) / sd(ppt, na.rm = T),
          vpd = (vpd - mean(vpd, na.rm = T)) / sd(vpd, na.rm = T)
        ) %>% # standardize to mean 0, standard deviation 1
        calc_thin_occ_sp(option = option)
      thin
    }
  dat_occ_thin <- bind_rows(ls_dat_occ_thin)
  stopCluster(cl)


  path_occ_thin <- str_c(outdir, "occ_thin_", option, ".rds")
  write_rds(dat_occ_thin, path_occ_thin)

  return(path_occ_thin)
}

calc_thin_occ_sp <- function(data, option = "spatial") {
  set.seed(31416)

  # define function to use spThin to sample and return data frame

  full_tbl <- tibble(species = "dummy", data) # create dummy species for spThin function

  if (option == "spatial") {
    thin_ls <- spThin::thin(
      loc.data = full_tbl,
      lat.col = "lat",
      long.col = "lon",
      spec.col = "species",
      thin.par = 10, # distance in km that records to be separated by
      reps = 1,
      locs.thinned.list.return = TRUE,
      write.files = FALSE,
      write.log.file = FALSE
    )
  }

  if (option == "climate") {
    thin_ls <- thin_clim(
      loc.data = full_tbl,
      lat.col = "tmp",
      long.col = "ppt",
      spec.col = "species",
      thin.par = 0.2, # Euclidean distance (unitless) that records to be separated by
      reps = 1,
      locs.thinned.list.return = TRUE,
      write.files = FALSE,
      write.log.file = FALSE
    )
  }

  thin_tbl <- thin_ls[[1]] %>%
    rownames() %>%
    as.numeric() %>%
    slice(full_tbl, .) %>%
    select(consolidatedName, key)
}

thin_clim <- function(loc.data, lat.col = "LAT", long.col = "LONG", spec.col = "SPEC",
                      thin.par, reps, locs.thinned.list.return = FALSE, write.files = TRUE,
                      max.files = 5, out.dir, out.base = "thinned_data", write.log.file = TRUE,
                      log.file = "climate_thin_log.txt", verbose = TRUE) {
  log.begin <- paste("**********************************************",
    "\n", "Beginning Climate Thinning.\n", "Script Started at:",
    date(),
    sep = " "
  )
  if (verbose) {
    cat(log.begin)
  }
  if (write.log.file) {
    write(log.begin, file = log.file, append = TRUE)
  }
  locs.df <- loc.data
  species <- unique(locs.df[[which(names(locs.df) == spec.col)]])
  if (length(species) > 1) {
    log.spec.warn.1 <- "There appear to be more than one species name in this *.csv file."
    warning(log.spec.warn.1)
    if (write.log.file) {
      write(log.spec.warn.1, file = log.file, append = TRUE)
    }
    species <- species[1]
    log.spec.warn.2 <- paste(
      "Only using species name:",
      species
    )
    warning(log.spec.warn.2)
    if (write.log.file) {
      write(log.spec.warn.2, file = log.file, append = TRUE)
    }
  }
  lat <- which(names(locs.df) == lat.col)
  long <- which(names(locs.df) == long.col)
  locs.long.lat <- as.data.frame(cbind(locs.df[[long]], locs.df[[lat]]))
  log.thin.par <- paste(
    "\nThinning Parameter Used (in km):",
    thin.par
  )
  if (write.log.file) {
    write(log.thin.par, file = log.file, append = TRUE)
  }
  log.num.reps <- paste(
    "Number of replicates of thinning script:",
    reps
  )
  if (write.log.file) {
    write(log.num.reps, file = log.file, append = TRUE)
  }
  thin.time <- system.time(locs.thinned <- thin.algorithm_clim(
    rec.df.orig = locs.long.lat,
    thin.par = thin.par, reps = reps
  ))
  if (write.log.file) {
    write("\nElapsed time for thinning completion",
      file = log.file,
      append = TRUE
    )
  }
  if (write.log.file) {
    write(thin.time, file = log.file, append = TRUE)
  }
  lat.long.thin.count <- unlist(lapply(locs.thinned, nrow))
  locs.thinned.tbl <- table(lat.long.thin.count)
  if (verbose) {
    cat("\n")
    print(locs.thinned.tbl)
  }
  if (write.log.file) {
    write("\nNumber of data.frames per locations retained\nloc.cnt df.freq",
      file = log.file, append = TRUE
    )
  }
  if (write.log.file) {
    write(names(locs.thinned.tbl),
      file = log.file, append = TRUE,
      ncolumns = length(names(locs.thinned.tbl)), sep = "\t"
    )
  }
  if (write.log.file) {
    write(locs.thinned.tbl,
      file = log.file, append = TRUE,
      ncolumns = length(locs.thinned.tbl), sep = "\t"
    )
  }
  max.thin.recs <- max(lat.long.thin.count)
  log.max.rec <- paste(
    "Maximum number of records after thinning:",
    max.thin.recs
  )
  if (verbose) {
    print(log.max.rec)
  }
  if (write.log.file) {
    write(log.max.rec, file = log.file, append = TRUE)
  }
  max.dfs <- which(lat.long.thin.count == max.thin.recs)
  max.dfs.length <- length(max.dfs)
  log.max.df.cnt <- paste(
    "Number of data.frames with max records:",
    max.dfs.length
  )
  if (verbose) {
    print(log.max.df.cnt)
  }
  if (write.log.file) {
    write(log.max.df.cnt, file = log.file, append = TRUE)
  }
  if (write.files) {
    if (verbose) {
      print("Writing new *.csv files")
    }
    if (write.log.file) {
      write("\n**New *.csv file creation:**",
        file = log.file,
        append = TRUE
      )
    }
    n.csv <- min(c(max.files, max.dfs.length))
    if (!file.exists(out.dir)) {
      log.dir <- paste("Created new output directory: ",
        out.dir,
        sep = ""
      )
      dir.create(out.dir, recursive = TRUE)
    } else {
      log.dir <- paste("Writing new *.csv files to output directory: ",
        out.dir,
        sep = ""
      )
    }
    warning(log.dir)
    if (write.log.file) {
      write(log.dir, file = log.file, append = TRUE)
    }
    if (!grepl("/$", out.dir)) {
      out.dir <- paste(out.dir, "/", sep = "")
    }
    csv.files <- paste(out.dir, out.base, "_thin", rep(1:n.csv),
      ".csv",
      sep = ""
    )
    for (df in 1:n.csv) {
      df.temp <- locs.thinned[[max.dfs[df]]]
      df.temp <- cbind(
        rep(as.character(species), max.thin.recs),
        df.temp
      )
      colnames(df.temp) <- c(spec.col, long.col, lat.col)
      while (file.exists(csv.files[df])) {
        csv.files[df] <- sub(".csv$", "_new.csv", csv.files[df])
        log.csv.overwrite <- paste(csv.files[df], "' already exists. Renaming file \n                                   to avoid overwriting.")
        warning(log.csv.overwrite)
        if (write.log.file) {
          write(log.csv.overwrite, file = log.file, append = TRUE)
        }
      }
      write.csv(df.temp,
        file = csv.files[df], quote = FALSE,
        row.names = FALSE
      )
      log.write.file <- paste("Writing file:", csv.files[df])
      if (verbose) {
        print(log.write.file)
      }
      if (write.log.file) {
        write(log.write.file, file = log.file, append = TRUE)
      }
    }
  } else {
    log.write.file <- "No files written for this run."
    if (verbose) {
      print(log.write.file)
    }
    if (write.log.file) {
      write(log.write.file, file = log.file, append = TRUE)
    }
  }
  if (locs.thinned.list.return) {
    return(locs.thinned)
  }
}

thin.algorithm_clim <- function(rec.df.orig, thin.par, reps) {
  reduced.rec.dfs <- vector("list", reps)
  DistMat.save <- dist(rec.df.orig, method = "euclidean", diag = T, upper = T, p = 2) %>%
    as.matrix() %>%
    `<`(thin.par)
  diag(DistMat.save) <- FALSE
  DistMat.save[is.na(DistMat.save)] <- FALSE
  SumVec.save <- rowSums(DistMat.save)
  df.keep.save <- rep(TRUE, length(SumVec.save))
  for (Rep in seq_len(reps)) {
    DistMat <- DistMat.save
    SumVec <- SumVec.save
    df.keep <- df.keep.save
    while (any(DistMat) && sum(df.keep) > 1) {
      RemoveRec <- which(SumVec == max(SumVec))
      if (length(RemoveRec) > 1) {
        RemoveRec <- sample(RemoveRec, 1)
      }
      SumVec <- SumVec - DistMat[, RemoveRec]
      SumVec[RemoveRec] <- 0L
      DistMat[RemoveRec, ] <- FALSE
      DistMat[, RemoveRec] <- FALSE
      df.keep[RemoveRec] <- FALSE
    }
    rec.df <- rec.df.orig[df.keep, , drop = FALSE]
    colnames(rec.df) <- c("Longitude", "Latitude")
    reduced.rec.dfs[[Rep]] <- rec.df
  }
  reduced.rec.order <- unlist(lapply(reduced.rec.dfs, nrow))
  reduced.rec.order <- order(reduced.rec.order, decreasing = TRUE)
  reduced.rec.dfs <- reduced.rec.dfs[reduced.rec.order]
  return(reduced.rec.dfs)
}
