# download PRISM data
prism::prism_set_dl_dir(.path$cli_prism)
prism::get_prism_normals("tmean", "800m", annual = TRUE, keepZip = FALSE)
prism::get_prism_normals("ppt", "800m", annual = TRUE, keepZip = FALSE)
prism::get_prism_normals("vpdmax", "800m", annual = TRUE, keepZip = FALSE)
