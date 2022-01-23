.onLoad <- function(libname, pkgname) {
  reticulate::source_python(system.file("python/generate_partitions.py", package = "nn2poly"), envir = globalenv())
  reticulate::source_python(system.file("python/OLD_VERSION_generate_partitions.py", package = "nn2poly"), envir = globalenv())

}
