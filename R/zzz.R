.onLoad <- function(libname, pkgname) {
  reticulate::source_python(system.file("python/generate_partitions.py", package = "nn2poly"), envir = globalenv())
  reticulate::source_python(system.file("python/generate_partitions_old.py", package = "nn2poly"), envir = globalenv())

}
