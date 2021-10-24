
.onLoad <- function(libname, pkgname) {
  reticulate::source_python(system.file("python/generate_partitions.py", package = "nn2pr"))
}
