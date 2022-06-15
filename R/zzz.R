.onLoad <- function(libname, pkgname) {
  reticulate::source_python(system.file("python/generate_partitions.py", package = "nn2poly"), envir = asNamespace("nn2poly"))
  reticulate::source_python(system.file("python/OLD_VERSION_generate_partitions.py", package = "nn2poly"), envir = asNamespace("nn2poly"))

}
