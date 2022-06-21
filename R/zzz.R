python <- NULL

generate_partitions <- function(...) {
  python$generate_partitions(...)
}

.onLoad <- function(libname, pkgname) {
  # see https://rstudio.github.io/reticulate/articles/package.html#delay-loading-python-modules
  python <<- reticulate::import_from_path(
    module = "generate_partitions",
    path = system.file("python", package = "nn2poly"),
    delay_load = TRUE
  )
}
