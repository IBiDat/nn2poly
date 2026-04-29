skip_if_not_loadable <- function(pkg) {
  ok <- tryCatch(
    requireNamespace(pkg, quietly = TRUE),
    error = function(e) FALSE
  )
  if (!isTRUE(ok))
    testthat::skip(paste0("package '", pkg, "' is not available or failed to load"))
}

skip_if_keras_unavailable <- function() {
  skip_if_not_loadable("keras")
  skip_if_not_loadable("tensorflow")
}

skip_if_torch_unavailable <- function() {
  skip_if_not_loadable("torch")
  ok <- tryCatch(torch::torch_is_installed(), error = function(e) FALSE)
  if (!isTRUE(ok))
    testthat::skip("torch is installed but its backend is not configured; run torch::install_torch()")
}

skip_if_luz_unavailable <- function() {
  skip_if_not_loadable("luz")
  skip_if_torch_unavailable()
}

