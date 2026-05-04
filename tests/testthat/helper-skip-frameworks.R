skip_if_not_loadable <- function(pkg) {
  ok <- tryCatch(
    requireNamespace(pkg, quietly = TRUE),
    error = function(e) FALSE
  )
  if (!isTRUE(ok))
    testthat::skip(paste0("package '", pkg, "' is not available or failed to load"))
}

.python_env_vars <- c(
  "RETICULATE_PYTHON",
  "RETICULATE_PYTHON_ENV",
  "RETICULATE_PYTHON_FALLBACK",
  "VIRTUAL_ENV"
)

.has_explicit_python_configuration <- function() {
  any(!is.na(Sys.getenv(.python_env_vars, unset = NA_character_)))
}

.usable_path_python <- function() {
  candidates <- Sys.which(c("python", "python3"))
  candidates <- unname(candidates[nzchar(candidates)])
  candidates <- candidates[file.exists(candidates)]
  if (!length(candidates))
    return("")

  for (path_python in candidates) {
    ok <- tryCatch({
      out <- suppressWarnings(system2(path_python, "--version", stdout = TRUE, stderr = TRUE))
      status <- attr(out, "status")
      is.null(status) || identical(status, 0L)
    }, error = function(e) FALSE)

    if (isTRUE(ok))
      return(normalizePath(path_python, winslash = "/", mustWork = FALSE))
  }

  ""
}

.has_python_configuration <- function() {
  if (requireNamespace("reticulate", quietly = TRUE) &&
      reticulate::py_available(initialize = FALSE)) {
    return(TRUE)
  }

  if (.has_explicit_python_configuration())
    return(TRUE)

  nzchar(.usable_path_python())
}

.with_python_discovery_env <- function(expr, force_path_python = FALSE) {
  old_conda_no_plugins <- Sys.getenv("CONDA_NO_PLUGINS", unset = NA_character_)
  old_managed_venv <- Sys.getenv("RETICULATE_USE_MANAGED_VENV", unset = NA_character_)
  old_reticulate_python <- Sys.getenv("RETICULATE_PYTHON", unset = NA_character_)

  Sys.setenv(
    CONDA_NO_PLUGINS = "true",
    RETICULATE_USE_MANAGED_VENV = "false"
  )

  if (isTRUE(force_path_python) &&
      is.na(old_reticulate_python) &&
      !.has_explicit_python_configuration()) {
    path_python <- .usable_path_python()
    if (nzchar(path_python))
      Sys.setenv(RETICULATE_PYTHON = path_python)
  }

  on.exit({
    if (is.na(old_conda_no_plugins)) {
      Sys.unsetenv("CONDA_NO_PLUGINS")
    } else {
      Sys.setenv(CONDA_NO_PLUGINS = old_conda_no_plugins)
    }

    if (is.na(old_managed_venv)) {
      Sys.unsetenv("RETICULATE_USE_MANAGED_VENV")
    } else {
      Sys.setenv(RETICULATE_USE_MANAGED_VENV = old_managed_venv)
    }

    if (is.na(old_reticulate_python)) {
      Sys.unsetenv("RETICULATE_PYTHON")
    } else {
      Sys.setenv(RETICULATE_PYTHON = old_reticulate_python)
    }
  }, add = TRUE)

  force(expr)
}

.keras_backend_available <- local({
  available <- NULL

  function() {
    if (!is.null(available))
      return(available)

    if (!.has_python_configuration()) {
      available <<- FALSE
      return(available)
    }

    available <<- .with_python_discovery_env(
      tryCatch({
        suppressWarnings(suppressMessages(tensorflow::tf_version()))
        TRUE
      }, error = function(e) FALSE),
      force_path_python = TRUE
    )

    available
  }
})

skip_if_keras_unavailable <- function() {
  .with_python_discovery_env({
    skip_if_not_loadable("keras")
    skip_if_not_loadable("tensorflow")
    if (!.keras_backend_available())
      testthat::skip("tensorflow is installed but its Python backend is not configured")
  })
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
