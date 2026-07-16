# cleanup python files after each test (based on vetiver's setup.R)

defer <- function (expr, envir=parent.frame(), priority=c("first", "last")) {
  priority <- match.arg(priority, choices = c("first", "last"))
  thunk <- as.call(list(function() expr))
  after <- priority == "last"
  do.call(base::on.exit, list(thunk, TRUE, after), envir = envir)
}

clean_python_tmp_dir <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) return()
  if (!reticulate::py_available()) return()

  tryCatch({
    python_temp <- reticulate::py_run_string(
      "import tempfile; x=tempfile.NamedTemporaryFile().name", local=TRUE)
    detritus <- list.files(
      dirname(python_temp$x),
      pattern = "__autograph_generated_file|__pycache__",
      full.names = TRUE)
    unlink(detritus, recursive=TRUE, force=TRUE)
  }, error = function(cnd) message("Cannot clean Python temp directory: ", cnd))
}

defer(clean_python_tmp_dir(), teardown_env())
