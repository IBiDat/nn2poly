# cleanup python files after each test (based on vetiver's setup.R)
teardown({
  if (requireNamespace("reticulate", quietly = TRUE)) {
    python_temp_dir <- dirname(reticulate::py_run_string(
      "import tempfile; x=tempfile.NamedTemporaryFile().name",
      local = TRUE
    )$x)
    detritus <- Sys.glob(file.path(python_temp_dir, c("*.py", "__pycache__")))
    unlink(detritus, recursive=TRUE, force=TRUE)
  }
})
