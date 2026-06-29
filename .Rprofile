# Use a project-local renv sandbox so activation does not depend on the shared
# user-cache sandbox, where stale lock acquisition can hang on fresh macOS runs.
if (!nzchar(Sys.getenv("RENV_PATHS_SANDBOX", unset = ""))) {
  Sys.setenv(RENV_PATHS_SANDBOX = "renv/sandbox")
}

source("renv/activate.R")
