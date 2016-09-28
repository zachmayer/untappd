# The `onload` function is usually defined in `zzz.R` in order to make sure that all of the
# rest of the package is loaded before we try to run it (because R loads the files in
# alphabetical order)

.onAttach <- function(libname, pkgname) {
  config_path <- get_default_config_path()
  if (file.exists(config_path)) {
    packageStartupMessage(paste("Loading config from:", config_path))
    tryCatch(
      load_config_file(config_path),
      error = function(e) packageStartupMessage(e$message))
  } else {
    packageStartupMessage(paste("You can put a config file at:", config_path))
  }
}