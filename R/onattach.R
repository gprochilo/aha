# On attachment print the following message

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Warning: This package is under development")
}
