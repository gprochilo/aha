# On attachment print the following message

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("aha!")
}
