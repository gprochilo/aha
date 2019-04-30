# On attachment print the following message

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the aha package!")
}
