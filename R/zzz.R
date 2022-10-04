# Load RMAS module -------------------------------------------------------------------------
.onLoad <- function(libname, pkgname) {
  print("loading r4MAS")
  library.dynam("r4MAS", pkgname, libname)
}

.LastLib <- function(libpath) {
  print("unloading r4MAS")
  library.dynam.unload("r4MAS", libpath)
}
