.onLoad <- function(libname, pkgname){
  data(holidayCN,package = pkgname)
  that = Calendar(name="CN", holidays = holidayCN)
  op <- options()
  if (!"calendar" %in% names(op))  options(calendar=that)
  on.exit(options(op), add = TRUE)
}
