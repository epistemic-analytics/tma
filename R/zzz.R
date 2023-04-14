.onLoad <- function(libname, pkgname) {
  globalVariables(c(
    "..units_by", "..by", ".I", "QEID", "CID", "..conversations",
    "KEYCOL"
  ))
}
