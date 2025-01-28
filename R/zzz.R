.onLoad <- function(libname, pkgname) {
  globalVariables(c(
    "..units_by", "..by", ".I", "QEID", "CID", "..conversations",
    "KEYCOL"
  ))
}


ATTR_NAMES <- list(
  CONTEXT_COL_ID = "__CONTEXT_ROWID__"
)