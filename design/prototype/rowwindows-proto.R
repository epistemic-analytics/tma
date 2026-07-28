# THROWAWAY PROTOTYPE — not the shipping algorithm.
#
# Purpose: emit the `rowWindows` payload the webtool Data View client will bind to,
# so client UI work (per-modality window-span hover) can proceed in parallel.
# The PRODUCTION path is libqe::apply_tensor emitting row-level membership, with
# tma.conversations2 as a thin reshaper (see ../dataview-flexible-window.md 5b).
#
# Reads:
#   x$`_function.params`$tensor  (5a: persisted per-modality window/weight matrix)
#   x$model$contexts             (retained hoo horizons; attr "tma.unit_rows")
#
# Window interpretation (documented assumption for the throwaway):
#   a candidate earlier row r' is in reference row r's window iff
#     (pos(r) - pos(r')) <= tensor[ modality(r'), "window" ]     # each modality's own reach
#   The libqe primitive is authoritative; this R walk only shapes the payload.

suppressMessages({library(tma); library(data.table)})

tma.conversations2.proto <- function(x, codes, id_col = "QEID") {
  ten     <- x$`_function.params`$tensor
  modeCol <- attr(ten, "mode_column")
  modes   <- dimnames(ten)[["modes"]]
  winByMode <- setNames(as.integer(ten[modes, "window"]), modes)

  rowWindows <- list(); allRows <- integer(); unitRowsAll <- integer(); toRemove <- integer()

  handle_ctx <- function(ctx) {
    unitRows <- attr(ctx, "tma.unit_rows"); if (is.null(unitRows)) return(invisible())
    rowid <- ctx[[id_col]]; modv <- as.character(ctx[[modeCol]])
    codeMat <- as.matrix(ctx[, ..codes]); hasCode <- rowSums(codeMat) > 0
    for (ur in unitRows) {
      cand  <- seq_len(ur)
      inWin <- cand[(ur - cand) <= winByMode[modv[cand]]]
      windowHasCoOccur <- sum(codeMat[inWin, , drop = FALSE]) > 1
      members <- lapply(setdiff(inWin, ur), function(p) list(
        row = rowid[p], mode = modv[p], distance = ur - p,
        admittedBy = modv[p], coOccurs = unname(hasCode[p] && windowHasCoOccur)))
      rowWindows[[as.character(rowid[ur])]] <<- list(
        refMode = modv[ur], windowByMode = as.list(winByMode), members = members)
      unitRowsAll <<- c(unitRowsAll, rowid[ur])
      if (windowHasCoOccur) allRows <<- c(allRows, rowid[inWin]) else toRemove <<- c(toRemove, rowid[ur])
    }
  }
  for (ctx in x$model$contexts) if (is.data.frame(ctx)) handle_ctx(ctx) else lapply(ctx, handle_ctx)

  list(unitRows = unique(unitRowsAll), allRows = unique(allRows),
       toRemove = unique(toRemove), rowWindows = rowWindows)
}

build <- function(chat_win, resource_win) {
  data(test_mockdata, package = "tma"); d <- data.table(test_mockdata)
  HOO <- rules(modality %in% "chat"     & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition,
               modality %in% "resource" & userID    %in% UNIT$userID    & condition %in% UNIT$condition)
  cm  <- contexts(x = d, units = c("userID","condition"), hoo_rules = HOO)
  ten <- context_tensor(cm$model$raw.input, mode_column = "modality", default_window = chat_win)
  ten["resource","window"] <- resource_win
  acc <- accumulate(data.table::copy(cm), codes = c("A","B","C"), tensor = ten)
  acc$`_function.params`$tensor <- ten                 # 5a: persist
  acc
}

if (sys.nframe() == 0) {
  codes <- c("A","B","C")
  outM <- tma.conversations2.proto(build(2, 6), codes)   # chat=2, resource=6
  outU <- tma.conversations2.proto(build(2, 2), codes)   # uniform=2

  cat("units:", length(outM$rowWindows), " allRows:", length(outM$allRows),
      " toRemove:", length(outM$toRemove), "\n")

  # member-level cross-check: pick a ref row whose window admits a far resource row
  refs <- names(outM$rowWindows)
  pick <- refs[ which(sapply(refs, function(k) {
    m <- outM$rowWindows[[k]]$members
    any(vapply(m, function(x) x$mode == "resource" && x$distance > 2, logical(1)))
  }))[1] ]
  inM <- vapply(outM$rowWindows[[pick]]$members, function(x) x$row, numeric(1))
  inU <- vapply(outU$rowWindows[[pick]]$members, function(x) x$row, numeric(1))
  cat(sprintf("\nref row %s: per-modality window admits rows {%s}; uniform admits {%s}\n",
              pick, paste(inM, collapse=","), paste(inU, collapse=",")))
  cat("-> per-modality reaches further back (resource window 6 > 2):",
      length(setdiff(inM, inU)) > 0, "\n")
}
