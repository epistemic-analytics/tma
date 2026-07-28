#' Conversation membership and per-modality window spans (Data View v2)
#'
#' TMA-aware successor to \code{tma.conversations()}. For each unit's coded
#' reference rows it returns the co-occurring rows within that row's
#' \emph{per-modality} window, plus a \code{rowWindows} payload that drives the
#' webtool Data View window-span hover.
#'
#' Unlike \code{tma.conversations()} (which predates the window/weight matrix and
#' uses a single scalar window), this derives windows from the resolved tensor
#' persisted on the accumulated object (\code{x$`_function.params`$tensor}) and
#' calls the libqe primitive \code{apply_tensor_members()} — the SAME code path
#' the network uses — so the Data View can't drift from the network.
#'
#' @param x An accumulated tma set (from \code{accumulate()}); must carry
#'   \code{_function.params$tensor} and \code{_function.params$mode_column}
#'   (populated by \code{accumulate()} since the 5a change).
#' @param codes Code column names. Defaults to \code{_function.params$codes}.
#' @param id_col Row-identity column. Default \code{"QEID"}.
#'
#' @return A list with \code{unitRows}, \code{allRows}, \code{toRemove} (row ids)
#'   and \code{rowWindows}: a named list keyed by reference row id, each
#'   \code{list(refMode, members = list(list(row, mode, distance, admittedBy,
#'   window, coOccurs)))}. (\code{conversations}/\code{unitConvs} grouping is
#'   layered on by the caller/server, mirroring \code{tma.conversations()}.)
#' @importFrom libqe apply_tensor_members
#' @export
tma.conversations2 <- function(x, codes = NULL, id_col = "QEID") {
  fp <- x$`_function.params`
  tensor <- fp$tensor
  if (is.null(tensor))
    stop("x lacks _function.params$tensor; re-accumulate with a tma build that persists it (5a).")
  if (is.null(codes)) codes <- fp$codes
  mode_col    <- attr(tensor, "mode_column")
  time_column <- if (!is.null(fp$time_column)) fp$time_column else ATTR_NAMES$CONTEXT_COL_ID

  send_cols <- attr(tensor, "sender_cols")
  recv_cols <- attr(tensor, "receiver_cols")
  mode_cols <- attr(tensor, "mode_column")
  tensor_dim           <- attr(tensor, "dim")
  tensor_sender_inds   <- attr(tensor, "sender_inds")   - 1
  tensor_receiver_inds <- attr(tensor, "receiver_inds") - 1
  tensor_mode_inds     <- attr(tensor, "mode_inds")     - 1

  rowWindows  <- list()
  allRows     <- integer()
  unitRowsAll <- integer()
  toRemove    <- integer()

  process_ctx <- function(ctx_o) {
    unit_rows <- attr(ctx_o, "tma.unit_rows")
    if (is.null(unit_rows) || length(unit_rows) == 0) return(invisible())

    # ---- prep identical to accumulate() so windows match the network ----
    ctx__ <- data.table::copy(ctx_o)
    cols_to_encode <- c(send_cols, recv_cols, mode_cols)
    cols_unq <- unique(cols_to_encode)
    if (!all(sapply(ctx__[, ..cols_to_encode], is.numeric))) {
      ctx__[, (cols_unq) := lapply(.SD, function(v) as.numeric(as.factor(v))), .SDcols = cols_unq]
    }
    unit_context_lookup <- as.matrix(ctx__[, ..cols_to_encode])
    unit_to_last <- seq.int(unit_rows[length(unit_rows)])
    ctx_unit_to_last <- ctx__[unit_to_last, ]

    res <- apply_tensor_members(
      tensor, tensor_dim, tensor_sender_inds, tensor_receiver_inds, tensor_mode_inds,
      unit_context_lookup[unit_to_last, , drop = FALSE] - 1,
      unit_rows - 1,
      as.matrix(ctx_unit_to_last[, ..codes]),
      ctx_unit_to_last[[time_column]]
    )

    # ---- reshape membership -> rowWindows (using original, unencoded values) ----
    ids     <- ctx_o[[id_col]]
    modev   <- as.character(ctx_o[[mode_col]])
    codeMat <- as.matrix(ctx_o[, ..codes])

    for (i in seq_along(unit_rows)) {
      ur   <- unit_rows[i]                 # 1-based position in ctx_o
      mem  <- res$row_window_members[[i]]  # 1-based positions in ctx_unit_to_last (== ctx_o[<= last])
      wins <- res$row_window_wins[[i]]

      windowHasCoOccur <- sum(codeMat[c(mem, ur), , drop = FALSE]) > 1
      members <- lapply(seq_along(mem), function(k) {
        g <- mem[k]
        list(row = ids[g], mode = modev[g], distance = ur - g,
             admittedBy = modev[g], window = wins[k],
             coOccurs = unname((sum(codeMat[g, ]) > 0) && windowHasCoOccur))
      })
      rowWindows[[as.character(ids[ur])]] <<- list(refMode = modev[ur], members = members)
      unitRowsAll <<- c(unitRowsAll, ids[ur])
      if (windowHasCoOccur) allRows <<- c(allRows, ids[c(mem, ur)]) else toRemove <<- c(toRemove, ids[ur])
    }
  }

  for (ctx in x$model$contexts) {
    if (is.data.frame(ctx)) process_ctx(ctx) else lapply(ctx, process_ctx)
  }

  list(
    unitRows   = unique(unitRowsAll),
    allRows    = unique(allRows),
    toRemove   = unique(toRemove),
    rowWindows = rowWindows
  )
}
