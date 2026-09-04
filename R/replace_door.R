# =============================================================================
# replace_door.R — Plug door() output back into an accumulation object
# =============================================================================

#' Replace raw co-occurrence columns with door-adjusted values
#'
#' After door(), co-occurrence columns are named door_<original "code & code">.
#' This function copies those values into accum$connection.counts, replacing
#' the raw co-occurrence columns, so model() / ena.make.set() use the door-adjusted
#' data without changing downstream code paths.
#'
#' @param accum Object from \code{tma::accumulate()}.
#' @param door_df Output of \code{door()} (must include \code{door_*} columns).
#' @return \code{accum} with updated \code{connection.counts}.
#' @export
replace_door <- function(accum, door_df) {
  if (!("connection.counts" %in% names(accum))) {
    stop("`accum` has no connection.counts")
  }
  conn <- accum$connection.counts
  if (!("ENA_UNIT" %in% names(conn)) && "QEUNIT" %in% names(conn)) {
    conn$ENA_UNIT <- as.character(conn$QEUNIT)
  }
  if (!("ENA_UNIT" %in% names(conn))) {
    stop("`accum$connection.counts` lacks ENA_UNIT (and QEUNIT)")
  }
  if (!("ENA_UNIT" %in% names(door_df)) && "QEUNIT" %in% names(door_df)) {
    door_df$ENA_UNIT <- as.character(door_df$QEUNIT)
  }
  if (!("ENA_UNIT" %in% names(door_df))) {
    stop("`door_df` lacks ENA_UNIT (and QEUNIT)")
  }

  nm <- names(conn)
  code_cols <- nm[grepl("\\s*&\\s*", nm) & vapply(conn, is.numeric, logical(1))]
  if (!length(code_cols)) {
    stop("No numeric co-occurrence columns (names with ' & ') in connection.counts")
  }

  prefix <- "door_"
  door_cols <- paste0(prefix, code_cols)
  missing <- setdiff(door_cols, names(door_df))
  if (length(missing)) {
    stop("Door table missing columns: ", paste(missing, collapse = ", "))
  }

  key_acc <- as.character(conn$ENA_UNIT)
  key_door <- as.character(door_df$ENA_UNIT)
  idx <- match(key_acc, key_door)
  if (anyNA(idx)) {
    bad <- which(is.na(idx))[seq_len(min(5L, sum(is.na(idx))))]
    stop("Cannot align by ENA_UNIT. Unmatched rows (examples): ",
         paste(bad, collapse = ", "))
  }

  table_class_before <- class(conn)
  use_datatable <- "data.table" %in% table_class_before

  for (k in seq_along(code_cols)) {
    j <- code_cols[k]
    dj <- door_cols[k]

    v <- door_df[[dj]][idx]
    if (!is.numeric(v)) {
      v <- suppressWarnings(as.double(as.character(v)))
    }
    if (any(!is.finite(v))) {
      stop("Non-finite values after replacement in column: ", j)
    }

    old_col_class <- class(conn[[j]])
    v <- structure(as.double(v), class = old_col_class)

    if (use_datatable) {
      data.table::set(conn, j = j, value = v)
    } else {
      conn[[j]] <- v
    }
  }

  class(conn) <- table_class_before
  accum$connection.counts <- conn
  accum
}

#' Apply door smoothing to an accumulation object
#'
#' This convenience wrapper runs \code{\link{door}()} on an accumulation object's
#' co-occurrence table and then writes the door-smoothed values back with
#' \code{\link{replace_door}()}. Use \code{\link{door}()} directly when you want
#' to inspect or compare the intermediate \code{door_*} columns before replacing
#' the accumulation values.
#'
#' @param accum Object from \code{tma::accumulate()}.
#' @param ... Arguments passed to \code{\link{door}()}, including
#'   \code{unit_col}, \code{time_col}, \code{method}, and method-specific
#'   settings.
#' @return \code{accum} with door-smoothed co-occurrence columns.
#' @export
door_accumulation <- function(accum, ...) {
  if ("connection.counts" %in% names(accum)) {
    conn <- accum$connection.counts
  } else if ("model" %in% names(accum) &&
             "row.connection.counts" %in% names(accum$model)) {
    conn <- accum$model$row.connection.counts
  } else {
    stop("`accum` must contain `connection.counts` or `model$row.connection.counts`.")
  }

  door_df <- door(conn, ...)
  replace_door(accum, door_df)
}
