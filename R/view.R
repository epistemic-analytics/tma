##
#' @title Find conversations by unit
#'
#' @description Find rows of conversations by unit
#'
#' @details [TBD]
#'
#' @param x [TBD]
#' @param units [TBD]
#' @param units.by [TBD]
#' @param codes [TBD]
#' @param conversation.by [TBD]
#' @param window [TBD]
#' @param conversation.exclude [TBD]
#'
#' @export
#' @return list containing row indices representing conversations
tma.conversations = function(
  x, units, 
  units.by = NULL, codes = NULL, conversation.by = NULL, 
  window = 4, conversation.exclude = c()
) {
  if(is.null(units.by)) {
    units.by = x$`_function.params`$units.by;
    if(is.null(units.by)) {
      stop("Unable to find values for `units.by`")
    }
  }
  
  if(is(x, "data.frame")) {
    rawAcc2 = data.table::data.table(x);
  }
  else {
    rawAcc2 = x$model$raw.input;
  }

  rawAcc2$KEYCOL = merge_columns(rawAcc2, conversation.by);
  conversationsTable2 = rawAcc2[, paste(.I, collapse = ","), by = c(conversation.by)];
  rows2 = lapply(conversationsTable2$V1, function(x) as.numeric(unlist(strsplit(x, split=","))));
  names(rows2) = merge_columns(conversationsTable2, conversation.by);
  unitRows2 = rawAcc2$QEUNIT;

  codedRows = rawAcc2[, rowSums(.SD), .SDcols = codes] > 0
  codedUnitRows2 = which(unitRows2 %in% units & codedRows)
  codedUnitRows2 = codedUnitRows2[!(codedUnitRows2 %in% as.vector(unlist(rows2[conversation.exclude])))]
  codedUnitRowConvs2 = rawAcc2[codedUnitRows2, KEYCOL];

  codedUnitRowConvsAll = NULL;
  codedUnitRowConvsAll2 = NULL;
  unitRowsNotCooccurred = c()
  if(length(codedUnitRows2) > 0) {
    codedUnitRowConvsAll = unique(unlist(sapply(X = 1:length(codedUnitRows2), simplify = F, FUN = function(x) {
      thisConvRows = rows2[[codedUnitRowConvs2[x]]]
      thisRowInConv = which(thisConvRows == codedUnitRows2[x])
      winUse = ifelse(is.infinite(window), thisRowInConv, window)
      thisRowAndWindow = rep(thisRowInConv,winUse) - (winUse-1):0
      # coOccursFound = all(rawAcc2[thisConvRows[thisRowAndWindow[thisRowAndWindow > 0]], lapply(.SD, sum), .SDcols=codes] > 0)
      coOccursFound = sum(rawAcc2[thisConvRows[thisRowAndWindow[thisRowAndWindow > 0]], lapply(.SD, sum), .SDcols=codes]) > 1
      # browser(expr = { thisConvRows[thisRowInConv] == 12 })
      if(coOccursFound) {
        thisConvRows[thisRowAndWindow[thisRowAndWindow > 0]]
      }
      else {
        unitRowsNotCooccurred <<- c(unitRowsNotCooccurred, thisConvRows[thisRowInConv]);
        
        NULL
      }
    })))
  }

  # browser()
  unitConvs <- unique(rawAcc2[codedUnitRows2, KEYCOL]);
  return(list(
    conversations = as.list(rows2),
    unitConvs = unitConvs,
    allRows = codedUnitRowConvsAll,
    unitRows = codedUnitRows2,
    convRows = unique(unlist(sapply(unitConvs, function(x) { rows2[[x]] }))),
    toRemove = unitRowsNotCooccurred
  ));
}


#' Title
#'
#' @param x TBD
#' @param wh TBD 
#' @param text_col TBD
#' @param units.by TBD
#' @param conversation.by TBD
#' @param codes TBD
#' @param window_size TBD
#' @param more_cols TBD
#' @param in_browser TBD
#'
#' @return TBD
#' @export
view <- function(
  x, wh, 
  text_col = "text", 
  units.by = x$`_function.params`$units.by,
  conversation.by = x$`_function.params`$conversation.by,
  codes = x$rotation$codes,
  window_size = x$`_function.params`$window_size,
  more_cols = NULL,
  in_browser = FALSE
) {
  unit_conv <- tma.conversations(x = x, units = wh, units.by = units.by, conversation.by = conversation.by, codes = codes, window = window_size);
  rows <- x$model$contexts[[wh]];
  if(is.null(rows)) {
    stop(paste0("Now rows found for context: ", wh));
  }

  cols = unique(c("QEID", "QEUNIT", units.by, conversation.by, text_col, more_cols, codes));
  tbl <- x$model$raw.input[unit_conv$convRows, cols, with = FALSE];
  unit_conv$unitRows <- unit_conv$unitRows;
  unit_conv$toRemove <- unit_conv$toRemove;
  unit_conv$data <- tbl;
  unit_conv$units <- wh;
  unit_conv$window <- window_size;
  tbl_json <- jsonlite::toJSON(unit_conv, auto_unbox = TRUE);

  tmp_html <- tempfile(fileext = ".html")
  html_lines <- readLines(system.file(package="tma", paste0("apps/viewer.html")));
  html_lines[grepl("//_ENA_MODEL_//", x = html_lines)] <- paste0("data = ", tbl_json, ";");
  writeLines(text = html_lines, con = tmp_html);
  
  if(in_browser == TRUE) {
    browseURL(tmp_html);
  }
  else {
    rstudioapi::viewer(tmp_html)
  }
}