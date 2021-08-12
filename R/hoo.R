# Set units on an object
#
# @param x TBD
# @param by TBD 
#
# @return TBD
units <- function(x, by) {
  model <- x;
  if ( is.data.frame(x) ) {
    model <- list(
      model = list (
        model.type = "TMA",
        raw.input = data.table::as.data.table( x ),
        row.connection.counts = as.data.frame(matrix(nrow = nrow( x ), ncol = 0))
      )
    )
    class(model) <- c("ena.set", class(model))
  }
  
  model$model$raw.input$QEID <- seq(nrow(model$model$raw.input));
  data.table::setkeyv(x = model$model$raw.input, cols = c("QEID"));
  
  # raw_input <- model$model$raw.input;
  model$`_function.params`$units.by <- by;
  
  all_units <- model$model$raw.input[, ..by];
  unique_units <- unique(all_units);
  model$`_function.params`$units <- all_units;
  
  unique_unit_labels <- apply(unique_units, 1, function(y) paste(y, collapse = "::"));

  # model$model$contexts <- structure(rep(list(model$model$raw.input[0,]), length(unique_unit_labels)), names = unique_unit_labels);
  model$model$contexts <- structure(rep(list(c()), length(unique_unit_labels)), names = unique_unit_labels);
  model$model$unit.labels <- unique_unit_labels;
  model$model$raw.input$QEUNIT <- apply(all_units, 1, function(y) paste(y, collapse = "::"));
  
  return(model);
}

# Title
#
# @param x TBD
# @param ... TBD
# @param rule TBD
#
# @return TBD
hoo <- function(x, ..., rule = NULL) {
  units <- x$`_function.params`$units;
  unit_contexts <- x$model$contexts;

  hoo_args <- substitute(...);
  if( is.null(hoo_args) ) {
    hoo_args <- rule;
  }

  raw_input <- x$model$raw.input;
  # apply(unique(units), 1, function(this_unit) {
  #   unit_label <- paste(this_unit, collapse = "::");
  #   # this_unit_rows <- raw_input[as.list(this_unit), , on = names(this_unit)];
  #   # UNIT <- sapply(this_unit_rows, unique);
  #   UNIT <- raw_input[as.list(this_unit), , on = names(this_unit)];
  # 
  #   this_context <- unit_contexts[[unit_label]];
  #   new_context <- unique(c(this_context, raw_input[eval(hoo_args), , which = TRUE]));
  # 
  #   unit_contexts[[unit_label]] <<- new_context; #@Q[!duplicated(new_context$QEID),];
  #   # data.table::setorder(unit_contexts[[unit_label]], "QEID");
  # });
  
  unique_units <- unique(units);
  updated_contexts <- lapply(seq(nrow(unique_units)), function(i) {
    this_unit <- unique_units[i,];
    unit_label <- paste(this_unit, collapse = "::");
    # UNIT <- raw_input[as.list(this_unit), , on = names(this_unit)];
    UNIT <- mapply(raw_input[as.list(this_unit), , on = names(this_unit)], FUN = unique, SIMPLIFY = FALSE);

    this_context <- unit_contexts[[unit_label]];
    ret <- sort(unique(c(this_context, raw_input[eval(hoo_args), , which = TRUE])));
    attr(x = ret, which = "tma.unit") <- this_unit;
    ret
  });
  names(updated_contexts) <- names(unit_contexts)
  
  x$model$contexts <- updated_contexts; #unit_contexts; 
  
  x
}

# Title
#
# @param ... TBD
#
# @return TBD
rules <- function(...) {
  rlang::exprs(...)
}

#' Conversation rules
#'
#' @param ... list of rules
#'
#' @return callable expressions, see `rlang::exprs`
#' @export
conversation_rules <- function(...) {
  rlang::exprs(...)
}

# Title
#
# @param x TBD
# @param units TBD
# @param hoo_rules TBD
# @param split_rules TBD
#
# @return TBD
contexts <- function(x, hoo_rules, units = NULL, split_rules = NULL) {
  if(!is.null( units )) {
    x_ <- units(x, units);
  }
  else {
    x_ <- x;
  }
  
  for(r in hoo_rules) {
    x_ <- hoo(x_, rule = r);
  }

  raw_input <- x_$model$raw.input;
  split_expr_valid <- TRUE;
  err_found <- 0;
  tryCatch(expr = is.null(split_rules), error = function(e) {
      err_found <<- 1;
      print(e$message)
  }, warning = function(w) { 
      err_found <<- 2;
  }, finally = {
      if(err_found != 0) {
        split_expr_valid <- FALSE;
      }
  })
  
  if(split_expr_valid) {
    split_rules_expr <- rlang::enexpr(split_rules);
    
    if( !is.null(split_rules_expr) ) {
      unit_contexts <- x_$model$contexts;
      model_units <- x_$`_function.params`$units;
  
      if ( is.function (split_rules) ) {
          # browser()
        # stop("THIS NEEDS UPDATING (2)")
        unique_units <- unique(model_units);
        unit_contexts_res <- apply(unique_units, 1, function(u) {
          unit_label <- paste(u, collapse = "::");
          # browser()
          res <- do.call(what = split_rules, args = list(unit = u, raw_input[unit_contexts[[unit_label]]]));
          attr(res, "tma.unit") <- attr(unit_contexts[[unit_label]], "tma.unit");
          res <- lapply(res, function(r) {
            attr(r, "tma.unit") <- attr(res, "tma.unit");
            r
          });
          res
        });
        names(unit_contexts_res) <- apply(unique_units, 1, paste, collapse = "::"); #names(unit_contexts);
        unit_contexts <- unit_contexts_res;
      }
      else {
        if( !inherits(x = split_rules_expr, what = "call") ) {
          for(i in seq(split_rules_expr)) {
            unit_contexts <- lapply(unit_contexts, function(uc) {
              # split(x = raw_input[uc,], by = as.character(split_rules_expr))
              ret <- lapply(split(x = raw_input[uc,], by = as.character(split_rules_expr)), function(x) {
                 ret2 <- x[["QEID"]]
                 attr(x = ret2, which = "tma.unit") <- attr(x = uc, "tma.unit");
                 ret2
              })
            })
          }
        }
        else {
          for( i in seq(split_rules) ) {
            if ( is.name(split_rules[[i]]) ){
              # unit_contexts <- lapply(unit_contexts, split, by = as.character(split_rules[[i]]))
              unit_contexts <- lapply(unit_contexts, function(uc) {
                split(x = raw_input[uc,], by = as.character(split_rules[[i]]))
              })
            }
            else {
              stop("THIS NEEDS UPDATING (1)")
              units_to_split <- model_units[eval(split_rules[[i]][[2]]), , which = TRUE]
              unit_contexts[units_to_split] <- lapply(unit_contexts[units_to_split], split, by = names(split_rules[i]))
            }
          }
        }
      }
      
      x_$model$contexts <- unit_contexts;
    }
  }
  
  return(x_);
}