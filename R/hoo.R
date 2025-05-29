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
  
  model$model$raw.input[[ATTR_NAMES$CONTEXT_ID]] <- 1;
  model$model$raw.input$QEID <- seq(nrow(model$model$raw.input));
  data.table::setkeyv(x = model$model$raw.input, cols = c("QEID"));
  
  # raw_input <- model$model$raw.input;
  model$`_function.params`$units.by <- by;
  
  all_units <- model$model$raw.input[, ..by];
  unique_units <- unique(all_units);
  model$`_function.params`$units <- all_units;
  
  unique_unit_labels <- apply(unique_units, 1, function(y) paste((y), collapse = "::"));

  # model$model$contexts <- structure(rep(list(model$model$raw.input[0,]), length(unique_unit_labels)), names = unique_unit_labels);
  model$model$contexts <- structure(rep(list(c()), length(unique_unit_labels)), names = unique_unit_labels);
  model$model$unit.labels <- unique_unit_labels;
  model$model$raw.input$QEUNIT <- apply(all_units, 1, function(y) paste((y), collapse = "::"));
  # model$model$raw.input[, QEUNIT:=paste(.SD, collapse = "::"), .SDcols = names(all_units), by=1:nrow(model$model$raw.input)]
  
  return(model);
}

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
    this_unit <- this_unit[, lapply(.SD, reclass, "ena.metadata"), .SDcols = colnames(this_unit)];
    
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


## Possible improvement: 
# rules((Condition & GroupName & UserName)) --->
#   bq = base::bquote(...);
#   bq[[2]][[3]] <- quote(UserName == UNIT$UserName)
#

#' rules
#'
#' @param ... TBD
#'
#' @return TBD
#' @export
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

#' contexts
#'
#' @param x TBD
#' @param units_by TBD
#' @param hoo_rules TBD
#' @param split_rules TBD
#'
#' @return TBD
#' @export
contexts <- function(x, hoo_rules, units_by = NULL, split_rules = NULL) {
  x_ <- units(x, units_by);
  # if(!is.null( units )) {
  # }
  # else {
  #   x_ <- x;
  # }
  
  for(r in hoo_rules) {
    x_ <- hoo(x_, rule = r);
  }

  raw_input <- x_$model$raw.input;
  split_expr_valid <- TRUE;
  err_found <- 0;
  tryCatch(expr = rlang::enexpr(split_rules), error = function(e) {
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
  
      # if ( is.function (split_rules) ) {
      if( is(split_rules, "function") ) {
        unique_units <- unique(model_units);
        unit_contexts_res <- apply(unique_units, 1, function(u) {
          unit_label <- paste(u, collapse = "::");
          res <- do.call(what = split_rules, args = list(unit = u, raw_input[unit_contexts[[unit_label]]]));
          attr(res, "tma.unit") <- attr(unit_contexts[[unit_label]], "tma.unit");
          res2 <- lapply(res, function(r) {
            r[[ATTR_NAMES$CONTEXT_COL_ID]] <- seq.int(nrow(r));
            attr(r, "tma.unit") <- attr(res, "tma.unit");
            attr(r, "tma.unit_rows") <- which(r$QEUNIT == unit_label)
            r
          });
          res2
        });
        names(unit_contexts_res) <- apply(unique_units, 1, paste, collapse = "::"); #names(unit_contexts);
        unit_contexts <- unit_contexts_res;
      }
      else {
        if( !inherits(x = split_rules_expr, what = "call") ) {
          for(i in seq(split_rules_expr)) {
            unit_contexts <- lapply(unit_contexts, function(uc) {
              lapply(split(x = raw_input[uc,], by = as.character(split_rules)), function(x) {
                unit_tbl <- attr(x = uc, "tma.unit");
                unit_label <- paste0(unit_tbl, collapse = "::");
                
                x[[ATTR_NAMES$CONTEXT_COL_ID]] <- seq.int(nrow(x));
                attr(x = x, which = "tma.unit") <- attr(x = uc, "tma.unit");
                attr(x = x, which = "tma.unit_rows") <- which(x$QEUNIT == unit_label);
                x
              });
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
    else {
      unit_contexts <- x_$model$contexts;
      model_units <- x_$`_function.params`$units;
      unique_units <- unique(model_units);
      unit_contexts_res <- apply(unique_units, 1, function(u) {
        # unit_label <- paste(u, collapse = "::");
        unit_label <- paste(trimws(u), collapse = "::");
        res <- raw_input[unit_contexts[[unit_label]]];
        res[[ATTR_NAMES$CONTEXT_COL_ID]] <- seq.int(nrow(res));
        
        attr(res, "tma.unit") <- attr(unit_contexts[[unit_label]], "tma.unit");
        attr(res, "tma.unit_rows") <- which(res$QEUNIT == unit_label);
        res
      });
      names(unit_contexts_res) <- apply(unique_units, 1, paste, collapse = "::"); #names(unit_contexts);
      unit_contexts <- unit_contexts_res;
      x_$model$contexts <- unit_contexts;
    }
  }
  
  # x_list <- list2env(x_);
  # class(x_list) <- c("ena.set", class(x_list));
  # return(x_list);
  x_
}