#' Set units on an object
#'
#' @param x 
#' @param by 
#'
#' @return
#' @export
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
  
  raw_input <- model$model$raw.input;
  model$`_function.params`$units.by <- by;
  
  all_units <- raw_input[, ..by];
  unique_units <- unique(all_units);
  model$`_function.params`$units <- all_units;
  
  unique_unit_labels <- apply(unique_units, 1, function(y) paste(y, collapse = "::"));

  # attr(x, "tma_units_by") <- by;
  # attr(x, "tma_units") <- unique_units;
  # attr(x, "tma_contexts") <- structure(rep(list(x[0,]), length(unique_unit_labels)), names = unique_unit_labels);

  model$model$contexts <- structure(rep(list(raw_input[0,]), length(unique_unit_labels)), names = unique_unit_labels);
  model$model$unit.labels <- unique_unit_labels;
  
  return(model);
}

#' Title
#'
#' @param x 
#' @param ... 
#' @param rule 
#'
#' @return
#' @export
hoo <- function(x, ..., rule = NULL) {
  units <- x$`_function.params`$units #attr(x, which = "tma_units");
  unit_contexts <- x$model$contexts; #attr(x, which = "tma_contexts");

  hoo_args <- substitute(...);
  if( is.null(hoo_args) ) {
    hoo_args <- rule;
  }

  raw_input <- x$model$raw.input;
  apply(units, 1, function(this_unit) {
    unit_label <- paste(this_unit, collapse = "::");
    this_unit_rows <- raw_input[as.list(this_unit), , on = names(this_unit)];
    UNIT <- lapply(this_unit_rows, unique);

    this_context <- unit_contexts[[unit_label]];
    new_context <- rbind(this_context, raw_input[eval(hoo_args), ]);

    unit_contexts[[unit_label]] <<- new_context[!duplicated(new_context$ID),];
    data.table::setorder(unit_contexts[[unit_label]], "ID");
    # }
  });
  
  # attr(x, "tma_contexts") <- unit_contexts;
  x$model$contexts <- unit_contexts; 
  
  x
}

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
rules <- function(...) {
  rlang::exprs(...)
}

#' Title
#'
#' @param x 
#' @param units 
#' @param hoo_rules 
#' @param split_rules 
#'
#' @return
#' @export
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
  
  raw_input <- x$model$raw.input;
  split_rules_expr <- rlang::enexpr(split_rules);
  if( !is.null(split_rules_expr) ) {
    unit_contexts <- x_$model$contexts;
    units <- x_$`_function.params`$units;

    if( !inherits(x = split_rules_expr, what = "call") ) {
      for(i in seq(split_rules_expr)) {
        unit_contexts <- lapply(unit_contexts, split, by = as.character(split_rules_expr))
      }
    }
    else {
      if(!is.function(split_rules)) {
        for( i in seq(split_rules) ) {
          if ( is.name(split_rules[[i]]) ){
            unit_contexts <- lapply(unit_contexts, split, by = as.character(split_rules[[i]]))
          }
          else {
            units_to_split <- units[eval(split_rules[[i]][[2]]), , which = TRUE]
            unit_contexts[units_to_split] <- lapply(unit_contexts[units_to_split], split, by = names(split_rules[i]))
          }
        }
      }
      else {
        unit_contexts_res <- apply(units, 1, function(u) {
          unit_label <- paste(u, collapse = "::");
          unit_label <- do.call(what = split_rules, args = list(unit = u, unit_contexts[[unit_label]]));
        });
        names(unit_contexts_res) <- names(unit_contexts);
        unit_contexts <- unit_contexts_res;
      }
    }
    
    x_$model$contexts <- unit_contexts;
  }
  
  return(x_);
}