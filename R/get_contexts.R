#' Get Contexts for each unit in x
#'
#' @param x data.frame or data.table
#' @param units_by character vector
#' @param rules list
#' @param indices.only boolean If TRUE, default, returns row indices for each unit, otherwise will return subset tables
#'
#' @return List, named by each unique unit, containing either row indices (indicies.only = TRUE) 
#' or subset data.tables of x (indices.only = FALSE)
get_contexts <- function(x, units_by, rules, indices.only = TRUE) {
  # Convert to data.table if needed
  if(!inherits(x, "data.table")) {
    x <- data.table(x, stringsAsFactors = FALSE)
  }
  
  # Find the unique units in the set
  unique_units <- unique(x[, ..units_by]);
  
  # Match the contexts
  conversations <- NULL;
  context_rows <- sapply(seq(nrow(unique_units)), function(.GRP) {
    
    # Rows in context b/c of the unit matched  
    unit_rows <- rowSums(sapply(units_by, function(u) {
      x[[u]] %chin% unique_units[[u]][.GRP]
    })) == length(units_by)
    
    # Find rows that fit Horizon of Observation rules
    hoo_rows <- rowSums(sapply(rules, function(rule) {
      
      # Apply each part of the rule
      if( length(rule) == 1 && names(rule) == "Split.on" ) {
        conversations <<- rule[["Split.on"]]
        rep(FALSE, nrow(x))
      }
      else {
        rules_matched <- sapply(names(rule), function(u) {
          if( 
            (is.logical(rule[[u]]) && rule[[u]] == TRUE) || 
            (rule[[u]] == "<UNIT>")
          ) {
            x[[u]] %chin% unique(x[[u]][unit_rows])
          }
          else {
            x[[u]] %chin% rule[[u]]
          } 
        })
        rowSums(rules_matched) == length(names(rule))
      }
    }))
    
    # Rows that either that of the unit or in the HoO
    # converted to row indices
    # passed_rows <- which(unit_rows | hoo_rows)
    passed_rows <- which(hoo_rows > 0)
    
    # Don't worry about rows after the unit's last row
    passed_rows #[passed_rows <= max(which(unit_rows))]
  }, simplify = FALSE);
  names(context_rows) <- apply(unique_units, 1, paste, collapse = "::");
  
  if(indices.only) {
    if(!is.null(conversations)) {
      lapply(context_rows, function(context) {
          split(context, unlist(x[context, ..conversations]))
      })
    }
    else {
      # context_rows
      sapply(names(context_rows), function(x) list(context_rows[x]))
      # context_rows_new <- lapply(seq(context_rows), function(x) list(context_rows[[x]]))
      # names(context_rows_new) <- names(context_rows)
      # context_rows_new
    }
  }
  else {
    unit_contexts <- apply(unique_units, 1, function(u) {
      to_ret <- list(
        "context" = x[context_rows[[paste(u, collapse = "::")]], ],
        "unit" = data.frame(as.list(u))
      )
      if(!is.null(conversations)) {
        to_ret[["context"]] <- split(x = to_ret[["context"]], by = conversations)
      }
      else {
        to_ret[["context"]] <- list(to_ret[["context"]])
      }
      to_ret
    });
    names(unit_contexts) <- names(context_rows);
    list(
      "contexts" = unit_contexts,
      "units" = unique_units,
      "raw.data" = x
    )
  }
}