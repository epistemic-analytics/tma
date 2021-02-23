#' Title
#'
#' @param x TBD
#' @param ... TBD
#'
#' @return TBD
#' @export
threaded_ena <- function(
  data,
  units_by,
  conversation_rules,
  code_cols,
  conversation_splits = NULL,
  as_directed = FALSE,
  window_size = 4,
  meta_data = units_by
) {
  if(!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data);
  }
  
  model <- contexts(
    x = data,
    units = units_by,
    hoo_rules = conversation_rules,
    split_rules = conversation_splits
  );
  model <- accumulate_threads(
    x = model, 
    codes = code_cols,
    as_directed = as_directed,
    window_size = window_size,
    meta.data = meta_data
  );
  
  return(model);
}