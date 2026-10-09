#' @title Transmodal Analysis (TMA)
#' @description
#' TMA is a computational framework for analyzing complex multimodal data. It
#' extends state-dependent models such as Epistemic Network Analysis (ENA) to
#' account for diverse data streams, addressing challenges such as varying
#' temporal scales and learner characteristics to improve the robustness and
#' interpretability of findings.
#'
#' The package imports, preprocesses, and accumulates coded data across
#' modalities with granular control over windows and weights, producing
#' connection structures that downstream packages (e.g. rENA, ONA) can model and
#' visualize.
#'
#' For methodological details, see Shaffer, Wang, and Ruis (2025), "Transmodal
#' Analysis," \doi{10.18608/jla.2025.8423}.
#' @name tma
#' 
#' @import stats
#' @import utils
#' @import data.table
#' @importFrom methods is
#' @importFrom rlang exprs
#' @importFrom libqe connection_indices
#' @importFrom Rcpp sourceCpp
#' @useDynLib tma, .registration = TRUE
#' @importFrom libqe door_lookback_block door_ema_block
#' @importFrom libqe fit_trajectory_poly eval_trajectory_curve eval_trajectory_derivatives
#' @importFrom libqe integrated_trajectory_distance lagged_trajectory_distance
#' @importFrom libqe signed_turn_lag sweep_signed_turn_lags dist_dist_correlation
NULL