#' Stage assignment based on SICR rule
#'
#' Simple rule: Stage 3 if credit_impaired TRUE; else Stage 2 if
#' relative increase in lifetime PD >= threshold_ratio OR absolute increase >= threshold_abs.
#' Otherwise Stage 1.
#' @param lifetime_pd_orig scalar lifetime PD at origination (cum at horizon)
#' @param lifetime_pd_curr scalar lifetime PD now (cum at same horizon)
#' @param credit_impaired logical
#' @param threshold_ratio numeric, e.g., 2 means 2x increase
#' @param threshold_abs numeric, absolute increase in points (e.g., 0.1 for +10pp)
#' @return integer stage 1, 2, or 3
#' @export
stage_from_sicr <- function(lifetime_pd_orig, lifetime_pd_curr,
                            credit_impaired = FALSE,
                            threshold_ratio = 2, threshold_abs = 0.1) {
  if (isTRUE(credit_impaired)) return(3L)
  ratio <- ifelse(lifetime_pd_orig > 0, lifetime_pd_curr / lifetime_pd_orig, Inf)
  abs_increase <- lifetime_pd_curr - lifetime_pd_orig
  if (is.finite(ratio) && ratio >= threshold_ratio) return(2L)
  if (abs_increase >= threshold_abs) return(2L)
  1L
}

#' Stage-aware ECL
#'
#' If stage == 1, computes 12-month ECL (or specified short horizon).
#' If stage in {2,3}, computes lifetime ECL.
#' @param cum_pd numeric vector cumulative PD over full lifetime
#' @param ead numeric vector over full lifetime
#' @param lgd numeric vector over full lifetime
#' @param df numeric vector over full lifetime
#' @param stage integer 1,2,3
#' @param short_horizon integer number of periods to approximate 12-month (default 12)
#' @return scalar ECL
#' @export
ecl_staged <- function(cum_pd, ead, lgd, df, stage, short_horizon = 12) {
  stopifnot(stage %in% c(1L, 2L, 3L))
  if (stage == 1L) {
    h <- min(short_horizon, length(cum_pd))
    return(ecl_from_cum_pd(cum_pd[seq_len(h)], ead[seq_len(h)], lgd[seq_len(h)], df[seq_len(h)]))
  } else {
    return(ecl_from_cum_pd(cum_pd, ead, lgd, df))
  }
}
