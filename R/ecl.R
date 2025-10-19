#' Compute ECL from marginal PD, EAD, LGD, and discount factors
#' @param marginal_pd numeric vector
#' @param ead numeric vector
#' @param lgd numeric vector
#' @param df numeric vector
#' @return scalar ECL
#' @export
compute_ecl <- function(marginal_pd, ead, lgd, df) {
  if (!all(lengths(list(marginal_pd, ead, lgd, df)) == length(marginal_pd))) {
    stop("All input vectors must have equal length.")
  }
  sum(marginal_pd * ead * lgd * df)
}

#' Compute ECL from cumulative PD
#' @param cum_pd numeric vector cumulative PD
#' @param ead numeric vector
#' @param lgd numeric vector
#' @param df numeric vector
#' @return scalar ECL
#' @export
ecl_from_cum_pd <- function(cum_pd, ead, lgd, df) {
  compute_ecl(marginal_pd_from_cum(cum_pd), ead, lgd, df)
}

#' Weighted ECL across macro scenarios from cumulative PD curves
#' @param cum_pd_list list of numeric vectors (same length)
#' @param weights numeric vector summing to 1
#' @param ead numeric vector
#' @param lgd numeric vector
#' @param df numeric vector
#' @return scalar expected ECL
#' @export
weighted_ecl_from_cum_pd <- function(cum_pd_list, weights, ead, lgd, df) {
  if (!is.list(cum_pd_list) || length(cum_pd_list) < 1) stop("Provide a non-empty list of cumulative PD vectors.")
  n <- length(cum_pd_list[[1]])
  if (any(vapply(cum_pd_list, length, integer(1)) != n)) stop("All cumulative PD vectors must have same length.")
  if (length(weights) != length(cum_pd_list)) stop("weights length must match number of scenarios.")
  if (abs(sum(weights) - 1) > 1e-8) stop("Scenario weights must sum to 1.")
  ecls <- vapply(cum_pd_list, function(cp) ecl_from_cum_pd(cp, ead, lgd, df), numeric(1))
  sum(ecls * weights)
}
