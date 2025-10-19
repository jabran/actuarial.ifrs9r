#' Discount factors from effective annual rate
#' @param annual_rate effective annual discount rate (e.g., 0.05 for 5%)
#' @param periods_per_year integer, e.g., 12 for monthly
#' @param horizon number of periods
#' @return numeric vector of discount factors DF_t = 1/(1+i)^t
#' @export
discount_factors <- function(annual_rate, periods_per_year, horizon) {
  if (periods_per_year < 1) stop("periods_per_year must be >= 1")
  i <- (1 + annual_rate)^(1 / periods_per_year) - 1
  1 / (1 + i)^(seq_len(horizon))
}

#' EAD schedule
#' @param horizon number of periods
#' @param method "flat" or "annuity"
#' @param principal starting principal for "annuity"
#' @param rate_per_period per-period interest rate for "annuity"
#' @param custom optional numeric vector of EADs (overrides other args if provided)
#' @return numeric vector length horizon
#' @export
ead_schedule <- function(horizon, method = c("flat", "annuity"),
                         principal = NULL, rate_per_period = NULL,
                         custom = NULL) {
  if (!is.null(custom)) {
    if (length(custom) != horizon) stop("custom EAD length must match horizon.")
    return(as.numeric(custom))
  }
  method <- match.arg(method)
  if (method == "flat") {
    if (is.null(principal)) stop("Provide principal for flat EAD.")
    return(rep(principal, horizon))
  } else {
    if (is.null(principal) || is.null(rate_per_period)) {
      stop("Provide principal and rate_per_period for annuity.")
    }
    n <- horizon
    if (rate_per_period == 0) {
      payment <- principal / n
    } else {
      payment <- principal * (rate_per_period) / (1 - (1 + rate_per_period)^(-n))
    }
    bal <- principal
    ead <- numeric(n)
    for (t in seq_len(n)) {
      interest <- bal * rate_per_period
      principal_pay <- payment - interest
      bal <- max(bal - principal_pay, 0)
      ead[t] <- bal + interest # exposure at period-end before payment next period
    }
    ead
  }
}

#' LGD curve utility
#' @param horizon number of periods
#' @param constant optional scalar LGD, if provided returns vector of that value
#' @param custom optional numeric vector of LGD values (0..1)
#' @return numeric vector length horizon
#' @export
lgd_curve <- function(horizon, constant = 0.45, custom = NULL) {
  if (!is.null(custom)) {
    if (length(custom) != horizon) stop("custom LGD length must match horizon.")
    if (any(custom < 0 | custom > 1)) stop("LGD values must be between 0 and 1.")
    return(as.numeric(custom))
  }
  rep(constant, horizon)
}
