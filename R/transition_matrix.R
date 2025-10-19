#' Validate a transition matrix
#'
#' Checks square-ness, non-negativity, row sums equal 1, and absorbing default.
#' @param P numeric matrix, row-stochastic (rows sum to 1)
#' @param states optional character vector of state names (length nrow(P))
#' @param default_state character, name of absorbing default state (must be in states or column/row name of P)
#' @param tol numeric, tolerance for row-sum checks
#' @return Invisible TRUE if valid; otherwise errors.
#' @export
tm_validate <- function(P, states = NULL, default_state = "Default", tol = 1e-8) {
  if (!is.matrix(P)) stop("P must be a matrix.")
  if (!is.numeric(P)) stop("P must be numeric.")
  if (nrow(P) != ncol(P)) stop("P must be square.")
  if (any(P < -tol)) stop("P must have non-negative entries.")
  rs <- rowSums(P)
  if (any(abs(rs - 1) > 1e-8)) stop("Each row of P must sum to 1 (within tolerance).")
  # Resolve state names
  if (is.null(states)) {
    if (!is.null(rownames(P))) states <- rownames(P)
    else states <- paste0("S", seq_len(nrow(P)))
  } else {
    if (length(states) != nrow(P)) stop("states length must match nrow(P).")
  }
  # Find default index
  idx_def <- which(states == default_state)
  if (length(idx_def) == 0 && !is.null(colnames(P))) idx_def <- which(colnames(P) == default_state)
  if (length(idx_def) == 0) stop("default_state not found in provided states or dimnames.")
  # Check absorbing default row (only default row must be absorbing)
  row_def <- P[idx_def, , drop = TRUE]
  target <- rep(0, ncol(P)); target[idx_def] <- 1
  if (any(abs(row_def - target) > 1e-8)) {
    stop("Default row must be absorbing: 1 on default column, 0 elsewhere.")
  }
  invisible(TRUE)
}

#' K-step transition matrix
#' @param P transition matrix
#' @param k positive integer number of steps
#' @return P^k
#' @export
tm_k_step <- function(P, k) {
  if (!is.matrix(P)) stop("P must be a matrix.")
  k <- as.integer(k)
  if (k < 1) stop("k must be >= 1")
  out <- diag(nrow(P))
  for (i in seq_len(k)) {
    out <- out %*% P
  }
  out
}

#' Example transition matrix (monthly)
#'
#' States: "A", "B", "C", "Default". Default is absorbing.
#' @return A 4x4 numeric matrix with dimnames.
#' @export
example_transition_matrix <- function() {
  P <- matrix(c(
    # To:   A      B      C      Default
           0.94,  0.04,  0.01,  0.01,   # From A
           0.02,  0.92,  0.04,  0.02,   # From B
           0.01,  0.05,  0.91,  0.03,   # From C
           0.00,  0.00,  0.00,  1.00    # From Default (absorbing)
  ), nrow = 4, byrow = TRUE)
  dimnames(P) <- list(c("A", "B", "C", "Default"), c("A", "B", "C", "Default"))
  P
}

#' Lifetime PD from an initial state via transition matrix
#'
#' Computes cumulative PD over t=1..h for a given starting non-default state.
#' @param P transition matrix (rows stochastic). Must be validated or valid.
#' @param state character, starting state name (e.g., "A", "B", "C")
#' @param horizon integer number of periods
#' @param default_state character name of absorbing default state
#' @return numeric vector of length horizon: cumulative PD by period
#' @export
lifetime_pd_from_state <- function(P, state, horizon, default_state = "Default") {
  if (horizon < 1) stop("horizon must be >= 1")
  states <- if (!is.null(rownames(P))) rownames(P) else paste0("S", seq_len(nrow(P)))
  if (!(state %in% states)) stop("state not found in P rownames.")
  if (!(default_state %in% states)) stop("default_state not found in P rownames.")
  tm_validate(P, states = states, default_state = default_state)
  idx_state <- match(state, states)
  idx_def <- match(default_state, states)
  p <- rep(0, length(states))
  p[idx_state] <- 1
  cum <- numeric(horizon)
  for (t in seq_len(horizon)) {
    p <- as.numeric(p %*% P)
    cum[t] <- p[idx_def]
  }
  cum
}

#' Convert cumulative PD to marginal PD
#' @param cum_pd numeric vector of cumulative default probabilities (non-decreasing)
#' @return numeric vector of marginal PD per period
#' @export
marginal_pd_from_cum <- function(cum_pd) {
  if (any(diff(cum_pd) < -1e-10)) stop("cum_pd must be non-decreasing.")
  prev <- c(0, head(cum_pd, -1))
  pmax(cum_pd - prev, 0)
}
