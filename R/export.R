#' Build a tidy ECL components data frame
#'
#' @param cum_pd numeric vector of cumulative PD
#' @param ead numeric vector of EAD
#' @param lgd numeric vector of LGD
#' @param df numeric vector of discount factors
#' @return data.frame with period, cum_pd, marginal_pd, ead, lgd, df, loss_t, cum_loss
#' @export
ecl_components <- function(cum_pd, ead, lgd, df) {
  stopifnot(length(cum_pd) == length(ead),
            length(ead) == length(lgd),
            length(lgd) == length(df))
  mpd <- marginal_pd_from_cum(cum_pd)
  loss_t <- mpd * ead * lgd * df
  data.frame(
    period = seq_along(cum_pd),
    cum_pd = as.numeric(cum_pd),
    marginal_pd = as.numeric(mpd),
    ead = as.numeric(ead),
    lgd = as.numeric(lgd),
    df = as.numeric(df),
    loss_t = as.numeric(loss_t),
    cum_loss = cumsum(loss_t)
  )
}

#' Export IFRS 9 results to an Excel workbook (.xlsx)
#'
#' Writes multiple sheets: ECL_Components, Transition_Matrix (optional),
#' Scenarios (optional), Stage (optional), Metadata (optional).
#'
#' @param path file path to write (must end with .xlsx)
#' @param components data.frame from ecl_components()
#' @param P optional transition matrix to include as a sheet
#' @param scenarios optional data.frame with columns: scenario, weight, ecl
#' @param stage_info optional named list or data.frame; if list, should include entries 'stage' and 'ecl'
#' @param metadata optional named list to document inputs, run date, currency, etc.
#' @param overwrite logical, default FALSE. If TRUE, overwrite existing file.
#' @return invisibly returns the path
#' @export
export_ifrs9_results_xlsx <- function(path,
                                      components,
                                      P = NULL,
                                      scenarios = NULL,
                                      stage_info = NULL,
                                      metadata = NULL,
                                      overwrite = FALSE) {
  if (!grepl("[.]xlsx$", path, ignore.case = TRUE)) stop("path must end with .xlsx")
  if (file.exists(path) && !overwrite) stop("File exists. Use overwrite=TRUE to replace.")
  if (!is.data.frame(components)) stop("components must be a data.frame, e.g., from ecl_components().")

  sheets <- list(ECL_Components = components)

  if (!is.null(P)) {
    if (!is.matrix(P)) stop("P must be a matrix.")
    dfP <- as.data.frame(P, stringsAsFactors = FALSE)
    dfP <- cbind(from_state = rownames(P) %||% paste0("S", seq_len(nrow(P))), dfP)
    sheets$Transition_Matrix <- dfP
  }

  if (!is.null(scenarios)) {
    if (!is.data.frame(scenarios)) stop("scenarios must be a data.frame.")
    needed <- c("scenario", "weight", "ecl")
    if (!all(needed %in% names(scenarios))) stop("scenarios must have columns: scenario, weight, ecl")
    sheets$Scenarios <- scenarios
  }

  if (!is.null(stage_info)) {
    if (is.list(stage_info) && !is.data.frame(stage_info)) {
      dfStage <- data.frame(
        name = names(stage_info),
        value = unname(unlist(stage_info)),
        stringsAsFactors = FALSE
      )
    } else if (is.data.frame(stage_info)) {
      dfStage <- stage_info
    } else {
      stop("stage_info must be list or data.frame.")
    }
    sheets$Stage <- dfStage
  }

  if (!is.null(metadata)) {
    if (!is.list(metadata)) stop("metadata must be a named list.")
    dfMeta <- data.frame(
      key = names(metadata),
      value = unname(unlist(metadata)),
      stringsAsFactors = FALSE
    )
    sheets$Metadata <- dfMeta
  }

  writexl::write_xlsx(sheets, path = path)
  invisible(path)
}

# Util: null coalescing for rownames
`%||%` <- function(a, b) if (!is.null(a)) a else b
