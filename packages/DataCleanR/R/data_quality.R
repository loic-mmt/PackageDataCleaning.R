#' @title Validate value ranges
#'
#' @description
#' Check that the data respect basic business rules and **remove** invalid rows.
#' The rules include: strictly positive salary, remote work ratio between 0 and 100, and a plausible work year.
#'
#' @details
#' This function applies a strict filter. A row is kept only if all of the following conditions are TRUE:
#' \itemize{
#'   \item `salary` > 0
#'   \item `remote_ratio` is between 0 and 100 inclusive
#'   \item `work_year` is between `min_year` and `max_year`
#'   \item none of these values is `NA`
#' }
#' A message is printed to the console if some rows are removed.
#'
#' @param data A `data.frame` containing the `salary`, `remote_ratio` and `work_year` columns.
#' @param min_year Integer. Minimum acceptable year (default 2000).
#' @param max_year Integer. Maximum acceptable year (default: current system year).
#'
#' @return A filtered `data.frame` (the number of rows may be lower than the original).
#'
#' @family Data quality
#' @examples
#' # Data with an invalid row (negative salary and remote_ratio > 100)
#' df <- data.frame(
#'   salary = c(50000, -100, 60000),
#'   remote_ratio = c(50, 150, 100),
#'   work_year = c(2022, 2022, 2023)
#' )
#'
#' # Apply the filter
#' df_clean <- validate_ranges(df)
#'
#' # Result: only valid rows are kept
#' nrow(df_clean) # Should be 2
#' @export

validate_ranges <- function(data, min_year = 2000, max_year = as.integer(format(Sys.Date(), "%Y"))) {
  if (!all(c("salary", "remote_ratio", "work_year") %in% names(data))) {
    stop("Les colonnes 'salary', 'remote_ratio' et 'work_year' doivent exister.")
  }

  data$salary <- as.numeric(data$salary)
  data$remote_ratio <- as.numeric(data$remote_ratio)
  data$work_year <- as.integer(data$work_year)

  initial_rows <- nrow(data)

  # Filtrer les lignes valides
  valid_data <- data[
    data$salary > 0 &
      data$remote_ratio >= 0 &
      data$remote_ratio <= 100 &
      data$work_year >= min_year &
      data$work_year <= max_year &
      !is.na(data$salary) &
      !is.na(data$remote_ratio) &
      !is.na(data$work_year),
  ]

  removed_rows <- initial_rows - nrow(valid_data)

  if (removed_rows > 0) {
    message(sprintf("%d ligne(s) supprimée(s) car hors des plages valides.", removed_rows))
  }

  return(valid_data)
}



#' Cap salary outliers (quantile)
#'
#' @description
#' Handle extreme values (outliers) in a numeric column by replacing them with
#' specified quantile cut-offs. This is a simple "winsorisation" strategy.
#'
#' @details
#' The function computes two bounds, L (lower) and U (upper), corresponding to the
#' requested quantile probabilities. Values below L are replaced by L, and values
#' above U are replaced by U. You can choose whether to cap both tails, only the
#' upper tail, or only the lower tail.
#'
#' @param data A `data.frame` containing the data.
#' @param col Character string. Name of the column to process (default "salary_in_usd").
#' @param lower Probability for the lower quantile (between 0 and 1, default 0.01).
#' @param upper Probability for the upper quantile (between 0 and 1, default 0.99).
#' @param clip_side Capping strategy: "both" (default), "upper" or "lower".
#' @param na_rm Logical. Should `NA` values be ignored when computing quantiles? (default TRUE)
#' @param verbose Logical. If TRUE, prints a summary of the replacements performed.
#'
#' @return The `data.frame` with the target column modified (extreme values are
#'   replaced by the corresponding quantile bounds).
#'
#' @family Data quality
#' @examples
#' # Simple data with an extreme salary value (1 million)
#' df <- data.frame(
#'   salary_in_usd = c(50000, 55000, 60000, 45000, 1000000)
#' )
#'
#' # Cap the upper 10% (winsorisation)
#' df_capped <- cap_outliers_salary(df, lower = 0.1, upper = 0.9, clip_side = "upper")
#'
#' # The value 1,000,000 is replaced by the 90th percentile
#' print(df_capped)
#' @export
cap_outliers_salary <- function(data,
                                col = "salary_in_usd",
                                lower = 0.01,
                                upper = 0.99,
                                clip_side = c("both", "upper", "lower"),
                                na_rm = TRUE,
                                verbose = TRUE) {
  if (!is.data.frame(data)) stop("data doit être un data.frame")
  if (!col %in% names(data)) stop(sprintf("Colonne '%s' introuvable", col))
  if (!is.numeric(data[[col]])) stop(sprintf("'%s' doit être numérique", col))

  clip_side <- match.arg(clip_side)

  caps_quantile <- function(x) {
    L <- as.numeric(stats::quantile(x, probs = lower, na.rm = na_rm, type = 7))
    U <- as.numeric(stats::quantile(x, probs = upper, na.rm = na_rm, type = 7))
    c(lower = L, upper = U)
  }
  apply_caps <- function(x, caps) {
    L <- caps[["lower"]]; U <- caps[["upper"]]
    xi <- x
    idx_low <- !is.na(xi) & xi < L
    idx_up  <- !is.na(xi) & xi > U
    if (clip_side %in% c("both","lower")) xi[idx_low] <- L
    if (clip_side %in% c("both","upper")) xi[idx_up]  <- U
    list(x = xi, n_low = sum(idx_low), n_up = sum(idx_up))
  }

  x <- data[[col]]
  caps <- caps_quantile(x)
  res  <- apply_caps(x, caps)
  data[[col]] <- res$x

  if (verbose) message(sprintf(
    "cap_outliers_salary: %d bas / %d haut  (caps [%g, %g])",
    res$n_low, res$n_up, caps[["lower"]], caps[["upper"]]))

  data
}
