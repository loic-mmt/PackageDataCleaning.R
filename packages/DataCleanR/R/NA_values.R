#' Impute missing values for numeric and categorical columns
#'
#' @description
#' Impute missing values (`NA`) in a `data.frame` column by column,
#' using simple, independent strategies (no complex external dependencies).
#'
#' @details
#' The strategy applied depends on the column type:
#'
#' * **Numeric** (`numeric`, `integer`):
#'   * `"median"` (default): Replace by the median (robust to outliers).
#'   * `"mean"`: Replace by the mean.
#'   * `"constant"`: Replace by the value supplied in `num_constant` (e.g. 0).
#'
#' * **Categorical** (`character`, `factor`):
#'   * `"mode"` (default): Replace by the most frequent value. In case of ties, alphabetical order is used.
#'   * `"constant"` or `"new_level"`: Replace by the value `cat_constant` (default `"Missing"`).
#'   * *Note*: If the column is a factor, the levels are automatically updated to include the new value if needed.
#'
#' * **Logical** (`logical`): Replace by the majority value (`TRUE` or `FALSE`).
#'
#' @param data The `data.frame` containing missing values.
#' @param cols Optional character vector of column names to process. If `NULL`
#'   (default), all columns are processed.
#' @param exclude Optional character vector of column names to exclude from processing.
#' @param num_method Method for numeric columns: `"median"`, `"mean"` or `"constant"`.
#' @param cat_method Method for character/factor columns: `"mode"`, `"constant"` or `"new_level"`.
#' @param num_constant Value used if `num_method = "constant"` (default `0`).
#' @param cat_constant Value used if `cat_method = "constant"` (default `"Missing"`).
#' @param verbose Logical. If `TRUE` (default), prints a message in the console
#'   for each imputed column.
#'
#' @return The `data.frame` with missing values imputed.
#'
#' @family Missing values handling
#' @examples
#' df <- data.frame(
#'   val_num = c(10, 20, NA, 40, 100),
#'   val_cat = c("A", "B", NA, "A", "A"),
#'   stringsAsFactors = TRUE
#' )
#'
#' # Imputation: median for numeric columns, mode for factors
#' df_imp <- impute_missing(df)
#'
#' # Check result
#' print(df_imp)
#' @export
impute_missing <- function(data,
                           cols = NULL,
                           exclude = NULL,
                           num_method = c("median", "mean", "constant"),
                           cat_method = c("mode", "constant", "new_level"),
                           num_constant = 0,
                           cat_constant = "Missing",
                           verbose = TRUE) {
  if (!is.data.frame(data)) stop("data must be a data.frame")

  num_method <- match.arg(num_method)
  cat_method <- match.arg(cat_method)

  # mode (on character/factor); in case of ties, use alphabetical order
  mode_char <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_character_)
    tb <- sort(table(x), decreasing = TRUE)
    max_count <- tb[1]
    candidates <- names(tb)[tb == max_count]
    sort(candidates)[1]
  }

  cols_all <- names(data)
  cols_use <- if (is.null(cols)) cols_all else intersect(cols, cols_all)
  if (!is.null(exclude)) cols_use <- setdiff(cols_use, exclude)
  if (!length(cols_use)) return(data)

  for (nm in cols_use) {
    x <- data[[nm]]
    n_na <- sum(is.na(x))
    if (!n_na) {
      if (verbose) message(sprintf("impute_missing: '%s' -> 0 NA", nm))
      next
    }

    # Numeric
    if (is.numeric(x)) {
      repl <- switch(
        num_method,
        median   = if (sum(!is.na(x)) > 0) stats::median(x, na.rm = TRUE)  else num_constant,
        mean     = if (sum(!is.na(x)) > 0) base::mean(x, na.rm = TRUE)   else num_constant,
        constant = num_constant
      )
      x[is.na(x)] <- repl
      data[[nm]] <- x
      if (verbose) message(sprintf("impute_missing: '%s' (numeric) -> %d NA imputed (%s%s%g)",
                                   nm, n_na, num_method,
                                   if (num_method == "constant") "=" else ": ", repl))
      next
    }

    # Logical -> majority
    if (is.logical(x)) {
      if (sum(!is.na(x)) == 0) {
        repl_log <- FALSE
      } else {
        count_true  <- sum(x, na.rm = TRUE)
        count_false <- sum(!x, na.rm = TRUE)
        repl_log <- count_true >= count_false
      }
      x[is.na(x)] <- repl_log
      data[[nm]] <- x
      if (verbose) message(sprintf("impute_missing: '%s' (logical) -> %d NA imputed (%s)", nm, n_na, repl_log))
      next
    }

    # Character / Factor -> categorical
    is_fac <- is.factor(x)
    lev <- if (is_fac) levels(x) else NULL
    x_chr <- as.character(x)

    repl_chr <- switch(
      cat_method,
      mode = {
        m <- mode_char(x_chr)
        if (is.na(m)) cat_constant else m
      },
      constant  = cat_constant,
      new_level = cat_constant
    )

    x_chr[is.na(x_chr)] <- repl_chr

    if (is_fac) {
      new_levels <- lev
      if (!(repl_chr %in% new_levels)) new_levels <- c(new_levels, repl_chr)
      data[[nm]] <- factor(x_chr, levels = new_levels)
    } else {
      data[[nm]] <- x_chr
    }

    if (verbose) message(sprintf("impute_missing: '%s' (categorical) -> %d NA imputed (%s%s%s)",
                                 nm, n_na, cat_method,
                                 if (cat_method == "mode") ": " else "=",
                                 repl_chr))
  }

  data
}