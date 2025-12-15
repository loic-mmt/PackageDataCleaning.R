# Helpers

.load_dataframe <- function(data, load_fun, load_args) {
  if (is.data.frame(data)) {
    return(data)
  }
  if (is.character(data) && length(data) == 1) {
    if (!file.exists(data)) stop(sprintf("File not found: %s", data))
    return(do.call(load_fun, c(list(data), load_args)))
  }
  stop("data must be either a data.frame or a file path")
}

.assert_required_columns <- function(data, required_columns, strict) {
  if (is.null(required_columns)) return(invisible(TRUE))

  ok <- validate_schema(data, required_columns, boolean_form = TRUE)
  if (!ok) {
    msg <- validate_schema(data, required_columns, boolean_form = FALSE)
    if (strict) stop(msg) else warning(msg)
  }
  invisible(TRUE)
}

.ensure_salary_in_usd <- function(data) {
  if (!"salary_in_usd" %in% names(data)) {
    data$salary_in_usd <- as.numeric(NA)
  }
  data
}


# Pipeline modes

#' Minimal cleaning pipeline
#'
#' @description
#' Validate the presence of required columns (optional), standardise column names
#' and enforce simple column types.
#'
#' @param data A `data.frame`.
#' @param required_columns Character vector of required column names (optional).
#'   Validation happens **before** standardisation so names must match the raw data.
#' @param strict Logical. If `TRUE` (default) stop when required columns are missing,
#'   otherwise emit a warning and continue.
#' @param num_threshold Proportion for numeric conversion in `enforce_types`.
#' @param max_factor_levels Maximum levels allowed to convert a character column
#'   to factor in `enforce_types`.
#'
#' @return A cleaned `data.frame`.
#' @export
pipeline_minimal <- function(data,
                             required_columns = NULL,
                             strict = TRUE,
                             num_threshold = 0.9,
                             max_factor_levels = 20) {
  df <- data
  .assert_required_columns(df, required_columns, strict)

  names(df) <- standardize_colnames(names(df))
  df <- enforce_types(df,
                      num_threshold = num_threshold,
                      max_factor_levels = max_factor_levels)
  df
}


#' Light cleaning pipeline (EDA)
#'
#' @description
#' Minimal pipeline + deduplication + soft imputation.
#'
#' @param data A `data.frame`.
#' @param required_columns Optional character vector of required columns.
#' @param strict Stop on missing required columns when `TRUE` (default).
#' @param dedup_keys Character vector of columns used to detect duplicates
#'   (after standardisation). If `NULL`, all columns are used.
#' @param dedup_keep Which duplicate to keep: `"first"` (default) or `"last"`.
#' @param num_method Imputation method for numeric columns (`"median"`, `"mean"`,
#'   `"constant"`).
#' @param cat_method Imputation method for categorical columns (`"mode"`,
#'   `"constant"`, `"new_level"`).
#' @param num_constant Value used when `num_method = "constant"`.
#' @param cat_constant Value used when `cat_method` is `"constant"` or `"new_level"`.
#' @param impute_verbose Logical, passed to `impute_missing`.
#'
#' @return A deduplicated and lightly imputed `data.frame`.
#' @export
pipeline_light_clean <- function(data,
                                 required_columns = NULL,
                                 strict = TRUE,
                                 dedup_keys = NULL,
                                 dedup_keep = c("first", "last"),
                                 num_method = "median",
                                 cat_method = "mode",
                                 num_constant = 0,
                                 cat_constant = "Missing",
                                 impute_verbose = FALSE) {
  df <- pipeline_minimal(data,
                         required_columns = required_columns,
                         strict = strict)

  df <- deduplicate_rows(df,
                         keys = dedup_keys,
                         keep = match.arg(dedup_keep))

  df <- impute_missing(df,
                       num_method = num_method,
                       cat_method = cat_method,
                       num_constant = num_constant,
                       cat_constant = cat_constant,
                       verbose = impute_verbose)
  df
}


#' Strict cleaning pipeline
#'
#' @description
#' Minimal pipeline + aggressive deduplication + winsorisation of salary +
#' strict imputation. Optionally removes rows outside valid ranges.
#'
#' @param data A `data.frame`.
#' @param required_columns Optional character vector of required columns.
#' @param strict Stop on missing required columns when `TRUE` (default).
#' @param dedup_keys Columns used for deduplication; `NULL` uses all columns.
#' @param dedup_keep `"first"` (default) or `"last"` row to keep when duplicates
#'   are detected.
#' @param cap_col Column to winsorise (default `"salary_in_usd"`). If the column
#'   is missing or not numeric the step is skipped with a warning.
#' @param cap_lower Lower quantile for winsorisation (default 0.01).
#' @param cap_upper Upper quantile for winsorisation (default 0.99).
#' @param cap_side Side to cap: `"both"` (default), `"upper"` or `"lower"`.
#' @param cap_verbose Logical, forwarded to `cap_outliers_salary`.
#' @param check_ranges Logical. When `TRUE` (default) remove rows failing
#'   `validate_ranges` if the needed columns are present.
#' @param ranges_min_year,ranges_max_year Bounds passed to `validate_ranges`.
#' @param impute_verbose Logical, forwarded to `impute_missing`.
#' @param cat_constant Value used when imputing categorical columns with the
#'   `"new_level"` strategy.
#'
#' @return A cleaned `data.frame`.
#' @export
pipeline_strict_clean <- function(data,
                                  required_columns = NULL,
                                  strict = TRUE,
                                  dedup_keys = NULL,
                                  dedup_keep = c("first", "last"),
                                  cap_col = "salary_in_usd",
                                  cap_lower = 0.01,
                                  cap_upper = 0.99,
                                  cap_side = c("both", "upper", "lower"),
                                  cap_verbose = FALSE,
                                  check_ranges = TRUE,
                                  ranges_min_year = 2000,
                                  ranges_max_year = as.integer(format(Sys.Date(), "%Y")),
                                  impute_verbose = FALSE,
                                  cat_constant = "NA") {
  df <- pipeline_minimal(data,
                         required_columns = required_columns,
                         strict = strict)

  df <- deduplicate_rows(df,
                         keys = dedup_keys,
                         keep = match.arg(dedup_keep))

  if (check_ranges && all(c("salary", "remote_ratio", "work_year") %in% names(df))) {
    df <- validate_ranges(df,
                          min_year = ranges_min_year,
                          max_year = ranges_max_year)
  }

  cap_side <- match.arg(cap_side)
  if (!is.null(cap_col) && cap_col %in% names(df)) {
    if (is.numeric(df[[cap_col]]) && any(!is.na(df[[cap_col]]))) {
      df <- cap_outliers_salary(df,
                                col = cap_col,
                                lower = cap_lower,
                                upper = cap_upper,
                                clip_side = cap_side,
                                verbose = cap_verbose)
    } else {
      warning(sprintf("Skipping winsorisation: column '%s' is not numeric or only NA", cap_col))
    }
  } else if (!is.null(cap_col)) {
    warning("Skipping winsorisation: column not found (", cap_col, ")")
  }

  df <- impute_missing(df,
                       num_method = "median",
                       cat_method = "new_level",
                       cat_constant = cat_constant,
                       verbose = impute_verbose)
  df
}


#' ML-ready pipeline
#'
#' @description
#' Strict pipeline + business normalisations + optional currency conversion.
#'
#' @param data A `data.frame`.
#' @param required_columns Optional character vector of required columns.
#' @param strict Stop on missing required columns when `TRUE` (default).
#' @param normalize Logical. Apply `normalize_all` when all expected columns are present.
#' @param do_currency Logical. Apply `convert_currency_to_usd` (default `TRUE`).
#' @param dedup_keys,dedup_keep,cap_col,cap_lower,cap_upper,cap_side,cap_verbose
#'   See [pipeline_strict_clean()].
#' @param check_ranges,ranges_min_year,ranges_max_year See [pipeline_strict_clean()].
#' @param impute_verbose,cat_constant See [pipeline_strict_clean()].
#'
#' @return A fully cleaned `data.frame` ready for modelling.
#' @export
pipeline_ml_ready <- function(data,
                              required_columns = NULL,
                              strict = TRUE,
                              normalize = TRUE,
                              do_currency = TRUE,
                              finalize = FALSE,
                              dedup_keys = NULL,
                              dedup_keep = c("first", "last"),
                              cap_col = "salary_in_usd",
                              cap_lower = 0.01,
                              cap_upper = 0.99,
                              cap_side = c("both", "upper", "lower"),
                              cap_verbose = FALSE,
                              check_ranges = TRUE,
                              ranges_min_year = 2000,
                              ranges_max_year = as.integer(format(Sys.Date(), "%Y")),
                              impute_verbose = FALSE,
                              cat_constant = "NA") {
  df <- pipeline_strict_clean(data,
                              required_columns = required_columns,
                              strict = strict,
                              dedup_keys = dedup_keys,
                              dedup_keep = dedup_keep,
                              cap_col = cap_col,
                              cap_lower = cap_lower,
                              cap_upper = cap_upper,
                              cap_side = cap_side,
                              cap_verbose = cap_verbose,
                              check_ranges = check_ranges,
                              ranges_min_year = ranges_min_year,
                              ranges_max_year = ranges_max_year,
                              impute_verbose = impute_verbose,
                              cat_constant = cat_constant)

  if (normalize) {
    needed <- c(
      "company_location", "employee_residence", "job_title", "remote_ratio",
      "company_size", "employment_type", "experience_level"
    )
    missing_needed <- setdiff(needed, names(df))
    if (length(missing_needed) == 0) {
      df <- normalize_all(df)
    } else {
      warning("normalize_all skipped; missing columns: ", paste(missing_needed, collapse = ", "))
    }
  }

  if (do_currency) {
    df <- .ensure_salary_in_usd(df)
    required_fx <- c("salary", "salary_currency", "work_year")
    missing_fx <- setdiff(required_fx, names(df))
    if (length(missing_fx) > 0) {
      stop("convert_currency_to_usd requires columns: ", paste(missing_fx, collapse = ", "))
    }
    df <- convert_currency_to_usd(df)
  }

  if (finalize) {
    df <- finalize_salary_tbl(df)
  }

  df
}


#' Currency-focused pipeline
#'
#' @description
#' Minimal pipeline + currency conversion to USD.
#'
#' @param data A `data.frame`.
#' @param required_columns Optional character vector of required columns.
#' @param strict Stop on missing required columns when `TRUE` (default).
#' @param do_currency Logical (default `TRUE`) to trigger the conversion.
#'
#' @return A `data.frame` with salaries converted to USD.
#' @export
pipeline_currency_focus <- function(data,
                                    required_columns = NULL,
                                    strict = TRUE,
                                    do_currency = TRUE) {
  df <- pipeline_minimal(data,
                         required_columns = required_columns,
                         strict = strict)

  if (do_currency) {
    df <- .ensure_salary_in_usd(df)
    required_fx <- c("salary", "salary_currency", "work_year")
    missing_fx <- setdiff(required_fx, names(df))
    if (length(missing_fx) > 0) {
      stop("convert_currency_to_usd requires columns: ", paste(missing_fx, collapse = ", "))
    }
    df <- convert_currency_to_usd(df)
  }
  df
}


#' Pipeline without imputation
#'
#' @description
#' Minimal pipeline + deduplication. Missing values are preserved for downstream steps.
#'
#' @param data A `data.frame`.
#' @param required_columns Optional character vector of required columns.
#' @param strict Stop on missing required columns when `TRUE` (default).
#' @param dedup_keys Columns used for deduplication; `NULL` uses all columns.
#' @param dedup_keep `"first"` (default) or `"last"` row to keep when duplicates
#'   are detected.
#'
#' @return A deduplicated `data.frame` with missing values unchanged.
#' @export
pipeline_no_impute <- function(data,
                               required_columns = NULL,
                               strict = TRUE,
                               dedup_keys = NULL,
                               dedup_keep = c("first", "last")) {
  df <- pipeline_minimal(data,
                         required_columns = required_columns,
                         strict = strict)

  dedup_keep <- match.arg(dedup_keep)
  df <- deduplicate_rows(df,
                         keys = dedup_keys,
                         keep = dedup_keep)
  df
}


#' Legacy validation pipeline
#'
#' @description
#' Backward-compatible wrapper around `clean_data_pipeline` (schema validation +
#' standardisation + typing + deduplication).
#'
#' @param data A CSV path or a `data.frame`.
#' @inheritParams clean_data_pipeline
#'
#' @return A cleaned `data.frame`.
#' @export
pipeline_legacy_clean <- function(data,
                                  required_columns,
                                  num_threshold = 0.9,
                                  max_factor_levels = 20,
                                  keys = NULL,
                                  keep = "first") {
  if (is.character(data) && length(data) == 1) {
    return(clean_data_pipeline(
      data,
      required_columns,
      num_threshold = num_threshold,
      max_factor_levels = max_factor_levels,
      keys = keys,
      keep = keep
    ))
  }

  df <- pipeline_minimal(data,
                         required_columns = required_columns,
                         strict = TRUE,
                         num_threshold = num_threshold,
                         max_factor_levels = max_factor_levels)
  deduplicate_rows(df, keys = keys, keep = keep)
}


#' Generic pipeline entry point
#'
#' @description
#' Run one of the predefined cleaning pipelines on either a `data.frame` or a
#' path to a raw CSV file.
#'
#' @param data Either a `data.frame` or a path to a CSV file.
#' @param mode Pipeline mode: `"minimal"`, `"light_clean"`, `"strict_clean"`,
#'   `"ml_ready"`, `"currency_focus"` or `"no_impute"`.
#' @param load_fun Function used to load the data when `data` is a path
#'   (default `read_raw_csv`).
#' @param load_args Named list of extra arguments passed to `load_fun`.
#' @param ... Additional arguments forwarded to the chosen pipeline mode.
#'
#' @return A cleaned `data.frame`.
#' @export
pipeline <- function(data,
                     mode = c("minimal",
                              "light_clean",
                              "strict_clean",
                              "ml_ready",
                              "currency_focus",
                              "no_impute",
                              "legacy_clean"),
                     load_fun = read_raw_csv,
                     load_args = list(),
                     ...) {
  mode <- match.arg(mode)
  if (!is.function(load_fun)) {
    stop("load_fun must be a function (e.g. read_raw_csv)")
  }

  df <- .load_dataframe(data,
                        load_fun = load_fun,
                        load_args = load_args)

  switch(
    mode,
    minimal        = pipeline_minimal(df, ...),
    light_clean    = pipeline_light_clean(df, ...),
    strict_clean   = pipeline_strict_clean(df, ...),
    ml_ready       = pipeline_ml_ready(df, ...),
    currency_focus = pipeline_currency_focus(df, ...),
    no_impute      = pipeline_no_impute(df, ...),
    legacy_clean   = pipeline_legacy_clean(data, ...),
    stop("Unknown pipeline mode: ", mode)
  )
}


#' Run and export a pipeline
#'
#' @description
#' Convenience wrapper that runs [pipeline()] and writes the result using
#' [export_csv()].
#'
#' @param in_path Path to the raw CSV file.
#' @param mode Pipeline mode (see [pipeline()]).
#' @param out_path Destination path (directory + file name) for the cleaned CSV.
#' @param export_sep Separator used for writing (default `";"`).
#' @param report_path Optional path to write a cleaning report (text file).
#' @param report_stats Optional named list of stats to include in the report.
#' @param load_fun Function used to load the raw file (default `read_raw_csv`).
#' @param load_args Named list of arguments forwarded to `load_fun`.
#' @param ... Additional arguments forwarded to [pipeline()].
#'
#' @return The cleaned `data.frame` (invisibly), after writing it to disk.
#' @export
export_pipeline <- function(in_path,
                            mode = "ml_ready",
                            out_path,
                            export_sep = ";",
                            report_path = NULL,
                            report_stats = NULL,
                            load_fun = read_raw_csv,
                            load_args = list(),
                            ...) {
  df_clean <- pipeline(in_path,
                       mode = mode,
                       load_fun = load_fun,
                       load_args = load_args,
                       ...)

  out_dir <- dirname(out_path)
  out_name <- basename(out_path)
  export_csv(df_clean,
             path = out_dir,
             filename = out_name,
             sep = export_sep)

  if (!is.null(report_path)) {
    write_cleaning_report(
      df_clean,
      original_data = NULL,
      file = report_path,
      stats_list = report_stats
    )
  }

  invisible(df_clean)
}
