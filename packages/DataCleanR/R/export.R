#' Export data to CSV
#'
#' @description
#' Save a `data.frame` to a CSV file on disk.
#' This function automatically handles creation of the target directory, appends
#' the `.csv` extension if needed, and chooses an appropriate writer based on
#' the column separator.
#'
#' @details
#' The function simplifies CSV export by performing the following steps:
#' \enumerate{
#'   \item Check that the `data` object is a data.frame.
#'   \item Create the `path` directory recursively if it does not already exist.
#'   \item Append the `.csv` extension to the file name if it is missing.
#'   \item Choose an optimised writing function based on the separator:
#'     \itemize{
#'       \item For `sep = ";"`: use `utils::write.csv2` (European-style format).
#'       \item For `sep = ","`: use `utils::write.csv` (US-style format).
#'       \item Otherwise: use `utils::write.table`.
#'     }
#' }
#'
#' @param data The `data.frame` to export.
#' @param path Destination folder path (default `"exports"`). The folder will be
#'   created if it does not exist.
#' @param filename File name. The `.csv` extension is added automatically if
#'   necessary (default `"cleaned_data"`).
#' @param sep Column separator character (default `";"`).
#' @param row.names Logical. Should row names be included in the file?
#'   (default `FALSE`).
#' @param overwrite Logical. If `FALSE`, an error is thrown if the file already
#'   exists. If `TRUE` (default), it is overwritten.
#' @param verbose Logical. If `TRUE` (default), prints a message with the full
#'   path of the written file.
#'
#' @return (Invisibly) returns the full path to the created file.
#'
#' @family Export functions
#' @examples
#' # Test data
#' df_test <- data.frame(
#'   id = 1:3,
#'   name = c("Alice", "Bob", "Charlie"),
#'   score = c(10.5, 15.2, 8.0)
#' )
#'
#' # Create a temporary folder to avoid writing on your disk
#' tmp_folder <- tempfile() # Use a random folder name
#'
#' # 1. Simple export (semicolon separator by default)
#' path_file <- export_csv(df_test, path = tmp_folder, filename = "test_export")
#'
#' # Check that the file exists
#' file.exists(path_file)
#'
#' # 2. Export with comma separator and no overwrite (should error if rerun)
#' try({
#'   export_csv(df_test, path = tmp_folder, filename = "test_export", sep = ",", overwrite = FALSE)
#' })
#'
#' # Clean up temporary folder
#' unlink(tmp_folder, recursive = TRUE)
#' @export
export_csv <- function(data,
                       path = "exports",
                       filename = "cleaned_data",
                       sep = ";",
                       row.names = FALSE,
                       overwrite = TRUE,
                       verbose = TRUE) {
  if (!is.data.frame(data)) stop("data must be a data.frame")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  if (!grepl("\\.csv$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".csv")
  filepath <- file.path(path, filename)

  if (!overwrite && file.exists(filepath)) stop(sprintf("File already exists: %s", filepath))

  if (sep == ";") {
    utils::write.csv2(data, file = filepath, row.names = row.names)
  } else if (sep == ",") {
    utils::write.csv(data, file = filepath, row.names = row.names)
  } else {
    utils::write.table(data, file = filepath, sep = sep, dec = ".", row.names = row.names, col.names = TRUE, qmethod = "double")
  }

  if (verbose) message(sprintf("File written: %s", normalizePath(filepath)))
  invisible(filepath)
}


#' Write a cleaning report to a text file
#'
#' @description
#' Create a text file summarising the state of the data set after cleaning.
#' It optionally compares the dimensions before/after and records the provided statistics.
#'
#' @details
#' The report contains:
#' * The date and time of the report.
#' * The dimensions of the final data set.
#' * The number of remaining missing values (NA) per column.
#' * A section of operational logs displaying the counters provided in `stats_list`.
#'
#' @param data The cleaned (final) `data.frame`.
#' @param original_data (Optional) The raw `data.frame` (before cleaning) to
#'   compare removed rows.
#' @param file Path to the output file (e.g. `"reports/cleaning_log.txt"`).
#' @param stats_list A named list of counters or messages
#'   (e.g. `list(outliers_capped = 12, rows_removed = 5)`).
#'
#' @return Invisibly returns the path to the created file.
#' @family Export functions
#' @examples
#' # Raw data
#' df_raw <- data.frame(id = 1:5, val = c(10, NA, 30, 1000, 50))
#'
#' # Cleaned data (example)
#' df_clean <- data.frame(id = c(1, 3, 4, 5), val = c(10, 30, 100, 50))
#'
#' # List of operations performed (collected during the script)
#' my_stats <- list(
#'   "NA imputed" = 1,
#'   "Outliers capped" = 1,
#'   "Rows removed" = 1
#' )
#'
#' # Generate the report
#' tmp_file <- tempfile(fileext = ".txt")
#' write_cleaning_report(df_clean, original_data = df_raw, file = tmp_file, stats_list = my_stats)
#'
#' # Read the result
#' cat(readLines(tmp_file), sep = "\n")
#' @export
write_cleaning_report <- function(data,
                                  original_data = NULL,
                                  file = "cleaning_report.txt",
                                  stats_list = NULL) {

  if (!is.data.frame(data)) stop("data must be a data.frame")

  # Create parent directory if needed
  dir_name <- dirname(file)
  if (dir_name != "." && !dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)

  # Open file for writing
  sink(file)
  on.exit(sink())

  cat("   CLEANING REPORT  \n")

  cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

  # 1. Dimensions and Comparison
  cat("--- DIMENSIONS ---\n")
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  cat(sprintf("Final rows       : %d\n", n_rows))
  cat(sprintf("Final columns    : %d\n", n_cols))

  if (!is.null(original_data)) {
    n_orig <- nrow(original_data)
    diff_rows <- n_orig - n_rows
    cat(sprintf("Initial rows     : %d\n", n_orig))
    cat(sprintf("Rows removed     : %d (%.2f%%)\n", diff_rows, (diff_rows/n_orig)*100))
  }
  cat("\n")

  # 2. Remaining missing values
  cat("--- REMAINING MISSING VALUES (NA) ---\n")
  na_counts <- colSums(is.na(data))
  na_cols <- na_counts[na_counts > 0]

  if (length(na_cols) == 0) {
    cat("No missing values (complete data set).\n")
  } else {
    for (col in names(na_cols)) {
      cat(sprintf("- %s: %d NA\n", col, na_cols[col]))
    }
  }
  cat("\n")

  # 3. Custom logs (Outliers, etc.)
  if (!is.null(stats_list) && length(stats_list) > 0) {
    for (name in names(stats_list)) {
      cat(sprintf("- %s : %s\n", name, as.character(stats_list[[name]])))
    }
    cat("\n")
  }
  cat("End of report.\n")

  invisible(normalizePath(file, mustWork = FALSE))
}