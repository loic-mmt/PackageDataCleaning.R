#' Read a raw CSV file
#'
#' @description
#' Read a CSV file from disk and load it into a `data.frame`.
#' This function assumes semicolon (`;`) as the field separator and does not convert
#' character vectors to factors.
#'
#' @param file_path A character string indicating the path to the CSV file.
#'
#' @return A `data.frame` containing the raw data.
#'
#' @family Validation functions
#' @examples
#' # Create a temporary CSV file for the example
#' tf <- tempfile(fileext = ".csv")
#' writeLines("col1;col2\n1;a\n2;b", tf)
#'
#' # Use the function
#' df <- read_raw_csv(tf)
#' print(df)
#'
#' # Cleanup
#' unlink(tf)
#' @export
read_raw_csv <- function(file_path) {
  data <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE)
  return(data)
}


#' Validate presence of required columns
#'
#' @description
#' Check whether a `data.frame` contains all the columns required for
#' subsequent processing.
#'
#' @details
#' The function supports two output modes via the `boolean_form` argument:
#' \itemize{
#'   \item **Boolean mode**: Returns `TRUE` or `FALSE`. Useful inside `if` conditions.
#'   \item **Verbose mode** (default): Returns a text message explicitly listing
#'         missing columns, or confirming that all required columns are present.
#' }
#'
#' @param dataframe The `data.frame` to check.
#' @param required_columns A character vector with the names of the required columns.
#' @param boolean_form Logical (`TRUE`/`FALSE`). If `TRUE`, returns a simple boolean.
#'   If `FALSE`, returns a detailed message.
#'
#' @return
#' \itemize{
#'   \item If `boolean_form = TRUE`: A boolean.
#'   \item If `boolean_form = FALSE`: A character string (success or error message).
#' }
#'
#' @family Validation functions
#'
#' @examples
#' df <- data.frame(id = 1:3, salary = c(100, 200, 300))
#'
#' # Success case (message)
#' validate_schema(df, c("id", "salary"))
#'
#' # Failure case (message)
#' validate_schema(df, c("id", "salary", "age"))
#'
#' # Success case (boolean)
#' if (validate_schema(df, c("id"), boolean_form = TRUE)) {
#'   print("All required columns are present.")
#' }
#' @export
validate_schema <- function(dataframe, required_columns, boolean_form = FALSE) {
  not_commun <- required_columns[!required_columns %in% names(dataframe)]
  if (boolean_form) {
    presence <- (length(not_commun) == 0)
  }
  else {
    presence <- ""
    if (length(not_commun) == 0) {
        presence <- "All required columns are present in the data frame."
    }
    else {
        presence <- paste("The data frame is not complete; it is missing the following columns:", paste(not_commun, collapse = ", "))
    }
  }
  return(presence)
}

#' Standardise column names
#'
#' @description
#' Convert a character vector of column names to a standard `snake_case` format.
#'
#' @details
#' Normalisation applies the following rules:
#' \enumerate{
#'   \item Replace any non-alphanumeric character with `_`.
#'   \item Handle CamelCase by inserting `_` between lowercase and uppercase letters.
#'   \item Convert all characters to lowercase.
#'   \item Collapse multiple `_` and trim `_` characters at the beginning and end.
#' }
#'
#' @param data A character vector (e.g. `names(df)`).
#'
#' @return A cleaned character vector of column names.
#'
#' @family Validation functions
#' @examples
#' dirty_names <- c("First Name", "salary(USD)", "IsRemote?", "jobTitle")
#' clean_names <- standardize_colnames(dirty_names)
#' print(clean_names)
#' # Expected result: "first_name", "salary_usd", "is_remote", "job_title"
#' @export
standardize_colnames <- function(data) {
  data <- gsub("[^A-Za-z0-9]+", "_", data)
  data <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", data)
  data <- tolower(data)
  data <- gsub("_+", "_", data)
  data <- gsub("^_+|_+$", "", data)
  return(data)
}

#' Enforce simple column types
#'
#' @description
#' Analyse each column of a `data.frame` and try to convert `character` columns
#' to more appropriate types (`numeric`, `integer` or `factor`).
#'
#' @details
#' The algorithm processes the data column by column:
#' \itemize{
#'   \item Ignores columns that are already numeric, factors or dates.
#'   \item Trims surrounding whitespace with `trimws`.
#'   \item **Numeric conversion**: If the proportion of values that can be converted
#'         to numeric exceeds `num_threshold`, the column is converted to numeric
#'         (or integer when possible).
#'   \item **Factor conversion**: Otherwise, if the number of unique values is less
#'         than or equal to `max_factor_levels`, the column is converted to a factor.
#'   \item Otherwise, the column remains as character.
#' }
#'
#' @param data The input `data.frame`.
#' @param num_threshold Proportion (0 to 1). Minimum proportion of valid numeric
#'   values required to convert to numeric (default 0.9).
#' @param max_factor_levels Integer. Maximum number of unique values to allow
#'   conversion to a factor (default 20).
#'
#' @return A new `data.frame` with optimised column types.
#'
#' @family Validation functions
#' @seealso \code{\link{standardize_colnames}}
#' @examples
#' df <- data.frame(
#'   id = c("1", "2", "3"),              # Should become integer
#'   cat = c("A", "A", "B"),             # Should become factor
#'   text = c("Unique1", "Unique2", "Unique3"), # Remains character
#'   stringsAsFactors = FALSE
#' )
#'
#' str(df) # All columns are character initially
#'
#' df_typed <- enforce_types(df, max_factor_levels = 2)
#' str(df_typed) # Types corrected
#' @export
enforce_types <- function(data, num_threshold = 0.9, max_factor_levels = 20) {
  out <- data

  for (col in names(out)) {
    x <- out[[col]]

    # Ignorer si déjà au bon type
    if (is.numeric(x) || is.factor(x) || inherits(x, "Date")) {
      next
    }

    # Nettoyer les espaces
    if (is.character(x)) {
      x <- trimws(x)
    }

    # Calculer le nombre de valeurs non-NA
    valid_values <- x[!is.na(x) & x != ""]
    n_valid <- length(valid_values)

    if (n_valid == 0) {
      next  # Colonne vide
    }

    #  conversion numérique
    x_numeric <- suppressWarnings(as.numeric(x))
    n_numeric_valid <- sum(!is.na(x_numeric[!is.na(x) & x != ""]))

    # Si au moins num_threshold% des valeurs sont convertibles en numérique
    if (n_numeric_valid / n_valid >= num_threshold) {
      # Vérifier si ce sont des entiers
      if (all(x_numeric[!is.na(x_numeric)] == floor(x_numeric[!is.na(x_numeric)]))) {
        out[[col]] <- as.integer(x_numeric)
      } else {
        out[[col]] <- x_numeric
      }
      next
    }
    # Vérifier si c'est un facteur potentiel
    n_unique <- length(unique(valid_values))

    # Convertir en facteur SEULEMENT si <= max_factor_levels valeurs uniques
    if (n_unique <= max_factor_levels) {
      out[[col]] <- as.factor(x)
      next
    }

    # Sinon garder comme character
    out[[col]] <- as.character(x)
  }
  return(out)
}


#' Deduplicate data
#'
#' @description
#' Remove duplicate rows from a `data.frame` based on a key composed of one or
#' several columns.
#'
#' @param data The `data.frame` to deduplicate.
#' @param keys Character vector of column names or `NULL`.
#'   \itemize{
#'     \item If `NULL` (default): All columns are used to identify duplicates.
#'     \item If a vector: Only these columns define uniqueness (e.g. `c("ID", "Date")`).
#'   }
#' @param keep Character string indicating which row to keep when duplicates are
#'   found:
#'   \itemize{
#'     \item `"first"`: Keep the first occurrence.
#'     \item `"last"`: Keep the last occurrence.
#'   }
#'
#' @return The deduplicated `data.frame`.
#'   The returned object has an attribute `"n_removed"` indicating the number of
#'   rows removed.
#'
#' @family Validation functions
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2, 3),
#'   val = c("a", "a", "b", "c")
#' )
#'
#' # Remove the exact duplicate (row 2)
#' deduplicate_rows(df)
#'
#' # Deduplicate using only the ID (keep the last occurrence)
#' df2 <- data.frame(id = c(1, 1), val = c("a", "b"))
#' deduplicate_rows(df2, keys = "id", keep = "last")
#' @export
deduplicate_rows <- function(data, keys = NULL, keep = c("first", "last")) {
  keep <- match.arg(keep)

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (is.null(keys)) {
    keys <- names(data)
  } else {
    inconnues <- setdiff(keys, names(data))
    if (length(inconnues) > 0) {
      stop("Unknown keys: ", paste(inconnues, collapse = ", "))
    }
  }
  from_last <- identical(keep, "last")
  dup <- duplicated(data[keys], fromLast = from_last)

  out <- data[!dup, , drop = FALSE]

  attr(out, "n_removed") <- sum(dup)
  return(out)
}


#' Complete data validation pipeline
#'
#' @description
#' A pipeline function that orchestrates the entire data validation process.
#' It reads the file, validates the raw schema, standardizes column names,
#' enforces types, and deduplicates rows.
#'
#' @param file_path A character string indicating the path to the CSV file.
#' @param required_columns A character vector of column names expected in the
#' raw input file (before standardization).
#' @param num_threshold Proportion.
#' @param max_factor_levels Integer.
#' @param dedupe_keys Character vector or NULL.
#' @param dedupe_keep Character string ("first" or "last").
#'
#' @return A clean, typed, and deduplicated DataFrame.
#' @export
clean_data_pipeline <- function(file_path,required_columns, num_threshold = 0.9, 
                                max_factor_levels = 20, keys = NULL,
                                keep = "first") {

  # 1. Read Data
  df <- read_raw_csv(file_path)

  # 2. Validate Schema
  is_valid <- validate_schema(df, required_columns, boolean_form = TRUE)
  if (!is_valid) {
    error_msg <- validate_schema(df, required_columns, boolean_form = FALSE)
    stop(error_msg)
  }

  # 3. Standardize Column Names
  names(df) <- standardize_colnames(names(df))

  # 4. Enforce Types
  df <- enforce_types(df,
                      num_threshold = num_threshold,
                      max_factor_levels = max_factor_levels)

  # 5. Deduplicate
  df <- deduplicate_rows(df, keys = keys, keep = keep)

  return(df)
}