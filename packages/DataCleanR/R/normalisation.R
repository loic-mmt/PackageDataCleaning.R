#' Normalize a vector to a factor
#' (use with `df$col <- normalize_factor(df$col, mapping, levels)`)
#'
#' @description
#' Transform a raw vector into a factor by applying a mapping table.
#' Values that are not found or are missing are replaced by `"Unknown"`.
#'
#' @details
#' This function is useful to standardise poorly formatted categorical columns.
#' It performs the following operations:
#' \enumerate{
#'   \item Replace the values of the vector according to the supplied `mapping`.
#'   \item Replace `NA` values with the string `"Unknown"`.
#'   \item Convert the result to a `factor` with the imposed levels (`levels_in_factor`).
#' }
#'
#' @param vector The raw vector to normalise.
#' @param mapping A named vector (key-value) indicating the transformation
#'   (e.g. `c("old_val" = "new_val")`).
#' @param levels_in_factor A vector containing all valid levels for the final factor.
#' @param ordered_factor Logical (default `FALSE`). If `TRUE`, creates an ordered factor.
#'
#' @return A normalised `factor` vector.
#'
#' @family Normalization functions
#' @examples
#' raw_data <- c("FR", "US", "UK", "FR", "XX")
#'
#' # Mapping table
#' map_countries <- c(
#'   "FR" = "France",
#'   "US" = "United States",
#'   "UK" = "United Kingdom"
#' )
#'
#' # Allowed levels
#' valid_levels <- c("France", "United States", "United Kingdom", "Unknown")
#'
#' # Apply normalisation
#' normalize_factor(raw_data, map_countries, valid_levels)
#' @export
normalize_factor <- function(vector, mapping, levels_in_factor, ordered_factor = FALSE) {
  vector <- as.character(vector)
  normalized <- mapping[vector]
  normalized[is.na(normalized)] <- "Unknown"
  normalized <- factor(normalized, levels = levels_in_factor, ordered = ordered_factor)
  return(normalized)
}

#' Normalise the `remote_ratio` column
#'
#' @description
#' Clean and standardise the `remote_ratio` column of a data.frame.
#' Ensures that values are numeric and between 0 and 100.
#'
#' @details
#' The processing includes:
#' \itemize{
#'   \item Conversion to numeric (conversion errors become `NA`).
#'   \item Bounding values (`pmin`/`pmax`): anything < 0 becomes 0, anything > 100 becomes 100.
#'   \item (Optional) Binarisation: If `binary = TRUE`, values become either 0 or 100
#'         according to the `threshold`.
#' }
#'
#' @param data The data.frame containing the `remote_ratio` column.
#' @param binary Logical (default `FALSE`). Activates binary mode (only 0 or 100).
#' @param threshold Numeric (default 50). Threshold for binarisation.
#'   \itemize{
#'     \item If value >= `threshold`: becomes 100.
#'     \item If value < `threshold`: becomes 0.
#'   }
#'
#' @return The `data.frame` with the corrected `remote_ratio` column.
#'
#' @family Normalization functions
#' @examples
#' df <- data.frame(remote_ratio = c("0", "100", "50", "30", "-5", "abc"))
#'
#' # Standard normalisation (numeric + bounding 0-100)
#' normalize_remote_ratio(df)
#'
#' # Binary normalisation (everything >= 50 becomes 100, the rest 0)
#' normalize_remote_ratio(df, binary = TRUE, threshold = 50)
#' @export
normalize_remote_ratio <- function(data, binary = FALSE, threshold = 50) {
  if (!"remote_ratio" %in% names(data)) {
    stop("The 'remote_ratio' column does not exist in the data frame.")
  }
  # convert to numeric (non-convertible will be NA)
  data$remote_ratio <- suppressWarnings(as.numeric(as.character(data$remote_ratio)))
  # bounds
  data$remote_ratio <- pmin(pmax(data$remote_ratio, 0), 100)
  if (binary) {
    data$remote_ratio <- ifelse(is.na(data$remote_ratio), NA, ifelse(data$remote_ratio >= threshold, 100, 0))
  }
  return(data)
}


#' Normalise all columns of a salary table
#'
#' @description
#' Wrapper (orchestrator) function that sequentially applies all normalisation rules
#' specific to the salary dataset (`salary_tbl`).
#'
#' @details
#' This function transforms the following columns:
#' \itemize{
#'   \item **Location** (`company_location`, `employee_residence`): ISO2 normalisation and creation of regional groupings.
#'   \item **Job title** (`job_title`): Standardisation of job titles.
#'   \item **Remote work** (`remote_ratio`): Numeric cleaning of the remote work ratio.
#'   \item **Company size** (`company_size`): Conversion to an ordered factor.
#'   \item **Employment type** (`employment_type`): Standardisation.
#'   \item **Experience** (`experience_level`): Conversion to an ordered factor.
#' }
#'
#' @note
#' This function depends on global mapping variables (e.g. `mapping_total`, `levels_iso2`,
#' `size_mapping`, etc.) that must be loaded in the environment before execution.
#'
#' @param data The data.frame (`salary_tbl`) to normalise.
#'
#' @return The fully normalised `data.frame`, ready for analysis.
#'
#' @family Normalization functions
#' @seealso \code{\link{normalize_factor}}, \code{\link{normalize_remote_ratio}}
#' @examples
#' \dontrun{
#' # This example does not run automatically because it requires
#' # the mapping objects (mapping_total, levels_iso2, etc.)
#' # to be present in the global environment.
#'
#' df_clean <- normalize_all(salary_tbl_raw)
#' }
#' @export
normalize_all <- function(data) {
  #Normalize company location and creation of a regionnal grouping.
  data$company_location <- normalize_factor(data$company_location, mapping_total, levels_iso2)
  data$company_grouping <- normalize_factor(data$company_location, region_map, regions_levels)

  #Normalize employee residence and creation of a regionnal grouping.
  data$employee_residence <- normalize_factor(data$employee_residence, mapping_total, levels_iso2)
  data$employee_grouping <- normalize_factor(data$employee_residence, region_map, regions_levels)

  #Normalize job titles
  data$job_title <- normalize_factor(data$job_title, mapping_job_title, levels_job_title)

  #Normalize remote ratios
  data <- normalize_remote_ratio(data)

  #Normalize company sizes
  data$company_size <- normalize_factor(data$company_size, size_mapping, size_levels, TRUE)

  #Normalize employement types
  data$employment_type <- normalize_factor(data$employment_type, mapping_employement_type, levels_employement_type)

  #Normalize experience levels
  data$experience_level <- normalize_factor(data$experience_level, experience_mapping, experience_labels_ordered, TRUE)
  return(data)
}
