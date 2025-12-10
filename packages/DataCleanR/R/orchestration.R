#' Create a salary table object
#'
#' @param data a data frame containing salary data
#' @return An object of class "salary_tbl"
#' @export
finalize_salary_tbl <- function(data) {
  stopifnot(is.data.frame(data))
  # Check required columns
  required_cols <- c("work_year", "experience_level", "employment_type", "job_title",
                    "salary", "salary_currency", "salary_in_usd", "employee_residence",
                    "remote_ratio", "company_location", "company_size")
  if (validate_schema(data, required_cols, TRUE)) {
    class(data) <- c("salary_tbl", class(data))
    return(data)
  }
  else {
    stop(validate_schema(data, required_cols))
  }
}