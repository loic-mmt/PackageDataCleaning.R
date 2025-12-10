#' @title Convert salaries to USD
#'
#' @description
#' Convert local salary amounts to US dollars (USD).
#' This function takes into account the variation of exchange rates by work year (`work_year`).
#'
#' @details
#' The conversion procedure is as follows:
#' \enumerate{
#'   \item **Validation**: Checks that the columns `salary`, `salary_currency` and `work_year` are present.
#'   \item **Typing**: Ensures that the salary is numeric and the year is an integer.
#'   \item **Join**: Merges the data with the reference table `exchange_rates_to_usd` based on currency and year.
#'   \item **Computation**: Applies the exchange rate to update `salary_in_usd`.
#'   \item **Cleanup**: Removes temporary rate columns (`rate`).
#' }
#'
#' @note
#' This function depends on the global object `exchange_rates_to_usd`, which must contain the columns `currency`, `year` and `rate`.
#'
#' @param data A `data.frame` that must contain at least the following columns:
#'   \itemize{
#'     \item `salary`: The gross amount in the local currency.
#'     \item `salary_currency`: The ISO code of the currency (e.g. "EUR", "GBP").
#'     \item `work_year`: The reference fiscal year.
#'     \item `salary_in_usd`: (Optional, will be overwritten/created).
#'   }
#'
#' @return The `data.frame` enriched with the computed `salary_in_usd` column.
#'
#' @family Currency functions
#' @examples
#' # For the example to work, we simulate the exchange rate table
#' # Normally, this object 'exchange_rates_to_usd' is loaded in the package
#' exchange_rates_to_usd <- data.frame(
#'   currency = c("EUR", "GBP"),
#'   year = c(2023, 2023),
#'   rate = c(1.1, 1.25) # 1 EUR = 1.1 USD
#' )
#'
#' df <-  data.frame(
#'   salary = 60000,
#'   salary_currency = "GBP",
#'   salary_in_usd = 75000,
#'   work_year = 2023
#' )
#'
#' # Conversion
#' convert_currency_to_usd(df)
#' @export
convert_currency_to_usd <- function(data) {
  if (!all(c("salary", "salary_currency", "salary_in_usd", "work_year") %in% names(data))) {
    stop("The columns 'salary', 'salary_currency', 'salary_in_usd' and 'work_year' must exist.")
  }
  data$salary <- as.numeric(as.character(data$salary))
  data$salary_currency <- as.character(data$salary_currency)
  data$work_year <- as.integer(data$work_year)

  # Utilisation des données de mapping exportées
  data <- merge(data, exchange_rates_to_usd,
                by.x = c("salary_currency", "work_year"),
                by.y = c("currency", "year"),
                all.x = TRUE)

  data$salary_in_usd <- data$salary * data$rate
  data$calculated_usd <- NULL
  data$rate <- NULL

  return(data)
}