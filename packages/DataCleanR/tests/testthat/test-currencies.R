
test_that("convert_currency_to_usd convertit correctement les devises", {
  # Test 1: Conversion EUR vers USD
  test_data_eur <- data.frame(
    salary = 80000,
    salary_currency = "EUR",
    salary_in_usd = 85847,
    work_year = 2023
  )
  result <- convert_currency_to_usd(test_data_eur)
  expect_equal(result$salary_in_usd, 73600)  # 80000 * 0.92
  
  # Test 2: USD reste inchangé
  test_data_usd <- data.frame(
    salary = 100000,
    salary_currency = "USD",
    salary_in_usd = 100000,
    work_year = 2023
  )
  result <- convert_currency_to_usd(test_data_usd)
  expect_equal(result$salary_in_usd, 100000)  # 100000 * 1.00
  
  # Test 3: Conversion GBP vers USD
  test_data_gbp <- data.frame(
    salary = 60000,
    salary_currency = "GBP",
    salary_in_usd = 75000,
    work_year = 2023
  )
  result <- convert_currency_to_usd(test_data_gbp)
  expect_equal(result$salary_in_usd, 45000)  # 60000 * 0.75
  
  # Test 4: Conversion JPY vers USD
  test_data_jpy <- data.frame(
    salary = 10000000,
    salary_currency = "JPY",
    salary_in_usd = 95000,
    work_year = 2023
  )
  result <- convert_currency_to_usd(test_data_jpy)
  expect_equal(result$salary_in_usd, 82200)  # 10000000 * 0.00822
})

test_that("convert_currency_to_usd gère plusieurs lignes correctement", {
  test_data <- data.frame(
    salary = c(80000, 30000, 175000),
    salary_currency = c("EUR", "USD", "CAD"),
    salary_in_usd = c(85847, 30000, 180000),
    work_year = c(2023, 2023, 2023)
  )
  result <- convert_currency_to_usd(test_data)
  
  expect_equal(nrow(result), 3)
  expect_equal(result[result$salary_currency == "EUR", "salary_in_usd"], 73600)  # 80000 * 0.92
  expect_equal(result[result$salary_currency == "USD", "salary_in_usd"], 30000)  # 30000 * 1.00
  expect_equal(result[result$salary_currency == "CAD", "salary_in_usd"], 134750)  # 175000 * 0.77
})

test_that("convert_currency_to_usd gère différentes années", {
  test_data <- data.frame(
    salary = c(50000, 50000, 50000, 50000),
    salary_currency = c("EUR", "EUR", "EUR", "EUR"),
    salary_in_usd = c(48000, 45000, 52000, 49000),
    work_year = c(2020, 2021, 2022, 2023)
  )
  result <- convert_currency_to_usd(test_data)
  
  expect_equal(result[result$work_year == 2020, "salary_in_usd"], 44000)  # 50000 * 0.88
  expect_equal(result[result$work_year == 2021, "salary_in_usd"], 42500)  # 50000 * 0.85
  expect_equal(result[result$work_year == 2022, "salary_in_usd"], 47500)  # 50000 * 0.95
  expect_equal(result[result$work_year == 2023, "salary_in_usd"], 46000)  # 50000 * 0.92
})

test_that("convert_currency_to_usd vérifie que les valeurs calculées sont exactes", {
  test_data <- data.frame(
    salary = c(80000, 60000, 70000, 100000),
    salary_currency = c("EUR", "GBP", "CAD", "USD"),
    salary_in_usd = c(85847, 75000, 90000, 100000),
    work_year = c(2023, 2023, 2022, 2023)
  )
  result <- convert_currency_to_usd(test_data)
  
  # Vérifier les conversions exactes
  expect_equal(result[result$salary_currency == "EUR", "salary_in_usd"], 73600)  # 80000 * 0.92
  expect_equal(result[result$salary_currency == "GBP", "salary_in_usd"], 45000)  # 60000 * 0.75
  expect_equal(result[result$salary_currency == "CAD" & result$work_year == 2022, "salary_in_usd"], 54600)  # 70000 * 0.78
  expect_equal(result[result$salary_currency == "USD", "salary_in_usd"], 100000)  # 100000 * 1.00
})

test_that("convert_currency_to_usd lève une erreur si colonnes manquantes", {
  test_data_incomplete <- data.frame(
    salary = 50000,
    salary_in_usd = 50000,
    work_year = 2023
  )
  expect_error(
    convert_currency_to_usd(test_data_incomplete),
    "Les colonnes 'salary', 'salary_currency', 'salary_in_usd' et 'work_year' doivent exister."
  )
})

test_that("convert_currency_to_usd ne contient pas les colonnes rate et calculated_usd", {
  test_data <- data.frame(
    salary = 50000,
    salary_currency = "EUR",
    salary_in_usd = 52000,
    work_year = 2023
  )
  result <- convert_currency_to_usd(test_data)
  
  expect_false("rate" %in% names(result))
  expect_false("calculated_usd" %in% names(result))
})

test_that("convert_currency_to_usd gère les devises exotiques", {
  test_data <- data.frame(
    salary = c(1000000, 500000, 10000),
    salary_currency = c("VND", "KRW", "ZAR"),
    salary_in_usd = c(50, 400, 600),
    work_year = c(2023, 2023, 2023)
  )
  result <- convert_currency_to_usd(test_data)
  
  expect_equal(result[result$salary_currency == "VND", "salary_in_usd"], 41)      # 1000000 * 0.000041
  expect_equal(result[result$salary_currency == "KRW", "salary_in_usd"], 380)     # 500000 * 0.00076
  expect_equal(result[result$salary_currency == "ZAR", "salary_in_usd"], 530)     # 10000 * 0.053
})

test_that("convert_currency_to_usd convertit correctement les types de données", {
  test_data <- data.frame(
    salary = as.character(50000),
    salary_currency = factor("EUR"),
    salary_in_usd = 52000,
    work_year = "2023",
    stringsAsFactors = FALSE
  )
  result <- convert_currency_to_usd(test_data)
  
  expect_type(result$salary, "double")
  expect_type(result$salary_currency, "character")
  expect_type(result$work_year, "integer")
  expect_equal(result$salary_in_usd, 46000)  # 50000 * 0.92
})

