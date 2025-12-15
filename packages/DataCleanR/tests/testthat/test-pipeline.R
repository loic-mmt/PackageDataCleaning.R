test_that("pipeline_minimal standardises names and types", {
  df <- data.frame(
    "Num Col" = c("1", "2"),
    "CatCol" = c("a", "b"),
    stringsAsFactors = FALSE
  )

  res <- pipeline_minimal(df, num_threshold = 0.5, max_factor_levels = 5)

  expect_equal(names(res), c("num_col", "cat_col"))
  expect_true(is.integer(res$num_col))
  expect_true(is.factor(res$cat_col))
})


test_that("pipeline_light_clean deduplicates then imputes", {
  df <- data.frame(
    id = c(1, 1, 2),
    salary = c(10, 10, NA),
    cat = c("A", NA, "A"),
    stringsAsFactors = FALSE
  )

  res <- pipeline_light_clean(
    df,
    dedup_keys = "id",
    num_method = "median",
    cat_method = "mode",
    impute_verbose = FALSE
  )

  expect_equal(nrow(res), 2)
  expect_equal(res$salary, c(10, 10))
  expect_false(anyNA(res$cat))
})


test_that("pipeline_strict_clean validates ranges and caps outliers", {
  df <- data.frame(
    salary = c(100, 1e6, 200, -5),
    salary_currency = "USD",
    salary_in_usd = c(100, 1e6, 200, -5),
    work_year = c(2022, 2022, 2022, 2022),
    remote_ratio = c(50, 50, 50, 50),
    stringsAsFactors = FALSE
  )

  res <- pipeline_strict_clean(
    df,
    required_columns = c("salary", "salary_currency", "salary_in_usd", "work_year", "remote_ratio"),
    cap_lower = 0.5,
    cap_upper = 0.5,
    cap_col = "salary_in_usd",
    cap_verbose = FALSE,
    impute_verbose = FALSE
  )

  expect_equal(nrow(res), 3) # one row removed by validate_ranges (salary negative)
  expect_equal(res$salary_in_usd, rep(200, 3))
})


test_that("pipeline_ml_ready normalises and converts currency", {
  df <- data.frame(
    salary = 100000,
    salary_currency = "USD",
    salary_in_usd = 100000,
    work_year = 2023,
    remote_ratio = "120",
    company_location = "FR",
    employee_residence = "US",
    job_title = "Data Scientist",
    company_size = "M",
    employment_type = "FT",
    experience_level = "SE",
    stringsAsFactors = FALSE
  )

  res <- pipeline_ml_ready(
    df,
    normalize = TRUE,
    do_currency = TRUE,
    finalize = TRUE,
    check_ranges = FALSE,
    cap_lower = 0.1,
    cap_upper = 0.9,
    impute_verbose = FALSE
  )

  expect_equal(res$salary_in_usd, 100000)
  expect_equal(res$remote_ratio, 100)
  expect_equal(as.character(res$company_size), "Medium")
  expect_equal(as.character(res$employment_type), "Full-time")
  expect_true(is.factor(res$job_title))
  expect_true(inherits(res, "salary_tbl"))
})


test_that("pipeline_currency_focus converts to usd", {
  df <- data.frame(
    salary = 50000,
    salary_currency = "USD",
    salary_in_usd = NA,
    work_year = 2023,
    stringsAsFactors = FALSE
  )

  res <- pipeline_currency_focus(df)

  expect_equal(res$salary_in_usd, 50000)
})


test_that("pipeline_legacy_clean keeps compatibility with clean_data_pipeline", {
  data_for_import <- data.frame(
    Numeric_Col = c("1.5", "1.5", "3.1"),
    Int_Col = c("1", "1", "3"),
    factor_col = c("A", "A", "A"),
    char_col = c("Alice", "Bob", "Charlie"),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".csv")
  write.csv2(data_for_import, tmp, row.names = FALSE)

  data_cleaned <- pipeline(
    tmp,
    mode = "legacy_clean",
    required_columns = c("Numeric_Col", "Int_Col", "char_col"),
    keys = c("numeric_col", "int_col", "factor_col")
  )

  expect_equal(names(data_cleaned), c("numeric_col", "int_col", "factor_col", "char_col"))
  expect_equal(attr(data_cleaned, "n_removed"), 1)
})


test_that("export_pipeline can write a cleaning report", {
  df <- data.frame(
    salary = 10,
    salary_currency = "USD",
    salary_in_usd = 10,
    work_year = 2023,
    remote_ratio = 0,
    stringsAsFactors = FALSE
  )

  in_path <- tempfile(fileext = ".csv")
  write.csv2(df, in_path, row.names = FALSE)

  out_dir <- tempfile()
  out_file <- file.path(out_dir, "clean.csv")
  report <- tempfile(fileext = ".txt")

  res <- export_pipeline(
    in_path,
    mode = "currency_focus",
    out_path = out_file,
    report_path = report
  )

  expect_true(file.exists(out_file))
  expect_true(file.exists(report))
  expect_equal(res$salary_in_usd, 10)
})
