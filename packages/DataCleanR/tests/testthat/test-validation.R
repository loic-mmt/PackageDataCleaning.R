test_that("read_raw_csv importe correctement", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  tmp <- tempfile(fileext = ".csv")
  write.csv2(df, tmp, row.names = FALSE)
  imported_df <- read_raw_csv(tmp)
  expect_equal(imported_df, df)
  expect_true(is.data.frame(imported_df))
})

test_that("validate_schema", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  names_to_test1 <- c("a", "b")
  names_to_test2 <- c("a", "b", "c")
  test1 <- validate_schema(df, names_to_test1)
  test2 <- validate_schema(df, names_to_test2)
  expect_equal(list(test1, test2), list("All required columns are present in the data frame.", "The data frame is not complete; it is missing the following columns: c"))
})

test_that("standardize_colnames", {
  df <- data.frame("Nom Client" = c("Alice", "Bob"),
                   "MontantTotal" = c(100, 200),
                   "Date-De-Vente" = c("2024-01-01", "2024-01-02"))
  names(df) <- standardize_colnames(names(df))
  expect_equal(names(df), c("nom_client", "montant_total", "date_de_vente"))
})


test_that("enforce_types", {
  df <- data.frame(
    numeric_col = c("1.5", "2.2", "3.1"),
    int_col = c("1", "2", "3"),
    factor_col = c("A", "B", "A"),
    char_col = c("Alice", "Bob", "Charlie"),
    stringsAsFactors = FALSE
  )
  
  df_clean <- enforce_types(df, num_threshold = 0.9, max_factor_levels = 10)
  
  expect_true(is.numeric(df_clean$numeric_col), info = "numeric_col doit être numeric")
  expect_true(is.integer(df_clean$int_col), info = "int_col doit être integer")
  expect_true(is.factor(df_clean$factor_col), info = "factor_col doit être factor")
  expect_true(is.factor(df_clean$char_col), info = "char_col doit être factor maintenant")
  
  expect_equal(df_clean$numeric_col, c(1.5, 2.2, 3.1))
  expect_equal(levels(df_clean$factor_col), c("A", "B"))
})



test_that("deduplicate_rows", {
  df <- data.frame(
    id = c(1, 1, 2, 2, 3),
    job = c("DE", "DE", "DS", "DS", "DE"),
    salary = c(100, 100, 200, 200, 300)
  )

  out <- DataCleanR::deduplicate_rows(df)

  expect_equal(nrow(out), 3)
  expect_equal(attr(out, "n_removed"), 2)
  expect_equal(out, df[c(1, 3, 5), ], ignore_attr = TRUE)
})

test_that ("clean_data_pipeline work well", {
  data_for_import <- data.frame(
    Numeric_Col = c("1.5", "1.5", "3.1"),
    Int_Col = c("1", "1", "3"),
    factor_col = c("A", "A", "A"),
    char_col = c("Alice", "Bob", "Charlie"),
    stringsAsFactors = FALSE
  )
  tmp <- tempfile(fileext = ".csv")
  write.csv2(data_for_import, tmp, row.names = FALSE)
  data_cleaned <- clean_data_pipeline(tmp, c("Numeric_Col", "Int_Col", "char_col"), keys = c("numeric_col", "int_col", "factor_col"))
  data_expected <- data.frame(
    numeric_col = c(1.5,3.1),
    int_col = c(1,3),
    factor_col = as.factor(c("A","A")),
    char_col = as.factor(c("Alice", "Bob","Charlie"))[-2]
  )
  expect_equal(data_cleaned, data_expected, ignore_attr = TRUE)
  expect_equal(attr(data_cleaned, "n_removed"), 1)
})