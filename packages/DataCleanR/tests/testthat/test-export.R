test_that("export_csv writes to given path with ; and default filename", {
  df <- data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE)
  tmp <- tempfile("exports_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  res <- export_csv(df, path = tmp, verbose = FALSE)

  expect_true(dir.exists(tmp))
  expect_true(file.exists(file.path(tmp, "cleaned_data.csv")))
  expect_equal(res, file.path(tmp, "cleaned_data.csv"))

  back <- utils::read.csv2(file.path(tmp, "cleaned_data.csv"), stringsAsFactors = FALSE)
  expect_identical(back, df)
})

test_that("export_csv supports comma separator and custom filename", {
  df <- data.frame(x = c(1, 2.5, 3.2), stringsAsFactors = FALSE)
  tmp <- tempfile("exports_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  filename <- "out.csv"
  res <- export_csv(df, path = tmp, filename = filename, sep = ",", verbose = FALSE)

  expect_true(file.exists(file.path(tmp, filename)))
  expect_equal(res, file.path(tmp, filename))

  back <- utils::read.csv(file.path(tmp, filename), stringsAsFactors = FALSE)
  expect_identical(back, df)
})

test_that("export_csv respects overwrite = FALSE", {
  df <- data.frame(x = 1:2)
  tmp <- tempfile("exports_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  p <- export_csv(df, path = tmp, filename = "file.csv", verbose = FALSE)
  expect_true(file.exists(p))
  expect_error(
    export_csv(df, path = tmp, filename = "file.csv", overwrite = FALSE, verbose = FALSE),
    "File already exists: /tmp/RtmpZEdbr8/exports_1d2b0241ec102/file.csv"
  )
})

test_that("write_cleaning_report fonctionne correctement", {

  # === Test 1 : Création du rapport et structure de base ===
  df <- data.frame(x = 1:3, y = c("a", "b", "c"))
  tmp_file <- tempfile(fileext = ".txt")

  result <- write_cleaning_report(df, file = tmp_file)

  expect_true(file.exists(tmp_file))
  expect_equal(result, normalizePath(tmp_file, mustWork = FALSE))

  content <- readLines(tmp_file)
  expect_true(any(grepl("RAPPORT", content)))
  expect_true(any(grepl("Lignes finales.*: 3", content)))
  expect_true(any(grepl("Colonnes finales.*: 2", content)))
  expect_true(any(grepl("Date :", content)))


  # === Test 2 : Comparaison avec original_data et stats_list ===
  df_original <- data.frame(x = 1:10, y = letters[1:10])
  df_clean <- data.frame(x = 1:7, y = letters[1:7])
  tmp_file2 <- tempfile(fileext = ".txt")
  stats <- list("Outliers plafonnés" = 5, "NA imputés" = 12)

  write_cleaning_report(df_clean, original_data = df_original,
                        file = tmp_file2, stats_list = stats)

  content2 <- readLines(tmp_file2)
  expect_true(any(grepl("Lignes supprimées.*: 3", content2)))
  expect_true(any(grepl("Outliers plafonnés.*: 5", content2)))
  expect_true(any(grepl("NA imputés.*: 12", content2)))


  # === Test 3 : Détection des valeurs manquantes ===
  df_na <- data.frame(col1 = c(1, NA, 3), col2 = c(1, 2, 3))
  tmp_file3 <- tempfile(fileext = ".txt")

  write_cleaning_report(df_na, file = tmp_file3)

  content3 <- readLines(tmp_file3)
  expect_true(any(grepl("col1.*: 1 NA", content3)))

  df_complete <- data.frame(a = 1:3)
  tmp_file4 <- tempfile(fileext = ".txt")
  write_cleaning_report(df_complete, file = tmp_file4)
  content4 <- readLines(tmp_file4)
  expect_true(any(grepl("Aucune valeur manquante", content4)))


  # === Test 4 : Erreur si data n'est pas un data.frame ===
  expect_error(
    write_cleaning_report(data = c(1, 2, 3), file = tempfile()),
    "data doit être un data.frame"
  )
})