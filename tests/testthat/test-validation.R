test_that("preprocessing_report() produces a character report covering the key sections", {
  set.seed(40)
  n <- 60
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20),
    Irrigation = sample(c("Drip", "Furrow"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$Nitrogen[sample(n, 4)] <- NA

  prep <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
  report <- preprocessing_report(prep, verbose = FALSE)

  expect_type(report, "character")
  expect_true(any(grepl("PREPROCESSING REPORT", report)))
  expect_true(any(grepl("Response variable: Yield", report)))
  expect_true(any(grepl("Validation Status", report)))
  expect_true(any(grepl("Encoding Summary", report)))
  expect_true(any(grepl("Scaling Summary", report)))
})

test_that("preprocessing_report() reuses a supplied validate_preprocessing() result instead of recomputing", {
  set.seed(41)
  n <- 40
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  prep <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
  val <- validate_preprocessing(prep, verbose = FALSE)

  report <- preprocessing_report(prep, validation_result = val, verbose = FALSE)

  expect_true(any(grepl(sprintf("Errors: %d", val$n_errors), report)))
  expect_true(any(grepl(sprintf("Warnings: %d", val$n_warnings), report)))
})

test_that("preprocessing_report() can write the report to a file", {
  set.seed(42)
  n <- 30
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  prep <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)

  out_file <- tempfile(fileext = ".txt")
  on.exit(unlink(out_file))
  preprocessing_report(prep, output_file = out_file, verbose = FALSE)

  expect_true(file.exists(out_file))
  written <- readLines(out_file)
  expect_true(any(grepl("PREPROCESSING REPORT", written)))
})

test_that("preprocessing_report() rejects an invalid preprocessing result", {
  expect_error(
    preprocessing_report(list(no_data_here = TRUE), verbose = FALSE),
    "Invalid preprocessing result"
  )
})
