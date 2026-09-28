test_that("missing_summary() correctly counts missing cells and affected variables", {
  set.seed(30)
  df <- data.frame(
    Yield = rnorm(20, 5000, 500),
    Nitrogen = c(rnorm(17, 120, 20), NA, NA, NA)
  )

  result <- missing_summary(df, verbose = FALSE)

  expect_true(is.list(result))
  expect_equal(result$n_rows, 20)
  expect_equal(result$n_cols, 2)
  expect_equal(result$total_missing, 3)
  expect_equal(result$n_vars_with_missing, 1)
  expect_equal(unname(result$missing_by_var["Nitrogen"]), 3)
  expect_equal(result$missing_percent, 100 * 3 / (20 * 2))
})

test_that("missing_summary() reports zero missing values for complete data", {
  set.seed(31)
  df <- data.frame(Yield = rnorm(10, 5000, 500), Nitrogen = rnorm(10, 120, 20))

  result <- missing_summary(df, verbose = FALSE)

  expect_equal(result$total_missing, 0)
  expect_equal(result$n_vars_with_missing, 0)
  expect_equal(length(result$missing_by_var), 0)
})

test_that("missing_summary() handles a zero-row data frame without error", {
  df <- data.frame(Yield = numeric(0), Nitrogen = numeric(0))

  expect_warning(
    result <- missing_summary(df, verbose = FALSE),
    "zero rows"
  )
  expect_equal(result$n_rows, 0)
  expect_equal(result$total_missing, 0)
})

test_that("missing_summary() rejects non-data-frame input", {
  expect_error(
    missing_summary(list(a = 1), verbose = FALSE),
    "'data' must be a data frame"
  )
})
