test_that("prep_yield_gap() runs end to end without error", {
  # Regression test for the original bug where this function called two
  # nonexistent functions (detect_variables_for_yield_gap,
  # encode_for_linear_regression) and mishandled remove_non_predictors()'s
  # list return value as if it were a data frame - the pipeline could not
  # run at all before these fixes.
  set.seed(1)
  n <- 80
  df <- data.frame(
    FarmID = paste0("F", seq_len(n)),
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20),
    Irrigation = sample(c("Drip", "Furrow", "Sprinkler"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df$Nitrogen[sample(n, 5)] <- NA

  result <- NULL
  expect_no_error(
    result <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
  )
  expect_true(is.data.frame(result$data))
  expect_true(nrow(result$data) > 0)
})

test_that("prep_yield_gap() never scales or renames the response variable", {
  # Regression test for the critical bug where the response variable was
  # scaled like any other numeric column and renamed to "Yield_scaled".
  set.seed(2)
  n <- 60
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20)
  )
  result <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
  expect_true("Yield" %in% names(result$data))
  expect_false("Yield_scaled" %in% names(result$data))
  expect_false("Yield" %in% names(result$scaling_params))
})

test_that("prep_yield_gap() removes non-predictor ID columns", {
  set.seed(3)
  n <- 40
  df <- data.frame(
    FarmID = paste0("F", seq_len(n)),
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20)
  )
  result <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
  expect_false("FarmID" %in% names(result$data))
  expect_true("FarmID" %in% result$removed_vars$non_predictors)
})

test_that("prep_yield_gap() correctly encodes a real nominal predictor", {
  # Would previously fail because of the get_top_levels()/order() bug.
  set.seed(4)
  n <- 80
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  result <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
  expect_true(any(grepl("^Region_", names(result$data))))
})

test_that("prep_yield_gap() output is usable directly in lm()", {
  set.seed(5)
  n <- 80
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20),
    Irrigation = sample(c("Drip", "Furrow"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  result <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
  expect_no_error(model <- lm(Yield ~ ., data = result$data))
})

test_that("prep_yield_gap() does not scale binary/dummy variables by default", {
  # Regression test for the bug where binary (0/1) and one-hot dummy
  # columns were standardized just like continuous variables, which made
  # calculate_opt_values() compute a nonsensical "optimal" value (literal
  # 1 or 0) for a column whose real two states were no longer 0 and 1.
  set.seed(7)
  n <- 80
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20),
    Irrigated = sample(c(0L, 1L), n, replace = TRUE),
    Cultivar = sample(c("Azar", "Sardari", "Marvdasht"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  result <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)

  expect_true("Irrigated" %in% names(result$data))
  expect_false("Irrigated_scaled" %in% names(result$data))
  expect_true(any(grepl("^Cultivar_", names(result$data))))
  expect_false(any(grepl("^Cultivar_.*_scaled$", names(result$data))))
  # Nitrogen (genuinely continuous) should still be scaled as before.
  expect_true("Nitrogen_scaled" %in% names(result$data))
})

test_that("prep_yield_gap() with NZV removal enabled uses correctly-scaled thresholds", {
  # Regression test for the unit mismatch between auto_tune_params()
  # (which used to produce proportions like 0.95) and
  # remove_near_zero_variance() (which requires freq_cut as a ratio > 1
  # and unique_cut as a percentage in (0, 100)).
  set.seed(6)
  n <- 100
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20),
    RareFlag = c(rep(0, 98), 1, 1)
  )
  expect_no_error(
    result <- prep_yield_gap(df, response_var = "Yield",
                                        remove_nzv = TRUE, verbose = FALSE)
  )
  # RareFlag is detected as binary, and binary/dummy columns are excluded
  # from scaling by default (see scale_variables()/pipeline.R), so it keeps
  # its original (unscaled) name all the way through to NZV filtering.
  expect_true("RareFlag" %in% result$removed_vars$nzv)
})

test_that("prep_yield_gap() errors clearly when response_var is missing from data", {
  df <- data.frame(Nitrogen = rnorm(20, 120, 20))
  expect_error(
    prep_yield_gap(df, response_var = "Yield", verbose = FALSE),
    "Yield"
  )
})

test_that("validate_preprocessing() returns a consistent schema even on early-return error paths", {
  # Regression test: early-return paths used to omit has_encoding/
  # has_scaling entirely, causing `if (val$has_encoding)` to error with
  # "argument is of length zero" downstream.
  bad_result <- list(data = "not a data frame", response_var = "Yield", var_types = list())
  val <- validate_preprocessing(bad_result, verbose = FALSE)
  expect_false(val$valid)
  expect_false(val$has_encoding)
  expect_false(val$has_scaling)
})

test_that("yield_gap_analysis() prints step banners with numbers matching internal step order", {
  # Regression test: cross-validation previously had no numbered banner at
  # all, and the relative-importance banner printed "STEP 8" even though
  # the code's own internal comment (and logical position after CV) says
  # "STEP 9" - a real, findable inconsistency in verbose output.
  set.seed(1)
  n <- 100
  Nitrogen <- rnorm(n, 120, 20)
  Phosphorus <- rnorm(n, 40, 10)
  # Strong, low-noise signal so both variables are reliably significant
  # (required for the CV and relative-importance steps to actually run).
  Yield <- 3000 + 15 * Nitrogen + 20 * Phosphorus + rnorm(n, 0, 100)
  df <- data.frame(Yield = Yield, Nitrogen = Nitrogen, Phosphorus = Phosphorus)
  prep <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)

  out <- capture.output(
    result <- yield_gap_analysis(prep$data, response = "Yield", verbose = TRUE, cv_seed = 1)
  )
  full_text <- paste(out, collapse = "\n")

  expect_true(grepl("STEP 7: CROSS-VALIDATION", full_text))
  expect_true(grepl("STEP 9: CALCULATING RELATIVE IMPORTANCE", full_text))
  expect_false(grepl("STEP 8: CALCULATING RELATIVE IMPORTANCE", full_text))
})
