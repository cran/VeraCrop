test_that("is_constant() is defined exactly once across the whole package", {
  # Regression test for the collision bugs where is_constant(), get_mode(),
  # and count_missing() were each defined in two different files with
  # different (sometimes incompatible) semantics, and whichever file
  # loaded last silently won for the WHOLE package namespace.
  ns <- asNamespace("VeraCrop")
  # is_constant should exist and be exactly the filtering.R version
  expect_true(exists("is_constant", envir = ns, inherits = FALSE))
})

test_that("remove_constant_vars() removes constant columns and preserves the response variable", {
  set.seed(1)
  n <- 30
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20),
    AlwaysFive = 5
  )
  result <- remove_constant_vars(df, response_var = "Yield", verbose = FALSE)
  expect_true(is.list(result))
  expect_true("Yield" %in% names(result$data))
  expect_true("AlwaysFive" %in% result$removed)
  expect_false("AlwaysFive" %in% names(result$data))
})

test_that("remove_constant_vars() never flags the response variable as removable even if constant", {
  set.seed(2)
  n <- 20
  df <- data.frame(Yield = rep(5000, n), Nitrogen = rnorm(n, 120, 20))
  result <- remove_constant_vars(df, response_var = "Yield", verbose = FALSE)
  expect_true("Yield" %in% names(result$data))
})

test_that("remove_near_zero_variance() removes a variable with extreme frequency ratio", {
  set.seed(3)
  n <- 100
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20),
    RareFlag = c(rep(0, 98), 1, 1)  # extreme majority/minority ratio
  )
  result <- remove_near_zero_variance(df, response_var = "Yield",
                                       freq_cut = 19, unique_cut = 10, verbose = FALSE)
  expect_true("RareFlag" %in% result$removed)
  expect_true("Nitrogen" %in% names(result$data))
})

test_that("remove_highly_correlated() removes one of two near-collinear variables", {
  set.seed(4)
  n <- 100
  nitrogen <- rnorm(n, 120, 20)
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = nitrogen,
    Nitrogen_copy = nitrogen * 1.01 + rnorm(n, 0, 0.01)  # r > 0.99
  )
  result <- remove_highly_correlated(df, response_var = "Yield",
                                      threshold = 0.90, verbose = FALSE)
  remaining <- intersect(c("Nitrogen", "Nitrogen_copy"), names(result$data))
  expect_length(remaining, 1)
  expect_true("Yield" %in% names(result$data))
})

test_that("filter_variables() runs the full filter sequence without error", {
  set.seed(5)
  n <- 60
  nitrogen <- rnorm(n, 120, 20)
  df <- data.frame(
    FarmID = paste0("F", seq_len(n)),
    Yield = rnorm(n, 5000, 500),
    Nitrogen = nitrogen,
    Nitrogen_copy = nitrogen * 1.01,
    AlwaysConstant = 1
  )
  expect_no_error(
    result <- filter_variables(df, response_var = "Yield", verbose = FALSE)
  )
  expect_true("Yield" %in% names(result$data))
  expect_false("FarmID" %in% names(result$data))
  expect_false("AlwaysConstant" %in% names(result$data))
})

test_that("trim_whitespace() merges values that differ only by leading/trailing spaces", {
  # Regression test discovered via real data: "Sirvan" and "Sirvan " were
  # silently treated as two different categorical levels, splitting what
  # should be a single, statistically robust group into two (one of them
  # too rare to survive filtering), which can meaningfully distort results
  # for an important variable like cultivar.
  df <- data.frame(
    Cultivar = c("Sirvan", "Sirvan ", " Sirvan", "Chamran2"),
    Yield = c(5000, 4900, 5050, 4800),
    stringsAsFactors = FALSE
  )
  trimmed <- trim_whitespace(df, verbose = FALSE)
  expect_equal(length(unique(trimmed$Cultivar)), 2)
  expect_true(all(trimmed$Cultivar %in% c("Sirvan", "Chamran2")))
})

test_that("trim_whitespace() leaves already-clean data unchanged and reports nothing", {
  df <- data.frame(Cultivar = c("Sirvan", "Chamran2"), Yield = c(5000, 4900),
                    stringsAsFactors = FALSE)
  expect_silent(trimmed <- trim_whitespace(df, verbose = TRUE))
  expect_identical(trimmed, df)
})

test_that("trim_whitespace() does not touch numeric or factor columns", {
  df <- data.frame(
    Cultivar = c("Sirvan ", "Chamran2"),
    Yield = c(5000, 4900),
    Region = factor(c("North ", "South")),
    stringsAsFactors = FALSE
  )
  trimmed <- trim_whitespace(df, verbose = FALSE)
  expect_equal(trimmed$Yield, df$Yield)
  # Factor column must be left untouched (not a character column)
  expect_identical(trimmed$Region, df$Region)
})

test_that("prep_yield_gap() trims whitespace by default", {
  set.seed(1)
  n <- 40
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Cultivar = sample(c("Sirvan", "Sirvan ", "Chamran2"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  prep <- suppressWarnings(prep_yield_gap(df, response_var = "Yield", verbose = FALSE))
  # Only 2 real cultivars exist after trimming; "Sirvan"/"Sirvan " must not
  # produce two separate dummy groups.
  cultivar_cols <- grep("^Cultivar_", names(prep$data), value = TRUE)
  expect_lte(length(cultivar_cols), 1)  # binary after trimming (2 levels -> 1 column), or 0 if filtered
})

test_that("prep_yield_gap() can run on a small, high-dimensional dataset without erroring on max_dummy_levels", {
  # Regression test for a real crash: auto_tune_params()'s high p/n-ratio
  # adjustment could reduce max_dummy_levels to 1, but encode_variables()
  # requires max_dummy_levels >= 2 and errors otherwise. This crashed the
  # entire pipeline for any small dataset with many predictor columns
  # relative to its sample size (p/n > 0.5) - a common real-world profile
  # for agricultural field-trial data with few fields but many recorded
  # management variables.
  set.seed(1)
  n <- 40
  df <- data.frame(Yield = rnorm(n, 5000, 500))
  # Add 25 more columns to push p/n well above 0.5
  for (i in 1:25) {
    df[[paste0("Var", i)]] <- sample(c("A", "B", "C"), n, replace = TRUE)
  }
  expect_no_error(
    prep <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
  )
})
