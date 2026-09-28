test_that("get_top_levels() does not error on nominal variables with 3+ levels", {
  # Regression test for: Error in order(...) : 'decreasing' of length > 1 is
  # only for method = "radix". This affected essentially every real
  # categorical variable with more than 2 levels, discovered only via an
  # end-to-end run - encode_nominal() and encode_high_cardinality() both
  # depend on this helper.
  x <- factor(sample(c("Drip", "Furrow", "Sprinkler"), 60, replace = TRUE))

  result <- NULL
  expect_no_error(result <- VeraCrop:::get_top_levels(x, n = 2))
  expect_true(is.character(result))
  expect_lte(length(result), 2)
})

test_that("get_top_levels() handles tied frequencies deterministically", {
  # Equal counts for all three levels - ties must be broken by name, not by
  # whatever order table() happens to produce.
  x <- factor(rep(c("A", "B", "C"), each = 10))
  result1 <- VeraCrop:::get_top_levels(x, n = 2)
  result2 <- VeraCrop:::get_top_levels(rev(x), n = 2)
  expect_identical(result1, result2)
})

test_that("encode_variables() successfully encodes a 3-level nominal predictor", {
  # End-to-end check that a realistic nominal variable actually produces
  # dummy columns instead of being silently dropped via the tryCatch
  # fallback in encode_variables().
  set.seed(1)
  n <- 60
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Irrigation = sample(c("Drip", "Furrow", "Sprinkler"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  var_types <- detect_variable_types(df, response_var = "Yield", verbose = FALSE)
  result <- encode_variables(df, var_types, response_var = "Yield", verbose = FALSE)

  expect_length(result$dropped_vars, 0)
  expect_true(any(grepl("^Irrigation_", names(result$data))))
  expect_true("Yield" %in% names(result$data))
})

test_that("resolve_reference_level() honors info$reference before falling back to mode", {
  levels_vec <- c("A", "B", "C")
  x <- factor(c(rep("A", 2), rep("B", 5), rep("C", 3)), levels = levels_vec)

  # info$reference explicitly says "C" (e.g. chosen via reference_method =
  # "alphabetical" upstream) even though "B" is the mode - the explicit
  # choice must win.
  info <- list(reference = "C")
  ref <- VeraCrop:::resolve_reference_level(info, x, levels_vec, "test_var")
  expect_identical(ref, "C")
})

test_that("resolve_reference_level() falls back to mode when info$reference is invalid", {
  levels_vec <- c("A", "B", "C")
  x <- factor(c(rep("A", 2), rep("B", 5), rep("C", 3)), levels = levels_vec)

  info <- list(reference = "NotALevel")
  expect_warning(
    ref <- VeraCrop:::resolve_reference_level(info, x, levels_vec, "test_var"),
    NA  # no warning expected here: mode fallback succeeds silently
  )
  expect_identical(ref, "B")
})

test_that("encode_variables() never silently drops a column on name collision", {
  # Regression test for the bug where a renamed (collision-avoided) output
  # column was read back from result$data using its NEW name, which did not
  # exist there, silently dropping the column while encoding_info still
  # claimed success.
  set.seed(2)
  n <- 40
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    # Two variables whose dummy dispatch could plausibly produce the same
    # cleaned column name after make.names() sanitization.
    Region_A = sample(c("X", "Y"), n, replace = TRUE),
    Region.A = sample(c("X", "Y"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  var_types <- detect_variable_types(df, response_var = "Yield", verbose = FALSE)
  result <- suppressWarnings(
    encode_variables(df, var_types, response_var = "Yield", verbose = FALSE)
  )

  # Every column listed in encoding_info$*$encoded must actually exist in
  # the returned data and contain non-NULL values.
  for (var in names(result$encoding_info)) {
    encoded_cols <- result$encoding_info[[var]]$encoded
    for (col in encoded_cols) {
      expect_true(col %in% names(result$data),
                  info = sprintf("Column '%s' (from variable '%s') missing from encoded data", col, var))
    }
  }
})
