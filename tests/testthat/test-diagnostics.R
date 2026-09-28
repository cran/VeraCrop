test_that("check_assumptions() computes data_ready identically whether verbose is TRUE or FALSE", {
  # Regression test for the original bug where passed/total/data_ready were
  # only computed inside `if (verbose)` blocks, causing verbose = FALSE
  # calls to error or silently return NULL/NA for these fields.
  set.seed(1)
  n <- 60
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  model <- lm(Yield ~ Nitrogen, data = df)

  res_quiet <- check_assumptions(model, plot = FALSE, verbose = FALSE)
  res_loud  <- suppressWarnings(capture.output(
    res_loud_val <- check_assumptions(model, plot = FALSE, verbose = TRUE)
  ))

  expect_false(is.na(res_quiet$summary$data_ready))
  expect_identical(res_quiet$data_ready, res_loud_val$data_ready)
  expect_identical(res_quiet$summary$passed, res_loud_val$summary$passed)
})

test_that("check_assumptions() Shapiro-Wilk sampling is reproducible via the seed parameter", {
  set.seed(99)
  n <- 6000  # forces the > 5000 sampling branch
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  model <- lm(Yield ~ Nitrogen, data = df)

  r1 <- check_assumptions(model, plot = FALSE, verbose = FALSE, seed = 42)
  r2 <- check_assumptions(model, plot = FALSE, verbose = FALSE, seed = 42)

  expect_equal(r1$normality$p_value, r2$normality$p_value)
})

test_that("check_assumptions() does not permanently alter the caller's RNG stream", {
  # Regression/consistency test: check_assumptions() uses set.seed(seed)
  # internally (for reproducible Shapiro-Wilk sampling on large residual
  # vectors) but must save and restore the caller's global RNG state
  # around that, matching the same pattern used in validate_model_with_cv().
  n <- 6000
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  model <- lm(Yield ~ Nitrogen, data = df)

  set.seed(123)
  draw_without_call <- runif(1)

  set.seed(123)
  invisible(check_assumptions(model, plot = FALSE, verbose = FALSE, seed = 7))
  draw_after_call <- runif(1)

  expect_equal(draw_without_call, draw_after_call)
})

test_that("check_assumptions() reports honest FarmID mapping status when data is unaligned", {
  set.seed(1)
  n <- 30
  df <- data.frame(
    FarmID = paste0("F", seq_len(n)),
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20)
  )
  model <- lm(Yield ~ Nitrogen, data = df)

  # Pass a completely mismatched data frame (wrong number of rows, no NA
  # handling involved) - this must be flagged as critical and must NOT
  # produce influential-point FarmIDs that look valid but are wrong.
  bad_data <- df[1:10, ]
  expect_warning(
    res <- check_assumptions(model, data = bad_data, plot = FALSE, verbose = FALSE),
    "CRITICAL"
  )
  expect_true("Data-model alignment issue" %in% res$critical_issues)
  expect_false(isTRUE(res$data_ready))
})

test_that("check_assumptions() correctly maps influential-point FarmIDs when data is aligned", {
  set.seed(2)
  n <- 40
  df <- data.frame(
    FarmID = paste0("Farm_", seq_len(n)),
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20)
  )
  # Inject one extreme outlier to guarantee at least one influential point
  df$Yield[1] <- df$Yield[1] + 10000

  model <- lm(Yield ~ Nitrogen, data = df)
  res <- check_assumptions(model, data = df, plot = FALSE, verbose = FALSE)

  expect_true(!is.null(res$influential_points))
  expect_identical(res$influential_points$cooks_distance$mapping_status, "direct_aligned")
})

test_that("model_diagnostics() returns both a model summary and assumption checks", {
  set.seed(11)
  n <- 60
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  model <- lm(Yield ~ Nitrogen, data = df)

  result <- model_diagnostics(model, data = df, plot = FALSE, verbose = FALSE)

  expect_true(is.list(result))
  expect_true(all(c("summary", "assumptions") %in% names(result)))
  expect_s3_class(result$summary, "summary.lm")
  expect_equal(result$summary$r.squared, summary(model)$r.squared)
  expect_true(is.list(result$assumptions))
})

test_that("model_diagnostics() rejects non-lm models", {
  expect_error(
    model_diagnostics(list(a = 1), plot = FALSE, verbose = FALSE),
    "'model' must be an object of class 'lm'"
  )
})
