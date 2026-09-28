test_that("validate_model_with_cv() restores the caller's RNG state after running", {
  # Regression test for the original bug: set.seed(123) was hardcoded
  # inside the function with no restoration, permanently altering the
  # user's random number stream on every call.
  set.seed(555)
  before <- .Random.seed

  n <- 60
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  model <- lm(Yield ~ Nitrogen, data = df)

  invisible(validate_model_with_cv(model, df, sig_vars = "Nitrogen",
                                    response = "Yield", k_folds = 3, seed = 42))

  after <- .Random.seed
  expect_false(identical(before, after))  # RNG did advance (sample() was called)

  # But the state after the call must be a legitimate continuation of the
  # PRE-call stream, not reset to whatever seed=42 would produce. We verify
  # this indirectly: replaying the exact same sequence of operations that
  # happened before the call, then comparing a fresh draw.
  set.seed(555)
  draw_expected <- runif(1)  # nothing else consumed randomness yet at this point
  set.seed(555)
  invisible(validate_model_with_cv(model, df, sig_vars = "Nitrogen",
                                    response = "Yield", k_folds = 3, seed = 42))
  draw_actual <- runif(1)
  expect_equal(draw_expected, draw_actual)
})

test_that("validate_model_with_cv() fold assignment is reproducible when seed is supplied", {
  n <- 60
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  model <- lm(Yield ~ Nitrogen, data = df)

  r1 <- validate_model_with_cv(model, df, sig_vars = "Nitrogen",
                                response = "Yield", k_folds = 3, seed = 11)
  r2 <- validate_model_with_cv(model, df, sig_vars = "Nitrogen",
                                response = "Yield", k_folds = 3, seed = 11)

  expect_equal(r1$summary, r2$summary)
})

test_that("validate_model_with_cv() validates the model argument's response variable", {
  n <- 60
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  other_model <- lm(Nitrogen ~ Yield, data = df)  # response is "Nitrogen", not "Yield"

  expect_warning(
    validate_model_with_cv(other_model, df, sig_vars = "Nitrogen",
                            response = "Yield", k_folds = 3, seed = 1),
    "response"
  )
})

test_that("validate_model_with_cv() accepts model = NULL and skips the consistency check", {
  n <- 60
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  expect_no_warning(
    res <- validate_model_with_cv(NULL, df, sig_vars = "Nitrogen",
                                   response = "Yield", k_folds = 3, seed = 1)
  )
  expect_true(is.list(res))
})

test_that("select_vars_aic() runs and returns a schema compatible with select_vars_ftest()", {
  set.seed(1)
  n <- 60
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20),
    Phosphorus = rnorm(n, 40, 10)
  )
  df$Yield <- df$Yield + 15 * df$Nitrogen + 20 * df$Phosphorus

  sel <- select_vars_aic(df, response = "Yield", verbose = FALSE)
  expect_true(is.list(sel))
  expect_true(all(c("final_model", "significant_vars", "step_history", "method") %in% names(sel)))
  expect_identical(sel$method, "aic")
  expect_true(inherits(sel$final_model, "lm"))
})

test_that("select_vars_aic() drops aliased (perfectly collinear) predictors with a warning instead of erroring", {
  set.seed(2)
  n <- 40
  x <- rnorm(n, 100, 10)
  df <- data.frame(Yield = rnorm(n, 5000, 500), X1 = x, X2 = x)  # X2 is a perfect copy of X1
  expect_warning(
    sel <- select_vars_aic(df, response = "Yield", verbose = FALSE),
    "collinear"
  )
  expect_true(is.list(sel))
})

test_that("select_vars_aic() significant_vars order matches the final model's coefficient order", {
  set.seed(3)
  n <- 80
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    A = rnorm(n, 100, 10),
    B = rnorm(n, 50, 5),
    C = rnorm(n, 20, 2)
  )
  df$Yield <- df$Yield + 10 * df$A + 8 * df$B + 5 * df$C

  sel <- select_vars_aic(df, response = "Yield", direction = "both", verbose = FALSE)
  coef_names <- names(coef(sel$final_model))
  coef_names <- coef_names[coef_names != "(Intercept)"]
  expect_identical(sel$significant_vars, coef_names)
})

test_that("yield_gap_analysis() supports selection_method = 'aic'", {
  set.seed(4)
  n <- 60
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Nitrogen = rnorm(n, 120, 20),
    Phosphorus = rnorm(n, 40, 10)
  )
  df$Yield <- df$Yield + 15 * df$Nitrogen + 20 * df$Phosphorus
  prep <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)

  result <- yield_gap_analysis(prep$data, response = "Yield",
                                selection_method = "aic", verbose = FALSE)
  expect_true(is.list(result))
  expect_true(!is.null(result$cpa_table))
})

test_that("yield_gap_analysis() rejects an invalid selection_method", {
  df <- data.frame(Yield = rnorm(20, 5000, 500), Nitrogen = rnorm(20, 120, 20))
  expect_error(
    yield_gap_analysis(df, response = "Yield", selection_method = "stepwise_magic", verbose = FALSE)
  )
})

test_that("select_vars_ftest() gracefully handles a perfectly collinear (aliased) predictor", {
  # Regression test discovered via real data (a "total" column that is the
  # exact sum of other already-included component columns, e.g. total
  # fertilizer = sum of individual application timings - a very common
  # pattern in agricultural input data). The forward-selection loop must
  # not error or select NA-coefficient variables when a candidate variable
  # is a perfect linear combination of variables already selected.
  set.seed(1)
  n <- 100
  a <- rnorm(n, 50, 10)
  b <- rnorm(n, 30, 5)
  c <- rnorm(n, 20, 3)
  total <- a + b + c
  df <- data.frame(
    Yield = 3000 + 2 * a + 3 * b + 1.5 * c + rnorm(n, 0, 50),
    A = a, B = b, C = c, Total = total
  )

  sel <- suppressWarnings(select_vars_ftest(df, response = "Yield", verbose = FALSE))
  expect_true(is.list(sel))
  expect_false(any(is.na(coef(sel$final_model))))
})
