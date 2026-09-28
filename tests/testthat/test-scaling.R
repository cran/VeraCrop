test_that("scale_variables() never scales a variable listed in exclude_vars", {
  # Regression test for the critical bug where scale_variables() had no way
  # to protect the response variable: with vars = NULL (auto-detect all
  # numeric columns), "Yield" itself was silently standardized and renamed
  # to "Yield_scaled", breaking every downstream step that referenced it by
  # its original name.
  set.seed(1)
  df <- data.frame(
    Yield = rnorm(50, 5000, 500),
    Nitrogen = rnorm(50, 120, 20)
  )

  result <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)

  expect_true("Yield" %in% names(result$data))
  expect_false("Yield_scaled" %in% names(result$data))
  expect_true("Nitrogen_scaled" %in% names(result$data))
  expect_false("Yield" %in% names(result$scaling_params))
})

test_that("scale_variables() with vars = NULL scales all numeric columns when exclude_vars is NULL", {
  # Confirms default (no exclusion) behavior is unchanged for callers that
  # don't need to protect any column.
  set.seed(2)
  df <- data.frame(
    Yield = rnorm(30, 5000, 500),
    Nitrogen = rnorm(30, 120, 20)
  )
  result <- scale_variables(df, verbose = FALSE)
  expect_true("Yield_scaled" %in% names(result$data))
  expect_true("Nitrogen_scaled" %in% names(result$data))
})

test_that("scale_variables() warns if a variable is in both vars and exclude_vars", {
  set.seed(3)
  df <- data.frame(Yield = rnorm(20, 5000, 500), Nitrogen = rnorm(20, 120, 20))
  expect_warning(
    result <- scale_variables(df, vars = c("Yield", "Nitrogen"),
                               exclude_vars = "Yield", verbose = FALSE),
    "exclude_vars"
  )
  expect_false("Yield_scaled" %in% names(result$data))
  expect_true("Nitrogen_scaled" %in% names(result$data))
})

test_that("back_transform_coefficients() correctly rescales X-only-standardized coefficients", {
  set.seed(4)
  n <- 200
  x <- rnorm(n, 100, 10)
  y <- 5000 + 3 * x + rnorm(n, 0, 5)  # true slope on ORIGINAL scale = 3
  df <- data.frame(Yield = y, N_rate = x)

  scale_result <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)
  model <- lm(Yield ~ N_rate_scaled, data = scale_result$data)
  coefs_std <- stats::coef(model)

  back <- back_transform_coefficients(coefs_std, scale_result$scaling_params)

  # Back-transformed slope should recover the true original-scale slope
  # (~3), NOT the standardized coefficient.
  expect_equal(unname(back["N_rate_scaled"]), 3, tolerance = 0.5)
})

test_that("back_transform_coefficients() applies the Y-scaling correction when response_sd is supplied", {
  set.seed(5)
  n <- 200
  x <- rnorm(n, 100, 10)
  y <- 5000 + 3 * x + rnorm(n, 0, 5)
  df <- data.frame(Yield = y, N_rate = x)

  # Standardize BOTH X and Y this time (exclude_vars = NULL)
  scale_result <- scale_variables(df, verbose = FALSE)
  model <- lm(Yield_scaled ~ N_rate_scaled, data = scale_result$data)
  coefs_std <- stats::coef(model)

  response_sd <- scale_result$scaling_params[["Yield"]]$scale
  back <- back_transform_coefficients(coefs_std, scale_result$scaling_params,
                                       response_sd = response_sd)

  expect_equal(unname(back["N_rate_scaled"]), 3, tolerance = 0.5)
})

test_that("summary_scaling() summarizes each scaled variable's parameters", {
  set.seed(20)
  df <- data.frame(Yield = rnorm(30, 5000, 500), N_rate = rnorm(30, 100, 20))
  scaled <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)

  summary_df <- summary_scaling(scaled$scaling_params, verbose = FALSE)

  expect_true(is.data.frame(summary_df))
  expect_equal(nrow(summary_df), length(scaled$scaling_params))
  expect_true("N_rate" %in% summary_df$Variable)
  expect_equal(summary_df$Scaled_Name[summary_df$Variable == "N_rate"], "N_rate_scaled")
})

test_that("summary_scaling() handles an empty scaling_params list gracefully", {
  expect_null(summary_scaling(list(), verbose = FALSE))
})

test_that("back_transform_data() recovers the original variable values", {
  set.seed(21)
  df <- data.frame(Yield = rnorm(30, 5000, 500), N_rate = rnorm(30, 100, 20))
  scaled <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)

  restored <- back_transform_data(scaled$data, scaled$scaling_params)

  expect_true("N_rate" %in% names(restored))
  expect_equal(restored$N_rate, df$N_rate, tolerance = 1e-8)
})

test_that("back_transform_data() can drop the scaled columns when asked", {
  set.seed(22)
  df <- data.frame(Yield = rnorm(20, 5000, 500), N_rate = rnorm(20, 100, 20))
  scaled <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)

  restored <- back_transform_data(scaled$data, scaled$scaling_params, remove_scaled = TRUE)

  expect_false("N_rate_scaled" %in% names(restored))
  expect_true("N_rate" %in% names(restored))
})

test_that("scale_new_data() applies training scaling parameters to unseen data", {
  set.seed(23)
  train <- data.frame(Yield = rnorm(50, 5000, 500), N_rate = rnorm(50, 100, 20))
  scaled <- scale_variables(train, exclude_vars = "Yield", verbose = FALSE)
  params <- scaled$scaling_params[["N_rate"]]

  new_obs <- data.frame(N_rate = c(90, 100, 110))
  result <- scale_new_data(new_obs, scaled$scaling_params, verbose = FALSE)

  expect_true("N_rate_scaled" %in% names(result))
  expected <- (new_obs$N_rate - params$center) / params$scale
  expect_equal(result$N_rate_scaled, expected, tolerance = 1e-8)
})

test_that("scale_new_data() can keep the original unscaled columns", {
  set.seed(24)
  train <- data.frame(Yield = rnorm(30, 5000, 500), N_rate = rnorm(30, 100, 20))
  scaled <- scale_variables(train, exclude_vars = "Yield", verbose = FALSE)

  new_obs <- data.frame(N_rate = c(95, 105))
  result <- scale_new_data(new_obs, scaled$scaling_params, keep_original = TRUE, verbose = FALSE)

  expect_true(all(c("N_rate", "N_rate_scaled") %in% names(result)))
})

test_that("adjust_intercept() recovers the true original-scale intercept when only X is standardized", {
  set.seed(25)
  n <- 200
  x <- rnorm(n, 100, 10)
  y <- 5000 + 3 * x + rnorm(n, 0, 5)   # true intercept = 5000, true slope = 3
  df <- data.frame(Yield = y, N_rate = x)

  scaled <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)
  model <- lm(Yield ~ N_rate_scaled, data = scaled$data)
  back_coefs <- back_transform_coefficients(coef(model), scaled$scaling_params)

  adj_intercept <- adjust_intercept(
    unname(coef(model)["(Intercept)"]),
    back_coefs[names(back_coefs) != "(Intercept)"],
    scaled$scaling_params
  )

  expect_equal(unname(adj_intercept), 5000, tolerance = 5)
})
