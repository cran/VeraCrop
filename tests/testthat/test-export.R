test_that("save_results_excel() respects verbose = FALSE", {
  skip_if_not_installed("writexl")

  results <- list(
    cpa_table = data.frame(Variable = c("Intercept", "A"), Share_Percent_Abs = c(NA, 100)),
    metrics = list(R2 = 0.5, RMSE = 100, MAE = 80),
    yield_mean = 5000, yield_opt = 6000, yield_gap = 1000
  )
  tmp <- tempfile(fileext = ".xlsx")
  expect_silent(save_results_excel(results, output_path = tmp, verbose = FALSE))
  expect_true(file.exists(tmp))
  unlink(tmp)
})

test_that("save_field_level_excel() respects verbose = FALSE", {
  skip_if_not_installed("writexl")

  set.seed(1)
  n <- 20
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  model <- lm(Yield ~ Nitrogen, data = df)
  results <- list(
    model = model,
    predictions = data.frame(
      Observed = df$Yield, Predicted = stats::predict(model),
      Yield_Gap_Obs = df$Yield - stats::predict(model)
    ),
    yield_opt = max(df$Yield),
    var_table = data.frame(Variable = "Nitrogen", Opt = max(df$Nitrogen))
  )
  tmp <- tempfile(fileext = ".xlsx")
  expect_silent(save_field_level_excel(results, df, output_path = tmp, verbose = FALSE))
  expect_true(file.exists(tmp))
  unlink(tmp)
})
