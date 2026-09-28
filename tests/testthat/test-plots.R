test_that("plot_all_graphs() respects verbose = FALSE", {
  skip_if_not_installed("ggplot2")

  set.seed(1)
  n <- 40
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  model <- lm(Yield ~ Nitrogen, data = df)
  preds <- stats::predict(model)
  results <- list(
    model = model,
    cpa_table = data.frame(Variable = c("Intercept", "Nitrogen"),
                            Share_Percent_Abs = c(NA, 100),
                            Share_Cumulative = c(NA, 100)),
    predictions = data.frame(Observed = df$Yield, Predicted = preds,
                              Yield_Gap_Obs = df$Yield - preds),
    var_table = data.frame(Variable = "Nitrogen", Opt = max(df$Nitrogen),
                            Min = min(df$Nitrogen), Max = max(df$Nitrogen),
                            Mean = mean(df$Nitrogen)),
    minmax_results = data.frame(Variable = "Nitrogen", Effect = 10,
                                 Direction = "Positive"),
    # Column name must match what calculate_relative_importance() actually
    # produces for the std_beta fallback path: "RelImportancePct".
    relative_importance = list(results = data.frame(Variable = "Nitrogen",
                                                      RelImportancePct = 100)),
    metrics = list(R2 = summary(model)$r.squared, RMSE = 100),
    yield_mean = mean(df$Yield), yield_opt = max(df$Yield),
    yield_gap = max(df$Yield) - mean(df$Yield)
  )

  pdf(NULL)  # suppress actual plot device output during the test
  on.exit(dev.off())

  expect_no_error(
    out <- plot_all_graphs(results, verbose = FALSE)
  )
  expect_true(is.list(out))
  expect_false(is.null(out$p5))
})

test_that("plot_all_graphs() degrades gracefully when one plot is unavailable", {
  # Regression test for a bug found via testing: if relative_importance
  # data doesn't match the expected column names (or is missing/NULL),
  # plot_relative_importance() returns NULL, and combining a NULL with
  # `+ patchwork::plot_layout(...)` used to crash with a confusing
  # "non-numeric argument to binary operator" error instead of degrading
  # gracefully.
  skip_if_not_installed("ggplot2")

  set.seed(2)
  n <- 40
  df <- data.frame(Yield = rnorm(n, 5000, 500), Nitrogen = rnorm(n, 120, 20))
  model <- lm(Yield ~ Nitrogen, data = df)
  preds <- stats::predict(model)
  results <- list(
    model = model,
    cpa_table = data.frame(Variable = c("Intercept", "Nitrogen"),
                            Share_Percent_Abs = c(NA, 100),
                            Share_Cumulative = c(NA, 100)),
    predictions = data.frame(Observed = df$Yield, Predicted = preds,
                              Yield_Gap_Obs = df$Yield - preds),
    var_table = data.frame(Variable = "Nitrogen", Opt = max(df$Nitrogen),
                            Min = min(df$Nitrogen), Max = max(df$Nitrogen),
                            Mean = mean(df$Nitrogen)),
    minmax_results = data.frame(Variable = "Nitrogen", Effect = 10,
                                 Direction = "Positive"),
    relative_importance = NULL,  # deliberately unavailable
    metrics = list(R2 = summary(model)$r.squared, RMSE = 100),
    yield_mean = mean(df$Yield), yield_opt = max(df$Yield),
    yield_gap = max(df$Yield) - mean(df$Yield)
  )

  pdf(NULL)
  on.exit(dev.off())

  expect_no_error(
    out <- plot_all_graphs(results, verbose = FALSE)
  )
  expect_true(is.null(out$p5))
  # The other four plots must still have been produced successfully.
  expect_false(is.null(out$p1))
  expect_false(is.null(out$p4))
})
