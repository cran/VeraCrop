test_that("calculate_opt_values() uses actual Max/Min for an unscaled 0/1 binary variable", {
  set.seed(1)
  n <- 60
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Irrigated = sample(c(0L, 1L), n, replace = TRUE)
  )
  df_top <- df[df$Yield >= stats::quantile(df$Yield, 0.8), ]
  coefs <- c(Irrigated = 250)  # positive coefficient

  # A small top-percentile sample size (< min_top_samples) triggers an
  # informational "adjusting top percentile" warning - expected here, not
  # a test failure.
  result <- suppressWarnings(calculate_opt_values(
    data = df, vars = "Irrigated", coefs = coefs, data_top = df_top,
    response_var = "Yield"
  ))
  vt <- result$var_table

  expect_equal(vt$Opt[vt$Variable == "Irrigated"], 1)
  expect_equal(vt$Opt_Method[vt$Variable == "Irrigated"], "binary_positive_1")
})

test_that("calculate_opt_values() correctly handles a STANDARDIZED binary/dummy variable", {
  # Regression test for the bug where Opt was hardcoded to the literal
  # value 1 (or 0), which is wrong once the binary column has been
  # standardized (e.g. "Cultivar_Sardari_scaled") - the two real states
  # are no longer 0 and 1, they are whatever (x - center) / scale maps
  # them to.
  set.seed(2)
  n <- 60
  raw <- sample(c(0L, 1L), n, replace = TRUE)
  scaled <- (raw - mean(raw)) / stats::sd(raw)  # simulate standardization

  df <- data.frame(Yield = rnorm(n, 5000, 500), Cultivar_Sardari_scaled = scaled)
  df_top <- df[df$Yield >= stats::quantile(df$Yield, 0.8), ]
  coefs <- c(Cultivar_Sardari_scaled = 300)

  result <- suppressWarnings(calculate_opt_values(
    data = df, vars = "Cultivar_Sardari_scaled", coefs = coefs, data_top = df_top,
    response_var = "Yield"
  ))
  vt <- result$var_table

  actual_positive_state <- max(scaled)
  expect_equal(vt$Opt[1], actual_positive_state)
  # The old, buggy behavior would have produced exactly 1 here, which for a
  # standardized variable is almost certainly wrong.
  expect_false(isTRUE(all.equal(vt$Opt[1], 1)))
})

test_that("calculate_opt_values() picks Min for a binary variable with a negative coefficient", {
  set.seed(3)
  n <- 60
  raw <- sample(c(0L, 1L), n, replace = TRUE)
  scaled <- (raw - mean(raw)) / stats::sd(raw)
  df <- data.frame(Yield = rnorm(n, 5000, 500), Pest_scaled = scaled)
  df_top <- df[df$Yield >= stats::quantile(df$Yield, 0.8), ]
  coefs <- c(Pest_scaled = -180)  # negative coefficient: pest hurts yield

  result <- suppressWarnings(calculate_opt_values(
    data = df, vars = "Pest_scaled", coefs = coefs, data_top = df_top,
    response_var = "Yield"
  ))
  vt <- result$var_table

  expect_equal(vt$Opt[1], min(scaled))
})

test_that("calculate_opt_values() no longer misclassifies a continuous variable via name pattern", {
  # Regression test for a bug that surfaced only via real data
  # (Wheat.csv): a hardcoded fallback grepl("_Infestation$", v) forced ANY
  # variable ending in "_Infestation" into binary Max/Min treatment,
  # regardless of how many distinct values it actually had. In the real
  # dataset, "Weed_Infestation" is a genuinely continuous count (dozens of
  # distinct values) and must be treated as continuous.
  set.seed(4)
  n <- 100
  weed <- sample(1:60, n, replace = TRUE)
  df <- data.frame(Yield = rnorm(n, 5000, 500), Weed_Infestation = weed)
  df_top <- df[df$Yield >= stats::quantile(df$Yield, 0.8), ]
  coefs <- c(Weed_Infestation = -20)

  result <- suppressWarnings(calculate_opt_values(
    data = df, vars = "Weed_Infestation", coefs = coefs, data_top = df_top,
    response_var = "Yield"
  ))
  vt <- result$var_table

  expect_identical(vt$VarType[1], "continuous")
  expect_identical(vt$Opt_Method[1], "continuous_negative_top_mean")
})

test_that("pareto_analysis() respects verbose = FALSE", {
  # Regression test: pareto_analysis() previously had no verbose parameter
  # at all and always printed to the console, even when
  # yield_gap_analysis(..., verbose = FALSE) was requested.
  cpa_table <- data.frame(
    Variable = c("Intercept", "A", "B", "C"),
    Beta = c(5000, 100, -50, 30),
    Mean = c(1, 10, 20, 5),
    Opt = c(1, 15, 10, 8),
    stringsAsFactors = FALSE
  )
  yg <- yield_gap_components(cpa_table)

  expect_silent(pareto_analysis(yg, verbose = FALSE))
  expect_output(pareto_analysis(yg, verbose = TRUE), "Pareto Analysis")
})

test_that("pareto_analysis() handles top_n = 0 without the 1:0 pitfall", {
  cpa_table <- data.frame(
    Variable = c("Intercept", "A", "B"),
    Beta = c(5000, 100, -50),
    Mean = c(1, 10, 20),
    Opt = c(1, 15, 10),
    stringsAsFactors = FALSE
  )
  yg <- yield_gap_components(cpa_table)
  result <- pareto_analysis(yg, top_n = 0, verbose = FALSE)
  expect_identical(result$top_variables, character(0))
})
