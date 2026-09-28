test_that("bootstrap_cpa() returns valid CIs and matches yield_gap_analysis() point estimate when given its variables", {
  set.seed(42)
  n <- 80
  df <- data.frame(
    Yield = rnorm(n, 5000, 600),
    Irrigation = rnorm(n),
    Nitrogen = rnorm(n)
  )
  df$Yield <- df$Yield + 300 * df$Irrigation + 150 * df$Nitrogen

  # Recommended usage: reuse the exact variable set from an existing
  # yield_gap_analysis() run, so the bootstrap matches it precisely
  # regardless of any internal default differences in variable selection.
  direct <- yield_gap_analysis(df, response = "Yield", verbose = FALSE)
  sig_vars <- direct$cpa_table$Variable[direct$cpa_table$Variable != "Intercept"]

  boot <- bootstrap_cpa(
    df, response = "Yield", sig_vars = sig_vars, n_boot = 30,
    check_selection_stability = FALSE, seed = 1, verbose = FALSE
  )

  expect_type(boot, "list")
  expect_true(all(c("sig_vars", "summary", "boot_gap_component",
                     "boot_share_percent", "boot_yield_gap") %in% names(boot)))
  expect_s3_class(boot$summary, "data.frame")
  expect_true(all(c("Estimate", "Boot_Mean", "Boot_SE", "CI_Lower", "CI_Upper") %in% names(boot$summary)))
  expect_equal(nrow(boot$boot_gap_component), 30)
  expect_true(all(boot$summary$CI_Lower <= boot$summary$CI_Upper, na.rm = TRUE))

  gap_row <- boot$summary[boot$summary$Quantity == "Yield_Gap", ]
  expect_equal(gap_row$Estimate, direct$yield_gap, tolerance = 1e-6)

  # Regression test: Yield_Mean / Yield_Opt must include the intercept's
  # contribution and therefore be on the actual response scale, not near
  # zero (a past bug omitted the Intercept row from the internal cpa_table,
  # which silently zeroed out Yield_Mean/Yield_Opt while leaving Yield_Gap
  # -- and therefore Gap_Component/Share_Percent -- coincidentally correct,
  # since the intercept's own gap contribution is always zero).
  mean_row <- boot$summary[boot$summary$Quantity == "Yield_Mean", ]
  opt_row  <- boot$summary[boot$summary$Quantity == "Yield_Opt", ]
  expect_equal(mean_row$Estimate, direct$yield_mean, tolerance = 1e-6)
  expect_equal(opt_row$Estimate, direct$yield_opt, tolerance = 1e-6)
  expect_gt(mean_row$Estimate, mean(df$Yield) * 0.5)
})

test_that("bootstrap_cpa() respects a user-supplied sig_vars and seed reproducibility", {
  set.seed(7)
  n <- 60
  df <- data.frame(
    Yield = rnorm(n, 4000, 400),
    Drought = sample(c(0L, 1L), n, replace = TRUE),
    Pest = sample(c(0L, 1L), n, replace = TRUE)
  )
  df$Yield <- df$Yield - 200 * df$Drought - 150 * df$Pest

  b1 <- bootstrap_cpa(df, response = "Yield", sig_vars = c("Drought", "Pest"),
                       n_boot = 20, check_selection_stability = FALSE,
                       seed = 123, verbose = FALSE)
  b2 <- bootstrap_cpa(df, response = "Yield", sig_vars = c("Drought", "Pest"),
                       n_boot = 20, check_selection_stability = FALSE,
                       seed = 123, verbose = FALSE)

  expect_identical(b1$sig_vars, c("Drought", "Pest"))
  expect_equal(b1$boot_yield_gap, b2$boot_yield_gap)
})

test_that("bootstrap_cpa() reports selection_stability when requested", {
  set.seed(3)
  n <- 70
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Irrigation = rnorm(n),
    Noise1 = rnorm(n),
    Noise2 = rnorm(n)
  )
  df$Yield <- df$Yield + 400 * df$Irrigation

  boot <- bootstrap_cpa(df, response = "Yield", n_boot = 25,
                         check_selection_stability = TRUE, seed = 5, verbose = FALSE)

  expect_false(is.null(boot$selection_stability))
  expect_true(all(c("Variable", "Times_Selected", "Selection_Rate") %in% names(boot$selection_stability)))
  expect_true(all(boot$selection_stability$Selection_Rate >= 0 &
                     boot$selection_stability$Selection_Rate <= 100))
})

test_that("bootstrap_cpa() errors informatively on bad input", {
  df <- data.frame(Yield = rnorm(30, 5000, 500), X = rnorm(30))
  expect_error(bootstrap_cpa(df, response = "NotAColumn"), "not found")
  expect_error(bootstrap_cpa(df, response = "Yield", sig_vars = "NotAVar"), "not columns")
  expect_error(bootstrap_cpa(list(a = 1)), "must be a data frame")
})
