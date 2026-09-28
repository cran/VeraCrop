test_that("name-based ordinal detection uses the explicit level order when supplied", {
  set.seed(1)
  n <- 40
  # Deliberately construct row order so the first appearance is NOT the
  # correct low-to-high order (High appears before Low).
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    severity_level = sample(c("High", "Low", "Medium"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df <- df[order(df$severity_level == "High", decreasing = TRUE), ]  # High first

  result <- suppressWarnings(detect_variable_types(
    df, response_var = "Yield",
    ordinal_level_orders = list(severity_level = c("Low", "Medium", "High")),
    verbose = FALSE
  ))

  expect_identical(result$severity_level$type, "ordinal")
  expect_identical(result$severity_level$levels, c("Low", "Medium", "High"))
})

test_that("name-based ordinal detection downgrades to nominal without an explicit level order", {
  # Regression test for the original bug: without an explicit order, the
  # level order used to be taken from "order of first appearance" in the
  # raw data, which could silently reverse the true ordinal relationship.
  # The fixed behavior must NOT guess an order - it must downgrade to
  # nominal and warn.
  set.seed(2)
  n <- 40
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    severity_level = sample(c("High", "Low", "Medium"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- detect_variable_types(df, response_var = "Yield", verbose = FALSE),
    "NOMINAL"
  )
  expect_identical(result$severity_level$type, "nominal")
})

test_that("explicitly ordered factors are never downgraded, regardless of ordinal_level_orders", {
  set.seed(3)
  n <- 40
  df <- data.frame(
    Yield = rnorm(n, 5000, 500),
    severity_level = factor(sample(c("Low", "Medium", "High"), n, replace = TRUE),
                             levels = c("Low", "Medium", "High"), ordered = TRUE)
  )
  result <- detect_variable_types(df, response_var = "Yield", verbose = FALSE)
  expect_identical(result$severity_level$type, "ordinal")
  expect_identical(result$severity_level$levels, c("Low", "Medium", "High"))
})

test_that("nominal levels are sorted deterministically regardless of row order", {
  set.seed(4)
  n <- 30
  df1 <- data.frame(
    Yield = rnorm(n, 5000, 500),
    Region = sample(c("North", "South", "East"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  df2 <- df1[rev(seq_len(n)), ]  # same data, reversed row order

  r1 <- detect_variable_types(df1, response_var = "Yield", verbose = FALSE)
  r2 <- detect_variable_types(df2, response_var = "Yield", verbose = FALSE)

  expect_identical(r1$Region$levels, r2$Region$levels)
  expect_identical(r1$Region$reference, r2$Region$reference)
})

test_that("numeric nominal variables also get deterministic, sorted levels", {
  # Regression test: the earlier version sorted string/factor nominal
  # levels but left numeric-nominal levels dependent on order of first
  # appearance.
  set.seed(5)
  n <- 30
  df1 <- data.frame(
    Yield = rnorm(n, 5000, 500),
    ZoneCode = sample(c(3, 7, 12), n, replace = TRUE)
  )
  df2 <- df1[rev(seq_len(n)), ]

  r1 <- detect_variable_types(df1, response_var = "Yield", verbose = FALSE)
  r2 <- detect_variable_types(df2, response_var = "Yield", verbose = FALSE)

  expect_identical(r1$ZoneCode$levels, r2$ZoneCode$levels)
  expect_identical(r1$ZoneCode$reference, r2$ZoneCode$reference)
})
