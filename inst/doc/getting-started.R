## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(VeraCrop)

## ----load-data----------------------------------------------------------------
data(wheat1)
str(wheat1)

## ----preprocess---------------------------------------------------------------
prep <- prep_yield_gap(
  wheat1,
  response_var = "Yield",
  verbose = FALSE
)

str(prep$data)

## ----validate-----------------------------------------------------------------
val <- validate_preprocessing(prep, verbose = FALSE)
val$valid

## ----diagnostics--------------------------------------------------------------
model <- lm(Yield ~ ., data = prep$data)
summary(model)$r.squared

diag <- check_assumptions(model, data = prep$data, plot = FALSE, verbose = FALSE)
diag$data_ready
diag$data_ready_reason

## ----analysis-----------------------------------------------------------------
result <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)

result$metrics
result$yield_mean
result$yield_opt
result$yield_gap

## ----cpa-table----------------------------------------------------------------
result$cpa_table

## ----field-level--------------------------------------------------------------
head(result$predictions)

## ----plots, eval = requireNamespace("ggplot2", quietly = TRUE)----------------
plot_contribution_shares(result)

## ----plots-more, eval = requireNamespace("ggplot2", quietly = TRUE)-----------
plot_observed_vs_predicted(result)

## ----export, eval = requireNamespace("writexl", quietly = TRUE)---------------
tmp <- tempfile(fileext = ".xlsx")
save_results_excel(result, output_path = tmp, verbose = FALSE)
file.exists(tmp)

## ----messy-data---------------------------------------------------------------
data(wheat2)

# "Sirvan" and "Sirvan " (trailing space) are automatically merged
length(unique(wheat2$Cultivar))
trimmed <- trim_whitespace(wheat2, verbose = FALSE)
length(unique(trimmed$Cultivar))

## ----ordinal, eval = FALSE----------------------------------------------------
# prep_small <- prep_yield_gap(
#   wheat2,
#   response_var = "Yield",
#   ordinal_level_orders = list(
#     Litracy_Level = c("Elementary School", "Middle School", "Diploma",
#                        "Bachelor's Degree", "Master's Degree")
#   ),
#   verbose = FALSE
# )

