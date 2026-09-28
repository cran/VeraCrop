# VeraCrop

<!-- badges: start -->
<!-- badges: end -->

VeraCrop provides an end-to-end workflow for **yield gap analysis using
Comparative Performance Analysis (CPA)**: automatic variable-type
detection, preprocessing (missing-value handling, encoding, scaling,
filtering), regression diagnostics, variable selection with
cross-validation, and yield gap decomposition with reporting.

## Installation

```r
# Install from source tarball
install.packages("VeraCrop_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Usage

```r
library(VeraCrop)

# 1. Preprocess: type detection, imputation, encoding, scaling, filtering
prep <- prep_yield_gap(my_data, response_var = "Yield")

# 2. Check regression assumptions
model <- lm(Yield ~ ., data = prep$data)
diag  <- check_assumptions(model, data = prep$data, plot = FALSE)

# 3. Run the full yield gap analysis
result <- yield_gap_analysis(prep$data, response = "Yield")
result$yield_gap

# 4. Visualize and export
plot_all_graphs(result)
save_results_excel(result, output_path = "yield_gap_results.xlsx")
```

See `vignette("getting-started", package = "VeraCrop")` for a complete
worked example.

## Core workflow

| Step | Function |
|---|---|
| Variable type detection | `detect_variable_types()` |
| Missing value handling | `handle_missing_values()` |
| Encoding | `encode_variables()` |
| Scaling | `scale_variables()` |
| Filtering | `remove_constant_vars()`, `remove_near_zero_variance()`, `remove_highly_correlated()` |
| Full preprocessing pipeline | `prep_yield_gap()` |
| Regression diagnostics | `check_assumptions()` |
| Variable selection | `select_vars_ftest()` |
| Cross-validation | `validate_model_with_cv()` |
| Yield gap analysis | `yield_gap_analysis()` |
| Reporting | `save_results_excel()`, `save_field_level_excel()`, `plot_all_graphs()` |

## License

MIT © Abolfazl Derakhshan
