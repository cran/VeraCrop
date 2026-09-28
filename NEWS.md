# VeraCrop 0.1.0

Initial release.

## CRAN resubmission fixes

* `DESCRIPTION`: reworded the `Description` field so it no longer starts
  with "Tools for", and added method references (de Bie 2004
  <doi:10.1016/j.scienta.2003.11.017>; Kamkar et al. 2025
  <doi:10.1016/j.agsy.2025.104392>).
* Replaced `\dontrun{}` with self-contained, runnable `\donttest{}`
  examples in `plot_all_graphs()`, `save_results_excel()`, and
  `save_field_level_excel()`.
* `plot_contribution_shares()` no longer writes an unsuppressable
  console message when there is nothing to plot; it now raises a
  `warning()` instead.
* `save_results_excel()`, `save_field_level_excel()`, and
  `plot_all_graphs()` no longer default to writing into the current
  working directory; their path/directory arguments now default to
  `tempdir()`.
* `check_assumptions()` now restores graphics `par()` settings via an
  immediate `on.exit()` rather than a manual call after plotting, so
  the user's graphics state is restored even if plotting fails.

## Update: bootstrap confidence intervals for the CPA decomposition

Added `bootstrap_cpa()`, which uses case resampling (nonparametric
bootstrap) to compute percentile confidence intervals for the overall
yield gap and for each variable's `Gap_Component` and `Share_Percent` in
the CPA table. The significant-variable set is held fixed across
resamples so the intervals describe the uncertainty of the decomposition
itself; an optional `check_selection_stability` diagnostic reports how
often each variable would be independently re-selected across resamples.

`bootstrap_cpa()` correctly includes the model intercept when computing
`Yield_Mean`/`Yield_Opt` for each resample, so these are reported on the
actual yield scale (an early internal version omitted it, which zeroed
out only these two summary rows while leaving `Yield_Gap` and the
per-variable `Gap_Component`/`Share_Percent` values unaffected, since the
intercept's own contribution to the gap is always zero).

`bootstrap_cpa()` now auto-tunes `top_percentile` and `min_top_samples`
from the sample size using the exact same thresholds as
`yield_gap_analysis()`, instead of a fixed `top_percentile = 0.99` and
`min_top_samples = 20`. On datasets smaller than 100 rows, the fixed
defaults did not match what `yield_gap_analysis()` actually used
internally, producing a different high-yield subset and therefore
different `Yield_Mean`/`Yield_Opt`/`Gap_Component` values than the
analysis being bootstrapped. Both parameters can still be set explicitly
to override the auto-tuned value.

## Update: bundled example datasets

Three real field-trial datasets are now bundled with the package and used
throughout the documentation and vignette in place of synthetic data:

* `wheat1` - 200 fields, 13 variables, clean (no missing values).
  The primary worked example for the full preprocessing/analysis
  workflow.
* `wheat2` - 40 fields, 29 variables, kept in its original,
  unedited form (including a non-predictor ID column and a real
  leading/trailing whitespace inconsistency in `Cultivar`). Illustrates
  `trim_whitespace()`, ordinal-variable handling via
  `ordinal_level_orders`, high-cardinality grouping, and the small-sample/
  high-dimensionality auto-tuning behavior of
  `prep_yield_gap()`.
* `sugarcane1` - 371 fields, 30 variables, a different crop and a
  larger, clean dataset. Illustrates a genuinely ordinal variable
  (`crop_type`: PC < R1 < ... < R9) and a perfectly collinear ("aliased")
  derived column (`total_urea_kg_ha`, the exact sum of four other
  columns) - both `select_vars_ftest()` and
  `select_vars_aic()` handle the latter gracefully.

See `?wheat1`, `?wheat2`, and `?sugarcane1` for full
column descriptions.

## Update: variable selection method choice

`yield_gap_analysis()` now supports `selection_method = "aic"` as an
alternative to the default `"ftest"`:

* `select_vars_aic()` selects variables by minimizing AIC via
  `stats::step()` (new function), with a new `aic_direction` argument
  ("both", "forward", "backward").
* F-test-based selection (fixed F-to-Enter/F-to-Remove or p-value
  threshold, similar to SigmaPlot) and AIC-based selection encode
  different criteria for what counts as a useful predictor and will
  normally select a different number of variables - neither is
  universally "correct". In testing against real field data, the default
  AIC selection tended to retain MORE variables than the default F-test
  settings, since AIC's implicit penalty is typically more lenient than a
  fixed F-to-Enter threshold around 4; this can go the other way if AIC is
  compared against a much stricter criterion (e.g. BIC, or a low
  significance threshold).

## Update: whitespace trimming and small-sample/high-dimensionality fixes

* New `trim_whitespace()` function, run automatically (default
  `trim_whitespace = TRUE`) at the very start of
  `prep_yield_gap()`. Protects against a very common real-world
  data-entry artifact where a single true category (e.g. "Sirvan" vs.
  "Sirvan ") is silently split into two distinct levels by accidental
  leading/trailing whitespace.
* Fixed a crash (`max_dummy_levels must be a single integer >= 2`) that
  occurred for small, high-dimensional datasets (many predictor columns
  relative to the number of rows) - a common profile for real
  agricultural field-trial data with few fields but many recorded
  management variables.

## Overview

VeraCrop provides an end-to-end workflow for yield gap analysis using
Comparative Performance Analysis (CPA): automatic variable-type detection,
preprocessing (missing-value handling, encoding, scaling, filtering),
regression diagnostics, variable selection with cross-validation, and
yield gap decomposition with reporting (Excel export, plots).

## Key functions

* `prep_yield_gap()` - end-to-end preprocessing pipeline
* `detect_variable_types()` - automatic variable type detection
* `encode_variables()` - encoding for linear regression (binary, ordinal,
  nominal, high-cardinality, continuous, date)
* `handle_missing_values()` - imputation with optional missing indicators
* `scale_variables()` / `back_transform_coefficients()` - standardization
  with correct back-transformation of regression coefficients
* `remove_constant_vars()` / `remove_near_zero_variance()` /
  `remove_highly_correlated()` - variable filtering
* `check_assumptions()` - the five classical linear regression assumption
  checks (linearity, independence, homoscedasticity, normality,
  multicollinearity) plus influential-point diagnostics
* `select_vars_ftest()` / `validate_model_with_cv()` -
  stepwise variable selection and k-fold cross-validation
* `yield_gap_analysis()` - full CPA yield gap computation, including
  optimal-value estimation, min-max effect sizes, relative importance,
  and Pareto analysis
* `save_results_excel()` / `save_field_level_excel()` - Excel reporting
* `plot_contribution_shares()`, `plot_observed_vs_predicted()`,
  `plot_yield_gap_distribution()`, `plot_minmax_effects()`,
  `plot_relative_importance()`, `plot_all_graphs()` - visualization

## Notes on correctness

The following were identified and corrected during development, primarily
through end-to-end testing against real agronomic data rather than static
code review alone. They are listed here because they affect the numeric
results a user would get from earlier development snapshots of this code,
not because they represent changes in the current public API.

* The response variable is now always protected from being scaled by
  `scale_variables()` / `prep_yield_gap()`, and by default
  binary/dummy (0/1) columns produced by encoding are excluded from
  scaling as well, since standardizing a 0/1 indicator has no meaningful
  interpretation and previously produced an incorrect "optimal value" for
  such variables in the yield gap decomposition.
* `calculate_opt_values()` now derives the optimal value of a binary/dummy
  variable from its own observed maximum/minimum, rather than assuming
  the two states are always literally 0 and 1 (true only when the
  variable has not been scaled).
* Categorical (nominal) variable encoding no longer fails for variables
  with more than two levels.
* Near-zero-variance filtering (`remove_near_zero_variance()`) now
  actually runs instead of raising an internal error on every call.
* `check_assumptions()` and `validate_model_with_cv()` accept an optional
  `seed` for reproducible sampling / fold assignment, and always restore
  the caller's global random state afterward rather than leaving it
  altered.
* Name-based automatic detection of ordinal variables now requires an
  explicit level order (`ordinal_level_orders`) for character/factor
  columns; without one, the variable is conservatively treated as nominal
  instead of guessing an order from row order in the raw data (which is
  not reproducible and can silently reverse the true ordinal
  relationship).
* All `verbose` arguments are honored consistently: computed results
  (e.g. `data_ready`, filtering decisions) no longer depend on whether
  progress messages are printed, and functions that previously always
  printed to the console regardless of caller settings
  (`pareto_analysis()`, `save_results_excel()`, `save_field_level_excel()`,
  `plot_all_graphs()`) now respect `verbose = FALSE`.
* `plot_all_graphs()` degrades gracefully (skipping unavailable panels)
  instead of raising an error when one of its inputs is unavailable.

## Testing

The package ships with a `testthat` suite covering preprocessing,
encoding, filtering, scaling, diagnostics, variable selection, yield gap
computation, and the end-to-end pipeline.
