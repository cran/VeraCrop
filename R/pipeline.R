# ============================================================
# File: R/preprocessing/pipeline.R
# VeraCrop: Main Preprocessing Pipeline
#
# This file only ORCHESTRATES the real building blocks defined in the other
# preprocessing modules. It intentionally does NOT redefine any filtering,
# detection, encoding, or scaling logic itself, to avoid function-name
# collisions with those modules (see filtering.R, detection.R, encoding.R,
# missing.R, scaling.R for the actual implementations).
#
# Pipeline order:
#   1. remove_non_predictors()          [filtering.R]
#   2. handle_missing_values()          [missing.R]     (imputation + optional
#                                                          missing indicators)
#   3. detect_variable_types()          [detection.R]
#   4. encode_variables()               [encoding.R]
#   5. remove_constant_vars()           [filtering.R]    (post-encoding, since
#                                                          dummy columns can be
#                                                          constant)
#   6. scale_variables()                [scaling.R]      (optional)
#   7. remove_near_zero_variance()      [filtering.R]    (optional)
#   8. remove_highly_correlated()       [filtering.R]    (optional)
# ============================================================

# ============================================================
# Internal Helper: Auto-tune Parameters Based on Data Characteristics
# ============================================================

#' Auto-tune preprocessing parameters based on data characteristics
#'
#' Produces tuned defaults for parameters that are actually consumed by the
#' encoding (encoding.R) and near-zero-variance filtering (filtering.R)
#' functions. Units are chosen to exactly match what those functions expect:
#'   - min_freq_for_encoding: proportion in (0, 1)  (matches encode_variables())
#'   - min_count_for_encoding: integer count >= 1   (matches encode_variables())
#'   - nzv_freq_cut: ratio > 1 (majority/minority frequency ratio, caret-style)
#'     (matches remove_near_zero_variance())
#'   - nzv_unique_cut: percentage in (0, 100)       (matches remove_near_zero_variance())
#'
#' @param n_rows Number of rows
#' @param n_vars Number of variables
#' @param p_ratio p/n ratio (number of variables / number of rows)
#'
#' @return List with tuned parameters
#' @noRd
auto_tune_params <- function(n_rows, n_vars, p_ratio = NULL) {

  if (is.null(p_ratio)) {
    p_ratio <- n_vars / n_rows
  }

  params <- list()

  if (n_rows >= 100) {
    params$max_categorical_levels  <- 10
    params$max_dummy_levels        <- 5
    params$min_freq_for_encoding   <- 0.05
    params$min_count_for_encoding  <- 10
    params$nzv_freq_cut            <- 19    # ~ 95/5, caret default
    params$nzv_unique_cut          <- 10    # percent
  } else if (n_rows >= 50) {
    params$max_categorical_levels  <- 5
    params$max_dummy_levels        <- 3
    params$min_freq_for_encoding   <- 0.08
    params$min_count_for_encoding  <- 5
    params$nzv_freq_cut            <- 9     # ~ 90/10
    params$nzv_unique_cut          <- 15
  } else if (n_rows >= 30) {
    params$max_categorical_levels  <- 3
    params$max_dummy_levels        <- 2
    params$min_freq_for_encoding   <- 0.10
    params$min_count_for_encoding  <- 3
    params$nzv_freq_cut            <- 4     # ~ 80/20
    params$nzv_unique_cut          <- 20
  } else {
    params$max_categorical_levels  <- 2
    params$max_dummy_levels        <- 2
    params$min_freq_for_encoding   <- 0.15
    params$min_count_for_encoding  <- 2
    params$nzv_freq_cut            <- 2.33  # ~ 70/30, lenient for tiny samples
    params$nzv_unique_cut          <- 25
  }

  # Stricter thresholds when the variable-to-sample ratio is high
  # (high-dimensional relative to sample size), since spurious near-constant
  # or high-cardinality variables are more likely to cause instability.
  if (p_ratio > 0.5) {
    params$max_categorical_levels <- max(2, params$max_categorical_levels - 1)
    # Floor of 2, not 1: encode_variables() requires max_dummy_levels >= 2
    # and errors otherwise ("max_dummy_levels must be a single integer >=
    # 2"). A floor of 1 here crashed the entire pipeline for small,
    # high-dimensional datasets (p/n > 0.5), which are common in real
    # agricultural field-trial data (few fields, many recorded management
    # practices).
    params$max_dummy_levels       <- max(2, params$max_dummy_levels - 1)
    params$min_count_for_encoding <- max(2, params$min_count_for_encoding - 1)
    params$nzv_freq_cut           <- max(2, params$nzv_freq_cut - 1)
  }

  return(params)
}

# ============================================================
# Main Preprocessing Pipeline
# ============================================================

#' Main Preprocessing Pipeline for Yield Gap Analysis
#'
#' Runs the full preprocessing pipeline end to end: removes non-predictor
#' (ID-like) columns, imputes missing values, detects variable types,
#' encodes variables for linear regression, removes constant variables
#' introduced by encoding, optionally scales continuous variables, and
#' optionally removes near-zero-variance and highly correlated variables.
#'
#' This function is a thin orchestrator: all actual logic lives in the
#' dedicated modules (\code{\link{remove_non_predictors}},
#' \code{\link{handle_missing_values}}, \code{\link{detect_variable_types}},
#' \code{\link{encode_variables}}, \code{\link{remove_constant_vars}},
#' \code{\link{scale_variables}}, \code{\link{remove_near_zero_variance}},
#' \code{\link{remove_highly_correlated}}).
#'
#' @param data A data frame.
#' @param response_var Name of the response variable. Default: \code{"Yield"}.
#' @param ... Additional named parameters overriding the defaults below.
#'   \describe{
#'     \item{trim_whitespace}{Logical, default TRUE. Trims leading/trailing
#'       whitespace from all character columns before any other processing
#'       (see \code{\link{trim_whitespace}}), so that e.g. "Sirvan" and
#'       "Sirvan " are correctly treated as the same category rather than
#'       two distinct ones.}
#'     \item{max_categorical_levels}{Integer or NULL (auto-tuned). Passed to
#'       \code{detect_variable_types()}.}
#'     \item{max_dummy_levels}{Integer or NULL (auto-tuned). Passed to
#'       \code{encode_variables()}.}
#'     \item{treat_numeric_as_ordinal}{Logical, default FALSE. Passed to
#'       \code{detect_variable_types()}.}
#'     \item{auto_detect_ordinal_from_name}{Logical, default TRUE. Passed to
#'       \code{detect_variable_types()}.}
#'     \item{auto_detect_ordinal_mode}{Character, "strict" or "contains",
#'       default "strict". Passed to \code{detect_variable_types()}.}
#'     \item{ordinal_level_orders}{Named list, default \code{list()}. Passed
#'       to \code{detect_variable_types()}; required to treat a
#'       character/factor variable as ordinal based on its name alone.}
#'     \item{reference_method}{Character, "mode", "first", or "alphabetical",
#'       default "mode". Passed to \code{detect_variable_types()} and
#'       honored during encoding.}
#'     \item{impute_missing}{Logical, default TRUE. If FALSE, missing-value
#'       imputation is skipped entirely (rows/columns with NA are left as
#'       is, which may cause errors downstream).}
#'     \item{impute_method}{Character, "mean", "median", or "mode", default
#'       "median". Passed as \code{numeric_method} to
#'       \code{handle_missing_values()}.}
#'     \item{missing_threshold}{Numeric in [0, 1], default 0.20. Passed to
#'       \code{handle_missing_values()}.}
#'     \item{ties_method}{Character, "first", "random", or "na", default
#'       "first". Passed to \code{handle_missing_values()}.}
#'     \item{add_missing_indicators}{Logical, default FALSE. Passed as
#'       \code{add_indicators} to \code{handle_missing_values()}.}
#'     \item{indicator_suffix}{Character, default "_missing". Passed to
#'       \code{handle_missing_values()}.}
#'     \item{exclude_vars}{Character vector or NULL. Passed to
#'       \code{remove_non_predictors()}.}
#'     \item{exclude_patterns}{Character vector or NULL. Passed to
#'       \code{remove_non_predictors()}.}
#'     \item{min_freq_for_encoding}{Numeric in (0, 1) or NULL (auto-tuned).
#'       Passed to \code{encode_variables()}.}
#'     \item{min_count_for_encoding}{Integer or NULL (auto-tuned). Passed to
#'       \code{encode_variables()}.}
#'     \item{scale_vars}{Logical, default TRUE. Whether to scale continuous
#'       variables via \code{scale_variables()}.}
#'     \item{scale_method}{Character, "standard" or "robust", default
#'       "standard". Passed to \code{scale_variables()}.}
#'     \item{remove_nzv}{Logical, default TRUE. Whether to remove near-zero-
#'       variance variables via \code{remove_near_zero_variance()}.}
#'     \item{nzv_freq_cut}{Numeric > 1 or NULL (auto-tuned). Passed to
#'       \code{remove_near_zero_variance()}.}
#'     \item{nzv_unique_cut}{Numeric in (0, 100) or NULL (auto-tuned).
#'       Passed to \code{remove_near_zero_variance()}.}
#'     \item{remove_correlated}{Logical, default FALSE. Whether to remove
#'       highly correlated variables via \code{remove_highly_correlated()}.}
#'     \item{corr_threshold}{Numeric in (0, 1), default 0.90. Passed to
#'       \code{remove_highly_correlated()}.}
#'     \item{corr_keep_method}{Character, "higher_correlation" or "first",
#'       default "higher_correlation". Passed to
#'       \code{remove_highly_correlated()}.}
#'     \item{verbose}{Logical, default TRUE.}
#'   }
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Final preprocessed data frame.}
#'   \item{var_types}{Variable type metadata from \code{detect_variable_types()}.}
#'   \item{encoding_info}{Encoding metadata from \code{encode_variables()}.}
#'   \item{scaling_params}{Scaling parameters from \code{scale_variables()},
#'     or NULL if \code{scale_vars = FALSE}.}
#'   \item{removed_vars}{Named list of character vectors: variables removed
#'     at each filtering stage (\code{non_predictors}, \code{constant},
#'     \code{nzv}, \code{correlated}).}
#'   \item{data_size}{One of "small", "medium", "large", based on input row count.}
#'   \item{response_var}{The response variable name used throughout.}
#'   \item{preprocessing_params}{The fully-resolved settings list actually used.}
#' }
#' @examples
#' df <- data.frame(
#'   Yield = rnorm(30, 5000, 500),
#'   Nitrogen = rnorm(30, 120, 20),
#'   Region = sample(c("North", "South", "East"), 30, replace = TRUE)
#' )
#' prep <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
#' dim(prep$data)
#'
#' # Using the bundled real dataset:
#' data(wheat1)
#' prep2 <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' dim(prep2$data)
#'
#' @export
prep_yield_gap <- function(data,
                                     response_var = "Yield",
                                     ...) {

  args <- list(...)

  defaults <- list(
    trim_whitespace                 = TRUE,
    max_categorical_levels          = NULL,
    max_dummy_levels                = NULL,
    treat_numeric_as_ordinal        = FALSE,
    auto_detect_ordinal_from_name   = TRUE,
    auto_detect_ordinal_mode        = "strict",
    ordinal_level_orders            = list(),
    reference_method                = "mode",
    impute_missing                  = TRUE,
    impute_method                   = "median",
    missing_threshold               = 0.20,
    ties_method                     = "first",
    add_missing_indicators          = FALSE,
    indicator_suffix                = "_missing",
    exclude_vars                    = NULL,
    exclude_patterns                = NULL,
    min_freq_for_encoding           = NULL,
    min_count_for_encoding          = NULL,
    scale_vars                      = TRUE,
    scale_method                    = "standard",
    remove_nzv                      = TRUE,
    nzv_freq_cut                    = NULL,
    nzv_unique_cut                  = NULL,
    remove_correlated               = FALSE,
    corr_threshold                  = 0.90,
    corr_keep_method                = "higher_correlation",
    verbose                         = TRUE
  )

  settings <- defaults
  unknown_args <- setdiff(names(args), names(settings))
  if (length(unknown_args) > 0) {
    warning(sprintf(
      "Ignoring unknown prep_yield_gap() argument(s): %s",
      paste(unknown_args, collapse = ", ")
    ), call. = FALSE)
  }
  for (name in names(args)) {
    if (name %in% names(settings)) {
      settings[[name]] <- args[[name]]
    }
  }

  # ---- Input validation ----
  if (!is.data.frame(data)) stop("'data' must be a data frame")

  if (!response_var %in% names(data)) {
    stop(sprintf("Response variable \"%s\" not found in data", response_var))
  }

  n_rows <- nrow(data)
  n_vars <- ncol(data)

  if (n_rows == 0) stop("data has zero rows")

  p_ratio <- n_vars / n_rows
  tuned <- auto_tune_params(n_rows, n_vars, p_ratio)

  if (is.null(settings$max_categorical_levels)) {
    settings$max_categorical_levels <- tuned$max_categorical_levels
  }
  if (is.null(settings$max_dummy_levels)) {
    settings$max_dummy_levels <- tuned$max_dummy_levels
  }
  if (is.null(settings$min_freq_for_encoding)) {
    settings$min_freq_for_encoding <- tuned$min_freq_for_encoding
  }
  if (is.null(settings$min_count_for_encoding)) {
    settings$min_count_for_encoding <- tuned$min_count_for_encoding
  }
  if (is.null(settings$nzv_freq_cut)) {
    settings$nzv_freq_cut <- tuned$nzv_freq_cut
  }
  if (is.null(settings$nzv_unique_cut)) {
    settings$nzv_unique_cut <- tuned$nzv_unique_cut
  }

  data_size <- if (n_rows >= 100) "large" else if (n_rows >= 50) "medium" else "small"

  if (settings$verbose) {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n")
    cat("PREPROCESSING FOR YIELD GAP ANALYSIS\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat(sprintf("Input: %d rows, %d columns\n", n_rows, n_vars))
    cat(sprintf("p/n ratio: %.2f\n", p_ratio))
    cat(sprintf("Response: %s\n", response_var))
    cat(sprintf("Data size: %s\n", data_size))
    cat(sprintf("max_categorical_levels: %d\n", settings$max_categorical_levels))
    cat(sprintf("max_dummy_levels: %d\n", settings$max_dummy_levels))
    cat(sprintf("impute_method: %s\n", settings$impute_method))
    cat(sprintf("scale_vars: %s (%s)\n", settings$scale_vars, settings$scale_method))

    if (n_rows < 30) {
      cat("\nSmall dataset detected. Results may be less reliable.\n")
      cat("Consider collecting more data if possible.\n")
    }
  }

  removed_vars <- list(
    non_predictors = character(),
    constant       = character(),
    nzv            = character(),
    correlated     = character()
  )

  # ------------------------------------------------------------
  # Step 0: Trim whitespace from character columns
  # trim_whitespace() [filtering.R]
  #
  # Runs before anything else, including non-predictor removal, so that
  # accidental leading/trailing whitespace (a very common artifact of
  # Excel/CSV data entry) never causes a single true category to be
  # silently split into two distinct levels during type detection or
  # encoding.
  # ------------------------------------------------------------
  if (settings$trim_whitespace) {
    data <- trim_whitespace(data, verbose = settings$verbose)
  }

  # ------------------------------------------------------------
  # Step 1: Remove non-predictor variables (e.g. ID columns)
  # remove_non_predictors() [filtering.R] returns list(data=, removed=)
  # ------------------------------------------------------------
  step1 <- remove_non_predictors(
    data             = data,
    exclude_vars     = settings$exclude_vars,
    exclude_patterns = settings$exclude_patterns,
    verbose          = settings$verbose
  )
  data <- step1$data
  removed_vars$non_predictors <- step1$removed

  if (!response_var %in% names(data)) {
    stop(sprintf(
      "Response variable \"%s\" was removed by remove_non_predictors() - check exclude_vars/exclude_patterns",
      response_var
    ))
  }

  # ------------------------------------------------------------
  # Step 2: Handle missing values (imputation, optional indicators)
  # handle_missing_values() [missing.R] returns list(data=, summary=, ...)
  # ------------------------------------------------------------
  if (settings$impute_missing) {
    missing_result <- handle_missing_values(
      data                = data,
      response_var        = response_var,
      numeric_method      = settings$impute_method,
      categorical_method  = "mode",
      logical_method      = "mode",
      missing_threshold   = settings$missing_threshold,
      ties_method         = settings$ties_method,
      add_indicators      = settings$add_missing_indicators,
      indicator_suffix    = settings$indicator_suffix,
      verbose             = settings$verbose
    )
    data <- missing_result$data
  } else {
    missing_result <- NULL
  }

  # ------------------------------------------------------------
  # Step 3: Detect variable types
  # detect_variable_types() [detection.R] returns a named list of metadata
  # ------------------------------------------------------------
  var_types <- detect_variable_types(
    data                           = data,
    response_var                   = response_var,
    max_categorical_levels         = settings$max_categorical_levels,
    treat_numeric_as_ordinal       = settings$treat_numeric_as_ordinal,
    auto_detect_ordinal_from_name  = settings$auto_detect_ordinal_from_name,
    auto_detect_ordinal_mode       = settings$auto_detect_ordinal_mode,
    ordinal_level_orders           = settings$ordinal_level_orders,
    reference_method               = settings$reference_method,
    verbose                        = settings$verbose
  )

  # ------------------------------------------------------------
  # Step 4: Encode variables for linear regression
  # encode_variables() [encoding.R] returns list(data=, encoding_info=, dropped_vars=)
  # ------------------------------------------------------------
  if (settings$verbose) cat("\n--- Encoding variables ---\n")
  encode_result <- encode_variables(
    data                    = data,
    var_types               = var_types,
    response_var            = response_var,
    max_dummy_levels        = settings$max_dummy_levels,
    min_freq_for_encoding   = settings$min_freq_for_encoding,
    min_count_for_encoding  = settings$min_count_for_encoding,
    verbose                 = settings$verbose
  )
  data_encoded  <- encode_result$data
  encoding_info <- encode_result$encoding_info

  # ------------------------------------------------------------
  # Step 5: Remove constant variables (post-encoding; dummy columns from
  # rare-category grouping or single-level factors can become constant)
  # remove_constant_vars() [filtering.R] returns list(data=, removed=)
  # ------------------------------------------------------------
  step5 <- remove_constant_vars(
    data         = data_encoded,
    response_var = response_var,
    verbose      = settings$verbose
  )
  data_encoded <- step5$data
  removed_vars$constant <- step5$removed

  # ------------------------------------------------------------
  # Step 6: Scale continuous variables (optional)
  # scale_variables() [scaling.R] returns list(data=, scaling_params=, ...)
  #
  # By default, binary/dummy (0/1) columns produced by encoding (types
  # "binary", "nominal", "nominal_grouped") are excluded from scaling, in
  # addition to the response variable. Standardizing a 0/1 indicator is
  # unusual practice and, concretely, it broke calculate_opt_values()'s
  # binary-optimum logic downstream in yield_gap_core.R, which assumed the
  # two real states of a binary column were literally 0 and 1 - true only
  # if that column was never scaled in the first place.
  # ------------------------------------------------------------
  if (settings$scale_vars) {
    if (settings$verbose) cat("\n--- Scaling variables ---\n")

    dummy_types <- c("binary", "nominal", "nominal_grouped")
    dummy_cols <- unlist(lapply(encoding_info, function(info) {
      if (!is.null(info$type) && info$type %in% dummy_types) info$encoded else NULL
    }), use.names = FALSE)

    scale_result <- scale_variables(
      data          = data_encoded,
      exclude_vars  = c(response_var, dummy_cols),
      method        = settings$scale_method,
      keep_original = FALSE,
      verbose       = settings$verbose
    )
    data_encoded   <- scale_result$data
    scaling_params <- scale_result$scaling_params
  } else {
    scaling_params <- NULL
  }

  # ------------------------------------------------------------
  # Step 7: Remove near-zero-variance variables (optional)
  # remove_near_zero_variance() [filtering.R] returns list(data=, removed=)
  # NOTE: freq_cut is a RATIO > 1 (caret-style) and unique_cut is a
  # PERCENTAGE in (0, 100) - see auto_tune_params() above for matching units.
  # ------------------------------------------------------------
  if (settings$remove_nzv) {
    step7 <- remove_near_zero_variance(
      data         = data_encoded,
      response_var = response_var,
      freq_cut     = settings$nzv_freq_cut,
      unique_cut   = settings$nzv_unique_cut,
      verbose      = settings$verbose
    )
    data_encoded <- step7$data
    removed_vars$nzv <- step7$removed
  }

  # ------------------------------------------------------------
  # Step 8: Remove highly correlated variables (optional)
  # remove_highly_correlated() [filtering.R] returns list(data=, removed=)
  # ------------------------------------------------------------
  if (settings$remove_correlated) {
    step8 <- remove_highly_correlated(
      data         = data_encoded,
      response_var = response_var,
      threshold    = settings$corr_threshold,
      keep_method  = settings$corr_keep_method,
      verbose      = settings$verbose
    )
    data_encoded <- step8$data
    removed_vars$correlated <- step8$removed
  }

  if (settings$verbose) {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n")
    cat("PREPROCESSING COMPLETE\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat(sprintf("Final dataset: %d rows, %d columns\n",
                nrow(data_encoded), ncol(data_encoded)))
  }

  return(list(
    data                 = data_encoded,
    var_types            = var_types,
    encoding_info        = encoding_info,
    scaling_params       = scaling_params,
    missing_value_report = missing_result$summary,
    removed_vars         = removed_vars,
    data_size            = data_size,
    response_var         = response_var,
    preprocessing_params = settings
  ))
}
