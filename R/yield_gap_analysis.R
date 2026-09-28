# ============================================================
# File: R/yield_gap_analysis.R
# VeraCrop: Main Wrapper Function for Complete CPA Pipeline
# ============================================================

#' Complete Yield Gap Analysis Using CPA Method
#'
#' Runs the full Comparative Performance Analysis (CPA) pipeline:
#' variable selection, optimal value calculation, min-max effects,
#' yield gap decomposition, cross-validation, relative importance,
#' and Pareto analysis.
#'
#' @param data A preprocessed data frame (numeric columns only; see
#'   \code{\link{prep_yield_gap}}).
#' @param response Character. Name of the response (yield) variable. Default: "Yield".
#' @param p_threshold Numeric. Significance level for variable selection.
#'   Auto-tuned if NULL.
#' @param top_percentile Numeric. Top percentile threshold for defining optimal yield.
#'   Auto-tuned if NULL.
#' @param max_vars Integer. Maximum variables to select. Auto-tuned if NULL.
#' @param calculate_ri Logical. Calculate relative importance of variables. Default: TRUE.
#' @param ri_method Character. Method for relative importance: "lmg" or "std_beta".
#'   Default: "lmg".
#' @param selection_method Character, "ftest" or "aic". Variable selection
#'   method.
#'   \describe{
#'     \item{"ftest"}{Stepwise selection based on a fixed F-to-Enter /
#'       F-to-Remove (or p-value) threshold, similar to software such as
#'       SigmaPlot. See \code{\link{select_vars_ftest}} for the
#'       \code{p_threshold} / \code{use_bonferroni} controls. Tends to
#'       select FEWER variables, since each one must individually clear a
#'       fixed significance bar.}
#'     \item{"aic"}{Stepwise selection minimizing AIC via
#'       \code{\link{select_vars_aic}}. Tends to select MORE
#'       variables than "ftest", since it optimizes overall predictive fit
#'       rather than requiring each variable to be individually
#'       significant at a fixed threshold.}
#'   }
#'   Neither method is universally "correct" - they encode different
#'   criteria for what counts as a useful predictor, and it is normal for
#'   them to select a different number and/or set of variables on the same
#'   data. Default: "ftest".
#' @param aic_direction Character, "both", "forward", or "backward". Only
#'   used when \code{selection_method = "aic"}; passed to
#'   \code{\link[stats]{step}}. Default: "both".
#' @param perform_cv Logical. Perform cross-validation. Auto-disabled for small data.
#'   Default: TRUE.
#' @param cv_seed Integer or NULL. Random seed used only for cross-validation
#'   fold assignment, for reproducibility. The caller's global random state
#'   is saved and restored afterward, so this has no persistent side effect
#'   on the user's session. Set to NULL for non-reproducible fold assignment.
#'   Default: 42.
#' @param use_bonferroni Logical. Apply Bonferroni correction. Default: FALSE.
#' @param verbose Logical. Print step-by-step progress. Default: TRUE.
#'
#' @return A list with the following components:
#'   \item{model}{The final fitted \code{lm} model.}
#'   \item{significant_vars}{Character vector of selected variables.}
#'   \item{var_table}{Data frame of variable statistics and optimal values.}
#'   \item{minmax_results}{Data frame of min-max effects.}
#'   \item{cpa_table}{Full CPA table with gap contributions and shares.}
#'   \item{yield_mean}{Predicted mean yield.}
#'   \item{yield_opt}{Predicted optimal yield.}
#'   \item{yield_gap}{Total yield gap.}
#'   \item{predictions}{Data frame: Observed, Predicted, Yield_Gap_Obs per row.}
#'   \item{metrics}{List: R2, RMSE, MAE for the fitted model.}
#'   \item{cross_validation}{Cross-validation results (if performed).}
#'   \item{relative_importance}{Relative importance results.}
#'   \item{pareto_analysis}{Pareto analysis results.}
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' results$yield_gap
#' results$cpa_table
#'
#' @importFrom stats coef cor predict quantile
#' @export
yield_gap_analysis <- function(data,
                                response          = "Yield",
                                p_threshold       = NULL,
                                top_percentile    = NULL,
                                max_vars          = NULL,
                                calculate_ri      = TRUE,
                                ri_method         = c("lmg", "std_beta"),
                                selection_method  = c("ftest", "aic"),
                                aic_direction     = c("both", "forward", "backward"),
                                perform_cv        = TRUE,
                                cv_seed           = 42,
                                use_bonferroni    = FALSE,
                                verbose           = TRUE) {

  selection_method <- match.arg(selection_method)
  aic_direction    <- match.arg(aic_direction)
  ri_method        <- match.arg(ri_method)

  # ---- Auto-tune parameters based on data size ----
  n_rows <- nrow(data)
  n_vars <- ncol(data)

  if (is.null(p_threshold)) {
    if (n_rows >= 100) {
      p_threshold <- 0.05
    } else if (n_rows >= 50) {
      p_threshold <- 0.05
    } else if (n_rows >= 30) {
      p_threshold <- 0.10
    } else {
      p_threshold <- 0.15
    }
  }

  if (is.null(top_percentile)) {
    if (n_rows >= 100) {
      top_percentile <- 0.99
    } else if (n_rows >= 50) {
      top_percentile <- 0.95
    } else if (n_rows >= 30) {
      top_percentile <- 0.90
    } else {
      top_percentile <- 0.85
    }
  }

  if (is.null(max_vars)) {
    if (n_vars > 20) {
      max_vars <- min(15, floor(n_vars * 0.3))
    } else if (n_vars > 10) {
      max_vars <- min(10, floor(n_vars * 0.4))
    } else {
      max_vars <- n_vars
    }
  }

  # Disable CV for small datasets
  if (perform_cv && n_rows < 20) {
    perform_cv <- FALSE
    if (verbose) cat("  Cross-validation disabled (fewer than 20 rows)\n")
  }

  # Set min_top_samples based on data size
  if (n_rows >= 100) {
    min_top_samples <- 20
  } else if (n_rows >= 50) {
    min_top_samples <- 10
  } else if (n_rows >= 30) {
    min_top_samples <- 5
  } else {
    min_top_samples <- 3
  }

  if (verbose) {
    cat("\n", paste(rep("=", 70), collapse = ""), "\n")
    cat("YIELD GAP ANALYSIS\n")
    cat(paste(rep("=", 70), collapse = ""), "\n")
    cat(sprintf("Data: %d rows, %d columns\n", n_rows, n_vars))
    cat(sprintf("Response: %s\n", response))
    cat(sprintf("p_threshold: %.3f\n", p_threshold))
    cat(sprintf("top_percentile: %.2f\n", top_percentile))
    cat(sprintf("max_vars: %d\n", max_vars))
    cat(sprintf("perform_cv: %s\n", ifelse(perform_cv, "Yes", "No")))
    cat("\n")
  }

  # ---- STEP 1: Variable selection ----
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("STEP 1: SELECTING SIGNIFICANT VARIABLES\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }

  if (selection_method == "ftest") {
    sel <- select_vars_ftest(
      data = data,
      response = response,
      p_threshold = p_threshold,
      use_bonferroni = use_bonferroni,
      max_vars = max_vars,
      verbose = verbose
    )
  } else if (selection_method == "aic") {
    sel <- select_vars_aic(
      data = data,
      response = response,
      direction = aic_direction,
      verbose = verbose
    )
  }

  if (length(sel$significant_vars) == 0) {
    warning("No significant variables found. Returning NULL.")
    return(NULL)
  }

  model <- sel$final_model
  sig_vars <- sel$significant_vars
  coefs <- coef(model)
  intercept_val <- coefs["(Intercept)"]
  coefs_vars <- coefs[names(coefs) != "(Intercept)"]

  if (verbose) {
    cat("\n  Selected", length(sig_vars), "variables.")
    cat(" Intercept =", round(intercept_val, 2), "\n")
  }

  # ---- STEP 2: Top-percentile yields ----
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("STEP 2: IDENTIFYING TOP YIELDS\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }

  yield_thr <- quantile(data[[response]], top_percentile, na.rm = TRUE)
  data_top <- data[data[[response]] >= yield_thr, ]

  if (nrow(data_top) < min_top_samples && nrow(data) > min_top_samples) {
    new_top_percentile <- min(max(1 - min_top_samples / nrow(data), 0.80), 0.95)
    if (verbose) {
      cat("  Adjusting top percentile from", (1 - top_percentile) * 100,
          "% to", (1 - new_top_percentile) * 100, "%\n")
    }
    top_percentile <- new_top_percentile
    yield_thr <- quantile(data[[response]], top_percentile, na.rm = TRUE)
    data_top <- data[data[[response]] >= yield_thr, ]
  }

  if (verbose) {
    cat("  Threshold (top", (1 - top_percentile) * 100, "%):",
        round(yield_thr, 2), "\n")
    cat("  Observations in top group:", nrow(data_top), "\n")
  }

  # ---- STEP 3: Optimal values ----
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("STEP 3: CALCULATING OPTIMAL VALUES\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }

  opt_result <- calculate_opt_values(
    data = data,
    vars = sig_vars,
    coefs = coefs_vars,
    data_top = data_top,
    response_var = response,
    top_percentile = top_percentile,
    min_top_samples = min_top_samples
  )
  var_table <- opt_result$var_table

  if (verbose && nrow(var_table) > 0) {
    print(var_table[, c("Variable", "Mean", "Opt", "VarType", "Opt_Method")])
  }

  # ---- STEP 4: Min-Max effects ----
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("STEP 4: CALCULATING MIN-MAX EFFECTS\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }

  minmax_results <- calculate_minmax_effects(model, data, sig_vars, use_quantiles = TRUE)
  var_table$MinMaxEffect <- minmax_results$Effect[match(var_table$Variable, minmax_results$Variable)]
  var_table$MinMaxPct <- minmax_results$EffectPct[match(var_table$Variable, minmax_results$Variable)]

  # ---- STEP 5: Build CPA table and gap components ----
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("STEP 5: CALCULATING YIELD GAP COMPONENTS\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }

  cpa_table <- data.frame(
    Variable = c("Intercept", sig_vars),
    Beta = c(intercept_val, coefs_vars),
    Mean = c(1, var_table$Mean),
    Opt = c(1, var_table$Opt)
  )
  cpa_table <- cpa_table[!is.na(cpa_table$Mean) & !is.na(cpa_table$Opt), ]

  yg_results <- yield_gap_components(cpa_table)

  yg_results$cpa_table$MinMaxEffect <- NA
  yg_results$cpa_table$MinMaxPct <- NA
  idx <- match(yg_results$cpa_table$Variable, var_table$Variable)
  yg_results$cpa_table$MinMaxEffect[!is.na(idx)] <- var_table$MinMaxEffect[idx[!is.na(idx)]]
  yg_results$cpa_table$MinMaxPct[!is.na(idx)] <- var_table$MinMaxPct[idx[!is.na(idx)]]

  if (verbose) {
    cat("\n  Yield Statistics:\n")
    cat("    Mean Yield :", round(yg_results$yield_mean, 2), "\n")
    cat("    Optimal    :", round(yg_results$yield_opt, 2), "\n")
    cat("    YIELD GAP  :", round(yg_results$yield_gap, 2), "\n")
    if (yg_results$yield_opt > 0) {
      cat("    Gap %      :", round(yg_results$yield_gap / yg_results$yield_opt * 100, 1), "%\n")
    }
  }

  # ---- STEP 6: Model validation ----
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("STEP 6: MODEL VALIDATION\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }

  data$Predicted <- predict(model, newdata = data)
  rsq <- cor(data[[response]], data$Predicted, use = "complete.obs")^2
  rmse_val <- sqrt(mean((data[[response]] - data$Predicted)^2, na.rm = TRUE))
  mae_val <- mean(abs(data[[response]] - data$Predicted), na.rm = TRUE)

  if (verbose) {
    cat("  R2   =", round(rsq, 4), "\n")
    cat("  RMSE =", round(rmse_val, 2), "\n")
    cat("  MAE  =", round(mae_val, 2), "\n")
  }

  # ---- STEP 7: Cross-validation ----
  cv_results <- NULL
  if (perform_cv && length(sig_vars) >= 2 && nrow(data) >= 10) {
    if (verbose) {
      cat("\n", paste(rep("=", 60), collapse = ""), "\n")
      cat("STEP 7: CROSS-VALIDATION\n")
      cat(paste(rep("=", 60), collapse = ""), "\n")
    }
    # Set k_folds based on data size
    if (nrow(data) >= 50) {
      k_folds <- 5
    } else if (nrow(data) >= 20) {
      k_folds <- 3
    } else {
      k_folds <- 2
    }

    cv_results <- validate_model_with_cv(
      model = model,
      data = data,
      sig_vars = sig_vars,
      response = response,
      k_folds = k_folds,
      seed = cv_seed
    )

    if (verbose && !is.null(cv_results)) {
      cv_r2 <- cv_results$summary$Mean[cv_results$summary$Metric == "R2"]
      cv_r2sd <- cv_results$summary$SD[cv_results$summary$Metric == "R2"]
      cv_rmse <- cv_results$summary$Mean[cv_results$summary$Metric == "RMSE"]
      cat("\n  Cross-Validation (", k_folds, "-fold):\n", sep = "")
      cat("    R2   =", round(cv_r2, 4), "+/-", round(cv_r2sd, 4), "\n")
      cat("    RMSE =", round(cv_rmse, 2), "\n")
    }
  } else if (verbose && perform_cv) {
    cat("\n  Cross-validation skipped: insufficient data or variables\n")
  }

  # ---- STEP 8: Per-observation yield gap ----
  data$Yield_Gap_obs <- pmax(yg_results$yield_opt - data$Predicted, 0)

  # ---- STEP 9: Relative importance ----
  rel_imp <- NULL
  if (calculate_ri && length(sig_vars) > 1) {
    if (verbose) {
      cat("\n", paste(rep("=", 60), collapse = ""), "\n")
      cat("STEP 9: CALCULATING RELATIVE IMPORTANCE\n")
      cat(paste(rep("=", 60), collapse = ""), "\n")
    }
    rel_imp <- calculate_relative_importance(
      data = data,
      vars = sig_vars,
      response = response,
      method = ri_method,
      verbose = verbose
    )
  }

  # ---- STEP 10: Pareto analysis ----
  pareto <- pareto_analysis(yg_results, verbose = verbose)

  # ---- Return ----
  return(list(
    model = model,
    significant_vars = sig_vars,
    var_table = var_table,
    minmax_results = minmax_results,
    cpa_table = yg_results$cpa_table,
    yield_mean = yg_results$yield_mean,
    yield_opt = yg_results$yield_opt,
    yield_gap = yg_results$yield_gap,
    predictions = data.frame(
      Observed = data[[response]],
      Predicted = data$Predicted,
      Yield_Gap_Obs = data$Yield_Gap_obs
    ),
    metrics = list(R2 = rsq, RMSE = rmse_val, MAE = mae_val),
    cross_validation = cv_results,
    relative_importance = rel_imp,
    pareto_analysis = pareto
  ))
}
