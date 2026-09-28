# ============================================================
# File: R/diagnostics.R
# VeraCrop: Model Diagnostics and Assumption Checks
# ============================================================

# ============================================================
# Internal helper: Extract influential points with safe ID mapping
# ============================================================

#' Extract influential points with safe FarmID mapping
#'
#' Internal function that extracts Cook's distance, leverage, and DFFITS
#' with proper mapping to FarmID using row names rather than positional indices.
#'
#' @param model A fitted lm model object
#' @param data Original data frame (may contain NAs)
#' @param na_action The na.action attribute from the model
#' @return A list with influence statistics and mapped IDs
#' @keywords internal
.extract_influential_points <- function(model, data, na_action) {
  
  # Extract basic information
  n <- length(stats::residuals(model))
  p <- length(stats::coef(model)) - 1
  
  # Get model row names (these are the original row identifiers)
  model_row_names <- names(stats::residuals(model))
  if (is.null(model_row_names)) {
    model_row_names <- as.character(seq_len(n))
  }
  
  # Data row names for matching
  data_row_names <- rownames(data)
  if (is.null(data_row_names)) {
    data_row_names <- as.character(seq_len(nrow(data)))
  }
  
  # Check if we can map IDs safely
  can_map_farmid <- FALSE
  mapping_method <- "none"
  id_warning <- NULL
  is_critical <- FALSE
  
  # Detect if NAs were present
  # This works for both na.omit and na.exclude
  has_na_action <- !is.null(na_action) || nrow(data) > n
  
  if (is.null(data) || !("FarmID" %in% names(data))) {
    mapping_method <- "no_farmid"
  } else if (nrow(data) == n && !has_na_action) {
    # Perfect alignment: model fit directly on data
    can_map_farmid <- TRUE
    mapping_method <- "direct_aligned"
  } else if (nrow(data) >= n && has_na_action) {
    # Model had NA action - use row names for matching
    # Check if model row names are valid indices into data
    if (all(model_row_names %in% data_row_names)) {
      can_map_farmid <- TRUE
      mapping_method <- "rowname_match"
    } else {
      # Row names don't match - cannot map safely
      can_map_farmid <- FALSE
      mapping_method <- "rowname_mismatch"
      id_warning <- "Model row names do not match data row names; FarmID mapping disabled."
      warning(id_warning, call. = FALSE)
      is_critical <- TRUE
    }
  } else {
    # Size mismatch without NA handling - cannot map
    mapping_method <- "size_mismatch"
    id_warning <- sprintf(
      "CRITICAL: data has %d rows but model has %d observations.",
      nrow(data), n
    )
    warning(id_warning, call. = FALSE)
    is_critical <- TRUE
  }
  
  # Calculate influence statistics
  result <- list(
    cooks_distance = list(
      values = stats::cooks.distance(model),
      threshold = 4/n,
      influential_indices = NULL,
      influential_ids = NULL,
      mapping_status = mapping_method
    ),
    leverage = list(
      values = stats::hatvalues(model),
      threshold = 2 * p / n,
      high_leverage_indices = NULL,
      high_leverage_ids = NULL,
      mapping_status = mapping_method
    ),
    dffits = list(
      values = stats::dffits(model),
      threshold = 2 * sqrt(p/n),
      influential_indices = NULL,
      influential_ids = NULL,
      mapping_status = mapping_method
    ),
    summary = NULL,
    model_observations = n,
    data_rows = if (!is.null(data)) nrow(data) else NA,
    alignment_status = if (can_map_farmid) "ok" else "mapping_disabled",
    warning = id_warning,
    is_critical = is_critical,
    can_map_farmid = can_map_farmid,
    mapping_method = mapping_method
  )
  
  # Function to map indices to FarmID safely
  map_to_farmid <- function(indices, values_name) {
    if (length(indices) == 0) {
      return(character(0))
    }
    
    if (!can_map_farmid) {
      return(NULL)
    }
    
    # Get the row names for these indices
    row_names_for_indices <- model_row_names[indices]
    
    # Map to FarmID based on mapping method
    if (mapping_method == "direct_aligned") {
      # Direct mapping - data is in same order as model
      return(data$FarmID[indices])
    } else if (mapping_method == "rowname_match") {
      # Map by row name
      matched_ids <- data$FarmID[match(row_names_for_indices, data_row_names)]
      # Check for NA matches (shouldn't happen if all row names match)
      if (any(is.na(matched_ids))) {
        warning(sprintf("Some %s row names could not be mapped to FarmID", values_name), 
                call. = FALSE)
      }
      return(matched_ids)
    } else {
      return(NULL)
    }
  }
  
  # Extract influential points for each statistic
  cooks_indices <- which(result$cooks_distance$values > result$cooks_distance$threshold)
  result$cooks_distance$influential_indices <- cooks_indices
  result$cooks_distance$influential_ids <- map_to_farmid(cooks_indices, "Cook's distance")
  
  leverage_indices <- which(result$leverage$values > result$leverage$threshold)
  result$leverage$high_leverage_indices <- leverage_indices
  result$leverage$high_leverage_ids <- map_to_farmid(leverage_indices, "leverage")
  
  dffits_indices <- which(abs(result$dffits$values) > result$dffits$threshold)
  result$dffits$influential_indices <- dffits_indices
  result$dffits$influential_ids <- map_to_farmid(dffits_indices, "DFFITS")
  
  # Create summary
  result$summary <- sprintf(
    "Cook's D: %d influential, High leverage: %d, DFFITS: %d",
    length(cooks_indices),
    length(leverage_indices),
    length(dffits_indices)
  )
  
  # Store model row names for reference
  result$model_row_names <- model_row_names
  result$data_row_names <- data_row_names
  
  return(result)
}

# ============================================================
# Main exported function: Check Linear Regression Assumptions
# ============================================================

#' Check Linear Regression Assumptions
#'
#' Automatically checks the five main assumptions of linear regression:
#' 1. Linearity (using Ramsey RESET test or plot)
#' 2. Independence of residuals (Durbin-Watson with p-value)
#' 3. Homoscedasticity (Breusch-Pagan)
#' 4. Normality of residuals (Shapiro-Wilk with skewness/kurtosis)
#' 5. Multicollinearity (VIF/GVIF with proper scaling)
#'
#' @param model A fitted \code{lm} model object.
#' @param data Optional data frame. May be either:
#'   (a) the exact complete-case data used to fit the model, or
#'   (b) the original unfiltered data - in which case row names are used
#'       to safely map influential points back to FarmID.
#'   For best results, preprocess data using \code{handle_missing_values()}
#'   before fitting.
#' @param plot Logical. Show diagnostic plots. Default: TRUE.
#' @param verbose Logical. Print results. Default: TRUE.
#' @param max_warnings Integer. Maximum number of warnings allowed for data readiness.
#'   Default: 2.
#' @param seed Integer. Random seed for reproducible sampling in large datasets.
#'   Default: 42.
#' @param alpha Numeric. Significance level for hypothesis tests. Default: 0.05.
#' @param vif_threshold Numeric. VIF threshold for multicollinearity. Default: 10.
#' @param vif_warning_threshold Numeric. VIF warning threshold. Default: 5.
#'
#' @return A list with all test results, summary status, data readiness flag,
#'   and (if plot=TRUE) a recorded diagnostic plot object.
#' @export
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' model <- lm(Yield ~ ., data = prep$data)
#' result <- check_assumptions(model, data = prep$data, plot = FALSE, verbose = FALSE)
#' result$data_ready
check_assumptions <- function(model, data = NULL, plot = TRUE, verbose = TRUE, 
                              max_warnings = 2, seed = 42, alpha = 0.05,
                              vif_threshold = 10, vif_warning_threshold = 5) {
  
  if (!inherits(model, "lm")) {
    stop("'model' must be an object of class 'lm'")
  }
  
  results <- list()
  issues <- character()
  critical_issues <- character()
  warnings_issues <- character()
  
  # Rename to avoid masking base function
  model_residuals <- stats::residuals(model)
  n <- length(model_residuals)
  p <- length(stats::coef(model)) - 1
  
  # Check if model had NA action
  na_action <- model$na.action
  
  # Initialize summary variables
  passed <- NA_integer_
  total <- NA_integer_
  data_ready <- FALSE
  data_ready_reason <- "Not calculated"
  
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("LINEAR REGRESSION ASSUMPTION CHECKS\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat(sprintf("Observations: %d\n", n))
    cat(sprintf("Variables: %d\n", p))
    cat(sprintf("Sample size: %s\n", ifelse(n >= 100, "Large", ifelse(n >= 50, "Medium", "Small"))))
    cat(sprintf("Significance level (alpha): %.3f\n", alpha))
    cat("\n")
  }
  
  # ============================================================
  # 1. Linearity (Ramsey RESET test)
  # ============================================================
  if (verbose) {
    cat("1. LINEARITY\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
  }
  
  linearity_status <- TRUE
  reset_p <- NA
  
  if (requireNamespace("lmtest", quietly = TRUE) && n > p + 2) {
    reset_test <- tryCatch({
      lmtest::resettest(model, power = 2:3, type = "fitted")
    }, error = function(e) NULL)
    
    if (!is.null(reset_test)) {
      reset_p <- reset_test$p.value
      linearity_status <- reset_p > alpha
      results$linearity <- list(
        test = "Ramsey RESET",
        p_value = reset_p,
        status = linearity_status,
        alpha = alpha
      )
      
      if (verbose) {
        cat(sprintf("  Ramsey RESET P-value: %.4f\n", reset_p))
        if (linearity_status) {
          cat("  [OK] No evidence of non-linearity\n")
        } else {
          cat("  [CRITICAL] Non-linearity detected\n")
          issues <- c(issues, "Linearity: non-linearity detected (RESET p < alpha)")
          critical_issues <- c(critical_issues, "Linearity")
        }
      }
    } else {
      if (verbose) {
        cat("  [INFO] Ramsey RESET test failed.\n")
        cat("  [INFO] Please check Residuals vs Fitted plot.\n")
      }
      results$linearity <- list(
        test = "not_run",
        status = NA,
        note = "Check Residuals vs Fitted plot"
      )
      warnings_issues <- c(warnings_issues, "Linearity: manual check required")
    }
  } else {
    if (verbose) {
      cat("  [INFO] Package 'lmtest' not installed.\n")
      cat("  [INFO] Please check Residuals vs Fitted plot.\n")
    }
    results$linearity <- list(
      test = "not_run",
      status = NA,
      note = "Install 'lmtest' or check plot"
    )
    warnings_issues <- c(warnings_issues, "Linearity: manual check required")
  }
  
  # ============================================================
  # 2. Independence (Durbin-Watson) 
  # ============================================================
  if (verbose) {
    cat("\n2. INDEPENDENCE (Durbin-Watson)\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
  }
  
  if (requireNamespace("lmtest", quietly = TRUE)) {
    dw_result <- tryCatch({
      lmtest::dwtest(model)
    }, error = function(e) NULL)
    
    if (!is.null(dw_result)) {
      dw_stat <- dw_result$statistic
      dw_p_value <- dw_result$p.value
      
      dw_status <- dw_p_value > alpha
      
      results$independence <- list(
        test = "Durbin-Watson",
        statistic = as.numeric(dw_stat),
        p_value = dw_p_value,
        alpha = alpha,
        n = n,
        status = dw_status,
        note = sprintf("Test power increases with sample size (n = %d)", n)
      )
      
      if (verbose) {
        cat(sprintf("  Durbin-Watson statistic: %.3f\n", dw_stat))
        cat(sprintf("  P-value: %.4f (alpha: %.3f)\n", dw_p_value, alpha))
        cat(sprintf("  Note: Test power increases with sample size (n = %d)\n", n))
        if (dw_status) {
          cat("  [OK] No autocorrelation detected\n")
        } else {
          cat("  [WARNING] Autocorrelation detected\n")
          issues <- c(issues, sprintf("Independence: DW = %.3f, p = %.4f", dw_stat, dw_p_value))
          warnings_issues <- c(warnings_issues, "Autocorrelation")
        }
      }
    } else {
      if (verbose) {
        cat("  [INFO] Durbin-Watson test failed.\n")
      }
      results$independence <- list(
        test = "not_run",
        status = NA,
        note = "Test failed to execute"
      )
    }
  } else {
    if (verbose) {
      cat("  [INFO] Package 'lmtest' not installed. Skipping Durbin-Watson test.\n")
      cat("  [INFO] Install 'lmtest' for autocorrelation check.\n")
    }
    results$independence <- list(
      test = "not_run",
      status = NA,
      note = "Install 'lmtest' package"
    )
  }
  
  # ============================================================
  # 3. Homoscedasticity (Breusch-Pagan)
  # ============================================================
  if (verbose) {
    cat("\n3. HOMOSCEDASTICITY (Breusch-Pagan)\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
  }
  
  if (requireNamespace("lmtest", quietly = TRUE)) {
    bp_test <- tryCatch({
      lmtest::bptest(model)
    }, error = function(e) NULL)
    
    if (!is.null(bp_test)) {
      bp_p <- bp_test$p.value
      bp_status <- bp_p > alpha
      results$homoscedasticity <- list(
        test = "Breusch-Pagan",
        p_value = bp_p,
        status = bp_status,
        alpha = alpha
      )
      
      if (verbose) {
        cat(sprintf("  Breusch-Pagan P-value: %.4f\n", bp_p))
        if (bp_status) {
          cat("  [OK] Homoscedasticity (constant variance)\n")
        } else {
          cat("  [WARNING] Heteroscedasticity detected\n")
          issues <- c(issues, "Homoscedasticity: heteroscedasticity detected")
          warnings_issues <- c(warnings_issues, "Heteroscedasticity")
        }
      }
    }
  } else {
    if (verbose) {
      cat("  [INFO] Package 'lmtest' not installed. Skipping Breusch-Pagan test.\n")
    }
    results$homoscedasticity <- list(
      test = "not_run",
      status = NA,
      note = "Install 'lmtest' package"
    )
  }
  
  # ============================================================
  # 4. Normality (Shapiro-Wilk with skewness/kurtosis)
  # ============================================================
  if (verbose) {
    cat("\n4. NORMALITY (Shapiro-Wilk)\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
  }
  
  # Calculate Skewness and Kurtosis
  skewness_val <- NA_real_
  kurtosis_val <- NA_real_
  
  if (requireNamespace("e1071", quietly = TRUE)) {
    skewness_val <- tryCatch({
      e1071::skewness(model_residuals, na.rm = TRUE)
    }, error = function(e) NA_real_)
    
    kurtosis_val <- tryCatch({
      e1071::kurtosis(model_residuals, na.rm = TRUE)
    }, error = function(e) NA_real_)
  } else {
    # Fallback: manual calculation
    skewness_val <- tryCatch({
      m2 <- sum((model_residuals - mean(model_residuals))^2) / n
      m3 <- sum((model_residuals - mean(model_residuals))^3) / n
      m3 / (m2^(3/2))
    }, error = function(e) NA_real_)
    
    kurtosis_val <- tryCatch({
      m2 <- sum((model_residuals - mean(model_residuals))^2) / n
      m4 <- sum((model_residuals - mean(model_residuals))^4) / n
      m4 / (m2^2) - 3
    }, error = function(e) NA_real_)
  }
  
  # Set seed for reproducible sampling. The caller's global RNG state is
  # saved before, and restored after, so this function never has a
  # persistent side effect on the user's random number stream (matching
  # the same pattern used in validate_model_with_cv()).
  if (n > 5000) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(seed)
    sample_res <- sample(model_residuals, 5000)
    shapiro_test <- tryCatch({
      stats::shapiro.test(sample_res)
    }, error = function(e) NULL)
    if (verbose) {
      cat(sprintf("  (Sampled 5000 residuals with seed = %d)\n", seed))
    }
  } else {
    shapiro_test <- tryCatch({
      stats::shapiro.test(model_residuals)
    }, error = function(e) NULL)
  }
  
  if (!is.null(shapiro_test)) {
    shapiro_p <- shapiro_test$p.value
    
    shapiro_ok <- shapiro_p > alpha
    
    # Define thresholds based on sample size
    if (n > 100) {
      skewness_threshold <- 1.5
      kurtosis_threshold <- 2
    } else if (n > 50) {
      skewness_threshold <- 1
      kurtosis_threshold <- 1.5
    } else {
      skewness_threshold <- 0.8
      kurtosis_threshold <- 1
    }
    
    skewness_ok <- !is.na(skewness_val) && abs(skewness_val) < skewness_threshold
    kurtosis_ok <- !is.na(kurtosis_val) && abs(kurtosis_val) < kurtosis_threshold
    normality_status <- shapiro_ok || (skewness_ok && kurtosis_ok)
    normality_reason <- if (shapiro_ok) "Shapiro-Wilk OK" else "Skewness & Kurtosis OK"
    
    results$normality <- list(
      test = "Shapiro-Wilk",
      p_value = shapiro_p,
      skewness = skewness_val,
      kurtosis = kurtosis_val,
      status = normality_status,
      reason = normality_reason,
      alpha = alpha,
      skewness_threshold = skewness_threshold,
      kurtosis_threshold = kurtosis_threshold
    )
    
    if (verbose) {
      cat(sprintf("  Shapiro-Wilk P-value: %.4f\n", shapiro_p))
      if (!is.na(skewness_val)) {
        cat(sprintf("  Skewness: %.4f (threshold: %.2f)\n", skewness_val, skewness_threshold))
      }
      if (!is.na(kurtosis_val)) {
        cat(sprintf("  Kurtosis: %.4f (threshold: %.2f)\n", kurtosis_val, kurtosis_threshold))
      }
      if (normality_status) {
        cat("  [OK] Residuals are approximately normal\n")
      } else {
        if (n > 50) {
          cat("  [WARNING] Residuals deviate from normality\n")
          warnings_issues <- c(warnings_issues, "Normality deviation")
        } else {
          cat("  [CRITICAL] Residuals are NOT normal (small sample)\n")
          critical_issues <- c(critical_issues, "Normality (small sample)")
        }
        issues <- c(issues, "Normality: residuals not normal")
      }
    }
  }
  
  # ============================================================
  # 5. Multicollinearity (VIF/GVIF with proper scaling)
  # ============================================================
  if (verbose) {
    cat("\n5. MULTICOLLINEARITY (VIF/GVIF)\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
  }
  
  if (requireNamespace("car", quietly = TRUE) && n > p + 1) {
    vif_result <- tryCatch({
      car::vif(model)
    }, error = function(e) NULL)
    
    if (!is.null(vif_result)) {
      if (is.matrix(vif_result) && "GVIF" %in% colnames(vif_result)) {
        vif_adjusted <- vif_result[, "GVIF"]^(1 / (2 * vif_result[, "Df"]))
        names(vif_adjusted) <- rownames(vif_result)
        vif_type <- "GVIF"
      } else {
        vif_adjusted <- vif_result
        vif_type <- "VIF"
      }
      
      critical_vif <- vif_adjusted[vif_adjusted > vif_threshold]
      warning_vif <- vif_adjusted[vif_adjusted > vif_warning_threshold & vif_adjusted <= vif_threshold]
      vif_status <- length(critical_vif) == 0
      
      results$multicollinearity <- list(
        test = vif_type,
        values = vif_adjusted,
        critical_vif = critical_vif,
        warning_vif = warning_vif,
        status = vif_status,
        vif_threshold = vif_threshold,
        vif_warning_threshold = vif_warning_threshold
      )
      
      if (verbose) {
        if (vif_type == "GVIF") {
          cat(sprintf("  Using GVIF^(1/(2*Df)) for interpretation\n"))
        }
        if (length(critical_vif) == 0 && length(warning_vif) == 0) {
          cat(sprintf("  [OK] No multicollinearity (all adjusted VIF < %.1f)\n", vif_warning_threshold))
        } else if (length(critical_vif) > 0) {
          cat(sprintf("  [CRITICAL] High multicollinearity (> %.1f) detected:\n", vif_threshold))
          for (var in names(critical_vif)) {
            cat(sprintf("    %s: %.2f\n", var, critical_vif[var]))
          }
          issues <- c(issues, "Multicollinearity: high VIF detected")
          critical_issues <- c(critical_issues, "Multicollinearity")
        } else {
          cat(sprintf("  [WARNING] Moderate multicollinearity (%.1f-%.1f) detected:\n", 
                      vif_warning_threshold, vif_threshold))
          for (var in names(warning_vif)) {
            cat(sprintf("    %s: %.2f\n", var, warning_vif[var]))
          }
          warnings_issues <- c(warnings_issues, "Multicollinearity (moderate)")
          issues <- c(issues, "Multicollinearity: moderate VIF detected")
        }
      }
    }
  } else {
    if (verbose) {
      cat("  [INFO] Package 'car' not installed. Skipping VIF check.\n")
    }
    results$multicollinearity <- list(
      test = "not_run",
      status = NA,
      note = "Install 'car' package"
    )
  }
  
  # ============================================================
  # 6. Influential Points
  # ============================================================
  if (!is.null(data)) {
    if (verbose) {
      cat("\n6. INFLUENTIAL POINTS\n")
      cat(paste(rep("-", 40), collapse = ""), "\n")
    }
    
    # Variables to track errors and warnings
    influence_error <- NULL
    inf_result <- NULL
    
    # Use the extraction function with proper error handling
    tryCatch({
      inf_result <- .extract_influential_points(model, data, na_action)
    }, error = function(e) {
      # Use <<- here because we're inside a nested function (error handler)
      influence_error <<- e$message
      return(NULL)
    })
    
    # Check if error occurred - using regular <- because we're in the main body
    if (!is.null(influence_error)) {
      warning(sprintf("Could not calculate influential points: %s", influence_error), call. = FALSE)
      warnings_issues <- c(warnings_issues, "Influential points calculation failed")
      if (verbose) {
        cat(sprintf("  [WARNING] Could not calculate influential points: %s\n", influence_error))
      }
    } else if (!is.null(inf_result)) {
      # Store successful result
      results$influential_points <- inf_result
      
      # Check for critical issues - using regular <- (main body)
      if (inf_result$is_critical) {
        critical_issues <- c(critical_issues, "Data-model alignment issue")
      }
      
      # Add any warnings - using regular <- (main body)
      if (!is.null(inf_result$warning)) {
        warnings_issues <- c(warnings_issues, "FarmID mapping issue")
      }
      
      # Check for influential points - ALWAYS, regardless of verbose
      has_influential <- length(inf_result$cooks_distance$influential_indices) > 0 ||
                         length(inf_result$leverage$high_leverage_indices) > 0 ||
                         length(inf_result$dffits$influential_indices) > 0
      
      if (has_influential) {
        warnings_issues <- c(warnings_issues, "Influential points")
      }
      
      # Display results (only if verbose)
      if (verbose) {
        cooks <- inf_result$cooks_distance
        cat(sprintf("  Cook's distance threshold: %.3f\n", cooks$threshold))
        cat(sprintf("    Influential points: %d\n", length(cooks$influential_indices)))
        if (length(cooks$influential_indices) > 0 && !is.null(cooks$influential_ids)) {
          cat(sprintf("    Farm IDs: %s\n", 
                      paste(cooks$influential_ids, collapse = ", ")))
        } else if (length(cooks$influential_indices) > 0) {
          cat("    Farm IDs: [not available]\n")
        }
        
        lev <- inf_result$leverage
        cat(sprintf("  Leverage threshold: %.3f\n", lev$threshold))
        cat(sprintf("    High leverage points: %d\n", length(lev$high_leverage_indices)))
        if (length(lev$high_leverage_indices) > 0 && !is.null(lev$high_leverage_ids)) {
          cat(sprintf("    Farm IDs: %s\n", 
                      paste(lev$high_leverage_ids, collapse = ", ")))
        } else if (length(lev$high_leverage_indices) > 0) {
          cat("    Farm IDs: [not available]\n")
        }
        
        dff <- inf_result$dffits
        cat(sprintf("  DFFITS threshold: %.3f\n", dff$threshold))
        cat(sprintf("    Influential points: %d\n", length(dff$influential_indices)))
        if (length(dff$influential_indices) > 0 && !is.null(dff$influential_ids)) {
          cat(sprintf("    Farm IDs: %s\n", 
                      paste(dff$influential_ids, collapse = ", ")))
        } else if (length(dff$influential_indices) > 0) {
          cat("    Farm IDs: [not available]\n")
        }
        
        # Display status based on has_influential
        if (has_influential) {
          cat("  [WARNING] Influential points detected\n")
        } else {
          cat("  [OK] No influential points detected\n")
        }
        
        # Display mapping method info
        if (inf_result$mapping_method == "rowname_match") {
          cat("  [INFO] FarmID mapping: using row name matching\n")
        } else if (inf_result$mapping_method == "direct_aligned") {
          cat("  [INFO] FarmID mapping: direct alignment (no NA handling)\n")
        } else if (!inf_result$can_map_farmid) {
          cat("  [INFO] FarmID mapping: disabled - ", inf_result$mapping_method, "\n")
        }
      }
    }
  }
  
  # ============================================================
  # 7. Diagnostic plots (single drawing with recordPlot)
  # ============================================================
  if (plot) {
    if (verbose) {
      cat("\nGenerating diagnostic plots...\n")
    }
    
    # Store current graphics settings and restore them immediately on
    # exit (even if plotting below errors), per CRAN policy.
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)
    
    # Create the plots
    graphics::par(mfrow = c(2, 2))
    graphics::plot(model)
    
    # Record the plot for later use
    results$diagnostic_plot <- grDevices::recordPlot()
    
    if (verbose) {
      cat("  [OK] Diagnostic plots displayed and recorded\n")
    }
  }
  
  # ============================================================
  # 8. Summary with Data Readiness Status
  # ============================================================
  # Calculate summary statistics
  all_tests <- list(
    linearity = if (!is.null(results$linearity)) results$linearity$status else NA,
    independence = if (!is.null(results$independence)) results$independence$status else NA,
    homoscedasticity = if (!is.null(results$homoscedasticity)) results$homoscedasticity$status else NA,
    normality = if (!is.null(results$normality)) results$normality$status else NA,
    multicollinearity = if (!is.null(results$multicollinearity)) results$multicollinearity$status else NA
  )
  
  valid_tests <- all_tests[!sapply(all_tests, is.na)]
  passed <- sum(sapply(valid_tests, function(x) isTRUE(x)), na.rm = TRUE)
  total <- length(valid_tests)
  
  # Determine data readiness
  if (length(critical_issues) == 0) {
    if (length(warnings_issues) == 0) {
      data_ready <- TRUE
      data_ready_reason <- "No issues detected"
    } else if (length(warnings_issues) <= max_warnings) {
      data_ready <- TRUE
      data_ready_reason <- sprintf("%d warning(s) (<= %d allowed)", 
                                   length(warnings_issues), max_warnings)
    } else {
      data_ready <- FALSE
      data_ready_reason <- sprintf("%d warnings (> %d allowed)", 
                                   length(warnings_issues), max_warnings)
      critical_issues <- c(critical_issues, "Multiple warnings")
    }
  } else {
    data_ready <- FALSE
    data_ready_reason <- "Critical issues detected"
  }
  
  results$data_ready <- data_ready
  results$data_ready_reason <- data_ready_reason
  results$max_warnings <- max_warnings
  results$critical_issues <- critical_issues
  results$warnings_issues <- warnings_issues
  
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("SUMMARY\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    
    cat(sprintf("  Passed: %d / %d checks\n", passed, total))
    
    cat("\n", paste(rep("-", 40), collapse = ""), "\n")
    cat("DATA READINESS FOR NEXT STAGE\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat(sprintf("  max_warnings threshold: %d\n", max_warnings))
    cat(sprintf("  Warnings detected: %d\n", length(warnings_issues)))
    
    if (data_ready) {
      cat("  [READY] Data is ready for model interpretation and reporting.\n")
      cat("  Reason:", data_ready_reason, "\n")
      if (length(warnings_issues) > 0) {
        cat("  [NOTE] Minor issues detected:\n")
        for (w in warnings_issues) {
          cat(sprintf("    - %s\n", w))
        }
        cat("  These may affect inference but not model validity.\n")
      }
    } else {
      cat("  [NEEDS REVIEW] Data requires preprocessing before next stage.\n")
      cat("  Reason:", data_ready_reason, "\n")
      if (length(critical_issues) > 0) {
        cat("  Critical issues:\n")
        for (c in critical_issues) {
          cat(sprintf("    - %s\n", c))
        }
      }
      if (length(warnings_issues) > 0) {
        cat("  Warnings:\n")
        for (w in warnings_issues) {
          cat(sprintf("    - %s\n", w))
        }
      }
      cat("\n  Please run data preprocessing module before proceeding.\n")
    }
  }
  
  results$summary <- list(
    passed = passed,
    total = total,
    issues = issues,
    critical_issues = critical_issues,
    warnings_issues = warnings_issues,
    all_passed = length(issues) == 0,
    data_ready = data_ready,
    data_ready_reason = data_ready_reason,
    max_warnings = max_warnings
  )
  
  return(invisible(results))
}

# ============================================================
# Quick Model Summary with Assumption Checks
# ============================================================

#' Quick Model Summary with Assumption Checks
#'
#' Combines model summary and assumption checks in one function.
#'
#' @param model A fitted \code{lm} model object.
#' @param data Optional data frame. May be either complete-case data or original
#'   data with NAs (row names used for mapping).
#' @param plot Logical. Show diagnostic plots. Default: TRUE.
#' @param verbose Logical. Print results. Default: TRUE.
#' @param max_warnings Integer. Maximum number of warnings allowed for data readiness.
#'   Default: 2.
#' @param ... Additional arguments passed to \code{check_assumptions}.
#'
#' @return A list with model summary and assumption checks.
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' model <- lm(Yield ~ ., data = prep$data)
#' result <- model_diagnostics(model, data = prep$data, plot = FALSE, verbose = FALSE)
#' result$summary$r.squared
#'
#' @export
model_diagnostics <- function(model, data = NULL, plot = TRUE, verbose = TRUE, 
                              max_warnings = 2, ...) {
  
  if (!inherits(model, "lm")) {
    stop("'model' must be an object of class 'lm'")
  }
  
  model_summary <- summary(model)
  
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("MODEL SUMMARY\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat(sprintf("R-squared: %.4f\n", model_summary$r.squared))
    cat(sprintf("Adjusted R-squared: %.4f\n", model_summary$adj.r.squared))
    cat(sprintf("Residual standard error: %.4f\n", model_summary$sigma))
    cat(sprintf("F-statistic: %.4f (p-value: %.4e)\n", 
                model_summary$fstatistic[1],
                stats::pf(model_summary$fstatistic[1], 
                          model_summary$fstatistic[2], 
                          model_summary$fstatistic[3], 
                          lower.tail = FALSE)))
  }
  
  assumptions <- check_assumptions(model, data, plot = plot, verbose = verbose, 
                                   max_warnings = max_warnings, ...)
  
  return(list(
    summary = model_summary,
    assumptions = assumptions
  ))
}
