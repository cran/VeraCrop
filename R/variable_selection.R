
# ============================================================
# File: R/variable_selection.R
# VeraCrop: Forward F-test Variable Selection with F-to-Enter/F-to-Remove
# ============================================================

#' Forward Stepwise Variable Selection Using F-test
#'
#' Selects significant predictor variables using a forward stepwise algorithm
#' based on partial F-tests. Supports both P-value threshold and 
#' F-to-Enter/F-to-Remove criteria (like SigmaPlot).
#'
#' @param data A data frame containing predictor and response variables.
#' @param response Character. Name of the response variable. Default: "Yield".
#' @param p_threshold Numeric. Significance threshold (alpha). Default: 0.05.
#' @param use_bonferroni Logical. Apply Bonferroni correction. Default: TRUE.
#' @param max_vars Integer or NULL. Maximum variables to select. Default: NULL (no limit).
#' @param f_to_enter Numeric or NULL. F-to-Enter threshold (like SigmaPlot).
#'   Default: NULL (uses p_threshold instead).
#' @param f_to_remove Numeric or NULL. F-to-Remove threshold (like SigmaPlot).
#'   Default: NULL (uses p_threshold instead).
#' @param verbose Logical. Print progress. Default: TRUE.
#'
#' @return A list with components:
#'   \item{final_model}{The selected linear model (\code{lm} object).}
#'   \item{significant_vars}{Character vector of selected variable names.}
#'   \item{step_history}{Data frame recording each step.}
#'   \item{method}{Character, method used ("p_value" or "f_to_enter").}
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' sel <- select_vars_ftest(prep$data, response = "Yield", verbose = FALSE)
#' sel$significant_vars
#'
#' @importFrom stats lm as.formula df.residual residuals pf
#' @export
select_vars_ftest <- function(data,
                                           response = "Yield",
                                           p_threshold = 0.05,
                                           use_bonferroni = TRUE,
                                           max_vars = NULL,
                                           f_to_enter = NULL,
                                           f_to_remove = NULL,
                                           verbose = TRUE) {

  # Validate inputs
  if (!response %in% names(data)) {
    stop("Response variable '", response, "' not found in data.")
  }

  all_predictors <- names(data)[names(data) != response]
  if (length(all_predictors) == 0) {
    warning("No predictor variables found.")
    return(list(final_model = NULL, significant_vars = character(0)))
  }

  # Determine selection method
  use_f_to_enter <- !is.null(f_to_enter)
  
  if (use_f_to_enter) {
    if (verbose) {
      cat("Using F-to-Enter =", f_to_enter, 
          "(equivalent to P <", format(pf(f_to_enter, 1, nrow(data) - 2, lower.tail = FALSE), 
                                         scientific = FALSE, digits = 4), ")\n")
      if (!is.null(f_to_remove)) {
        cat("Using F-to-Remove =", f_to_remove, "\n")
      }
    }
    # Convert F-to-Enter to equivalent P-value
    df1 <- 1
    df2 <- nrow(data) - 2
    p_threshold <- pf(f_to_enter, df1, df2, lower.tail = FALSE)
  }

  # Apply Bonferroni correction if needed
  n_tests <- length(all_predictors)
  if (use_bonferroni && !use_f_to_enter) {
    adjusted_threshold <- p_threshold / n_tests
    if (verbose) {
      cat("Bonferroni correction: alpha =", round(adjusted_threshold, 6),
          "(original:", p_threshold, "for", n_tests, "tests)\n")
    }
  } else if (use_f_to_enter) {
    adjusted_threshold <- p_threshold  # Bonferroni not used with F-to-Enter
    if (verbose) {
      cat("F-to-Enter method: using unadjusted P =", format(p_threshold, scientific = FALSE, digits = 4), "\n")
    }
  } else {
    adjusted_threshold <- p_threshold
    if (verbose) {
      cat("Forward F-test selection (p <", round(adjusted_threshold, 6), "):\n")
    }
  }

  # Set max_vars default (no limit if NULL)
  if (is.null(max_vars)) {
    max_vars <- length(all_predictors)
  }

  # Initialize selection
  selected_vars <- character(0)
  remaining_vars <- all_predictors
  current_formula <- as.formula(paste(response, "~ 1"))
  current_model <- lm(current_formula, data = data)
  continue_selection <- TRUE

  step_history <- data.frame(
    Step = integer(),
    Variable = character(),
    F_stat = numeric(),
    P_value = numeric(),
    R2 = numeric(),
    stringsAsFactors = FALSE
  )

  # Main selection loop
  while (continue_selection && length(remaining_vars) > 0 && length(selected_vars) < max_vars) {

    best_f_stat <- -Inf
    best_var <- NULL
    best_model <- NULL
    best_p_val <- NA

    # Calculate F-to-Enter for each remaining variable
    for (v in remaining_vars) {
      new_formula <- as.formula(paste(response, "~", paste(c(selected_vars, v), collapse = " + ")))
      new_model <- lm(new_formula, data = data)

      ss_reduced <- sum(residuals(current_model)^2)
      ss_full <- sum(residuals(new_model)^2)
      df_reduced <- df.residual(current_model)
      df_full <- df.residual(new_model)

      if (df_reduced > df_full && ss_full > 0) {
        f_stat <- ((ss_reduced - ss_full) / (df_reduced - df_full)) / (ss_full / df_full)
        p_val <- pf(f_stat, df_reduced - df_full, df_full, lower.tail = FALSE)
      } else {
        f_stat <- NA
        p_val <- NA
      }

      # Selection criteria: F-to-Enter or P-value
      if (!is.na(f_stat) && !is.na(p_val)) {
        if (use_f_to_enter) {
          if (f_stat > f_to_enter && f_stat > best_f_stat) {
            best_f_stat <- f_stat
            best_var <- v
            best_model <- new_model
            best_p_val <- p_val
          }
        } else {
          if (p_val < adjusted_threshold && f_stat > best_f_stat) {
            best_f_stat <- f_stat
            best_var <- v
            best_model <- new_model
            best_p_val <- p_val
          }
        }
      }
    }

    # If a variable was selected
    if (!is.null(best_var)) {
      selected_vars <- c(selected_vars, best_var)
      remaining_vars <- setdiff(remaining_vars, best_var)
      current_model <- best_model
      current_r2 <- summary(current_model)$r.squared

      step_history <- rbind(step_history, data.frame(
        Step = length(selected_vars),
        Variable = best_var,
        F_stat = round(best_f_stat, 3),
        P_value = format(best_p_val, scientific = TRUE, digits = 4),
        R2 = round(current_r2, 4)
      ))

      if (verbose) {
        cat("  Added:", best_var, "(F =", round(best_f_stat, 3),
            ", P =", format(best_p_val, scientific = TRUE, digits = 4),
            ", R2 =", round(current_r2, 4), ")\n")
      }

      # Check F-to-Remove for existing variables
      if (use_f_to_enter && !is.null(f_to_remove) && length(selected_vars) > 1) {
        vars_to_remove <- character(0)
        for (v in selected_vars) {
          vars_test <- setdiff(selected_vars, v)
          test_formula <- as.formula(paste(response, "~", paste(vars_test, collapse = " + ")))
          test_model <- lm(test_formula, data = data)
          
          ss_reduced <- sum(residuals(test_model)^2)
          ss_full <- sum(residuals(current_model)^2)
          df_reduced <- df.residual(test_model)
          df_full <- df.residual(current_model)
          
          if (df_reduced > df_full && ss_full > 0) {
            f_remove_val <- ((ss_reduced - ss_full) / (df_reduced - df_full)) / (ss_full / df_full)
            p_remove <- pf(f_remove_val, df_reduced - df_full, df_full, lower.tail = FALSE)
            
            if (f_remove_val < f_to_remove) {
              vars_to_remove <- c(vars_to_remove, v)
              if (verbose) {
                cat("  Removing:", v, "(F =", round(f_remove_val, 3),
                    ", P =", format(p_remove, scientific = TRUE, digits = 4), ")\n")
              }
            }
          }
        }
        
        # Remove variables
        if (length(vars_to_remove) > 0) {
          selected_vars <- setdiff(selected_vars, vars_to_remove)
          remaining_vars <- c(remaining_vars, vars_to_remove)
          # Rebuild model
          current_formula <- as.formula(paste(response, "~", paste(selected_vars, collapse = " + ")))
          current_model <- lm(current_formula, data = data)
        }
      }

    } else {
      continue_selection <- FALSE
    }
  }

  # Build final model
  if (length(selected_vars) == 0) {
    warning("No significant variables found.")
    return(list(final_model = current_model, 
                significant_vars = character(0), 
                step_history = step_history,
                method = ifelse(use_f_to_enter, "f_to_enter", "p_value")))
  }

  final_formula <- as.formula(paste(response, "~", paste(selected_vars, collapse = " + ")))
  final_model <- lm(final_formula, data = data)

  if (verbose) {
    cat("\n  Final variables:", paste(selected_vars, collapse = ", "), "\n")
    cat("  Final R2 =", round(summary(final_model)$r.squared, 4), "\n")
  }

  return(list(
    final_model = final_model,
    significant_vars = selected_vars,
    step_history = step_history,
    method = ifelse(use_f_to_enter, "f_to_enter", "p_value")
  ))
}


#' Variable Selection via AIC-Based Stepwise Regression
#'
#' Selects variables by minimizing AIC (Akaike Information Criterion) using
#' \code{\link[stats]{step}}, as an alternative to F-test-based selection
#' (see \code{\link{select_vars_ftest}}).
#'
#' AIC-based and F-test-based selection answer different questions and will
#' often disagree on how many variables to keep: AIC-based stepwise
#' selection optimizes overall model fit (penalized by model complexity)
#' and has no fixed significance threshold, so it will often retain MORE
#' variables than a strict F-test-based approach with typical
#' F-to-Enter/F-to-Remove thresholds (e.g. around 4 and 3.9, as used by
#' default in software such as SigmaPlot) - variables that improve AIC
#' slightly may not individually clear a fixed F/p-value bar. Neither
#' method is universally "correct"; they encode different criteria for
#' what counts as a useful predictor.
#'
#' @param data A data frame.
#' @param response Character. Name of the response variable. Default: "Yield".
#' @param direction Character, one of "both", "forward", "backward". Passed
#'   to \code{\link[stats]{step}}. Default: "both".
#' @param verbose Logical. Print progress and the AIC trace. Default: TRUE.
#'
#' @return A list with:
#'   \item{final_model}{The final fitted \code{lm} model, or NULL if
#'     selection failed or no predictors were available.}
#'   \item{significant_vars}{Character vector of selected variable names.}
#'   \item{step_history}{The step-by-step ANOVA/AIC trace from
#'     \code{stats::step()}, or NULL.}
#'   \item{method}{Always \code{"aic"}.}
#'
#' @examples
#' df <- data.frame(Yield = rnorm(40, 5000, 500),
#'                   Nitrogen = rnorm(40, 120, 20),
#'                   Phosphorus = rnorm(40, 40, 10))
#' sel <- select_vars_aic(df, response = "Yield", verbose = FALSE)
#' sel$significant_vars
#'
#' @export
select_vars_aic <- function(data, response = "Yield",
                                        direction = "both", verbose = TRUE) {

  direction <- match.arg(direction, c("both", "forward", "backward"))

  if (!is.data.frame(data)) stop("'data' must be a data frame")
  if (!response %in% names(data)) {
    stop(sprintf("Response variable \"%s\" not found in data", response))
  }

  predictors <- setdiff(names(data), response)
  if (length(predictors) == 0) {
    if (verbose) cat("  No predictor variables available.\n")
    return(list(final_model = NULL, significant_vars = character(0),
                step_history = NULL, method = "aic"))
  }

  full_formula <- stats::as.formula(
    paste(response, "~", paste(predictors, collapse = " + "))
  )

  # Check for aliased (perfectly collinear) predictors before running
  # step(): these produce NA coefficients that step() can error or behave
  # unpredictably on. Drop them up front with a clear warning instead.
  full_fit <- tryCatch(stats::lm(full_formula, data = data),
                        error = function(e) NULL)
  if (is.null(full_fit)) {
    warning("Could not fit the full model for AIC-based selection.", call. = FALSE)
    return(list(final_model = NULL, significant_vars = character(0),
                step_history = NULL, method = "aic"))
  }

  aliased_coefs <- names(which(is.na(stats::coef(full_fit))))
  if (length(aliased_coefs) > 0) {
    predictors <- setdiff(predictors, aliased_coefs)
    warning(sprintf(
      "Dropping perfectly collinear (aliased) predictor(s) before AIC selection: %s",
      paste(aliased_coefs, collapse = ", ")
    ), call. = FALSE)
    if (length(predictors) == 0) {
      return(list(final_model = NULL, significant_vars = character(0),
                  step_history = NULL, method = "aic"))
    }
    full_formula <- stats::as.formula(
      paste(response, "~", paste(predictors, collapse = " + "))
    )
    full_fit <- stats::lm(full_formula, data = data)
  }

  trace_level <- if (verbose) 1 else 0

  step_result <- tryCatch({
    if (direction == "forward") {
      start_model <- stats::lm(stats::as.formula(paste(response, "~ 1")), data = data)
      stats::step(start_model,
                  scope = list(lower = ~1, upper = full_formula),
                  direction = "forward", trace = trace_level)
    } else {
      stats::step(full_fit, direction = direction, trace = trace_level)
    }
  }, error = function(e) {
    warning(sprintf("AIC-based selection failed: %s", e$message), call. = FALSE)
    NULL
  })

  if (is.null(step_result)) {
    return(list(final_model = NULL, significant_vars = character(0),
                step_history = NULL, method = "aic"))
  }

  final_model <- step_result
  final_vars <- setdiff(all.vars(stats::formula(final_model)), response)

  if (verbose) {
    cat("\n  Final variables (AIC):", paste(final_vars, collapse = ", "), "\n")
    cat("  Final R2 =", round(summary(final_model)$r.squared, 4), "\n")
    cat("  Final AIC =", round(stats::AIC(final_model), 2), "\n")
  }

  list(
    final_model = final_model,
    significant_vars = final_vars,
    step_history = final_model$anova,
    method = "aic"
  )
}


#' k-Fold Cross-Validation for Yield Gap Model
#'
#' Evaluates model predictive performance using k-fold cross-validation.
#' A NEW model is refit on each training fold (using \code{sig_vars} and
#' \code{response}); the \code{model} argument is used only to validate
#' that \code{sig_vars}/\code{response} are consistent with an already-
#' fitted whole-data model, not refit directly, since cross-validation by
#' definition requires refitting on each fold's training subset.
#'
#' @param model A fitted \code{lm} model, used only for consistency
#'   validation (its response variable must match \code{response}). Pass
#'   \code{NULL} to skip this check.
#' @param data A data frame.
#' @param sig_vars Character vector of selected predictor variable names.
#' @param response Character. Name of the response variable. Default: "Yield".
#' @param k_folds Integer. Number of folds. Default: 5.
#' @param seed Integer or NULL. If supplied, used to make the fold
#'   assignment reproducible; the caller's global random state is saved
#'   before, and restored after, this function runs, so calling this
#'   function never permanently alters the user's RNG stream. Default: NULL
#'   (fold assignment uses whatever RNG state is currently active, exactly
#'   like any other call to \code{sample()} would).
#'
#' @return A list with components: by_fold and summary.
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' sel <- select_vars_ftest(prep$data, response = "Yield", verbose = FALSE)
#' cv <- validate_model_with_cv(sel$final_model, prep$data, sel$significant_vars,
#'                               response = "Yield", k_folds = 5, seed = 42)
#' cv$summary
#'
#' @importFrom stats lm as.formula predict
#' @export
validate_model_with_cv <- function(model, data, sig_vars, response = "Yield",
                                   k_folds = 5, seed = NULL) {

  # `model` is optional context for a sanity check only - it is never
  # refit or reused directly (see function-level docs above).
  if (!is.null(model)) {
    if (!inherits(model, "lm")) {
      stop("'model' must be an object of class 'lm', or NULL")
    }
    model_response <- all.vars(stats::formula(model))[1]
    if (!identical(model_response, response)) {
      warning(sprintf(
        "'model' was fit with response \"%s\", but response = \"%s\" was requested for cross-validation; proceeding with \"%s\".",
        model_response, response, response
      ), call. = FALSE)
    }
  }

  # Save and restore the caller's global RNG state so that this function
  # never has a persistent side effect on the user's random number stream,
  # even when `seed` is supplied for reproducible fold assignment.
  if (!is.null(seed)) {
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
  }

  n <- nrow(data)
  folds <- sample(rep(1:k_folds, length.out = n))
  formula_model <- as.formula(paste(response, "~", paste(sig_vars, collapse = " + ")))

  cv_results <- data.frame(
    Fold = 1:k_folds,
    R2 = NA_real_,
    RMSE = NA_real_,
    MAE = NA_real_
  )

  for (k in 1:k_folds) {
    train_data <- data[folds != k, ]
    test_data <- data[folds == k, ]

    model_cv <- lm(formula_model, data = train_data)
    pred <- predict(model_cv, newdata = test_data)
    actual <- test_data[[response]]

    ss_res <- sum((actual - pred)^2, na.rm = TRUE)
    ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)

    cv_results[k, "R2"] <- if (ss_tot > 0 && length(actual) > 1) 1 - ss_res / ss_tot else NA
    cv_results[k, "RMSE"] <- sqrt(mean((actual - pred)^2, na.rm = TRUE))
    cv_results[k, "MAE"] <- mean(abs(actual - pred), na.rm = TRUE)
  }

  summary_cv <- data.frame(
    Metric = c("R2", "RMSE", "MAE"),
    Mean = c(mean(cv_results$R2, na.rm = TRUE),
             mean(cv_results$RMSE, na.rm = TRUE),
             mean(cv_results$MAE, na.rm = TRUE)),
    SD = c(stats::sd(cv_results$R2, na.rm = TRUE),
           stats::sd(cv_results$RMSE, na.rm = TRUE),
           stats::sd(cv_results$MAE, na.rm = TRUE))
  )

  return(list(by_fold = cv_results, summary = summary_cv))
}

