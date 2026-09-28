
# ============================================================
# File: R/yield_gap_core.R
# VeraCrop: Core Yield Gap Calculation Functions
# ============================================================

#' Calculate Optimal Values for Selected Variables
#'
#' Determines the optimal value for each selected predictor variable.
#'
#' @param data A data frame (full dataset).
#' @param vars Character vector of selected variable names.
#' @param coefs Named numeric vector of model coefficients.
#' @param data_top A data frame of high-yield observations.
#' @param response_var Character. Name of the response variable. Default: "Yield".
#' @param top_percentile Numeric. Percentile used to define top yields. Default: 0.99.
#' @param min_top_samples Integer. Minimum number of top-yield samples. Default: 20.
#'
#' @return A list with components: var_table, used_percentile, n_top_samples, n_total.
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' sel <- select_vars_ftest(prep$data, response = "Yield", verbose = FALSE)
#' sig_vars <- sel$significant_vars
#' coefs_vars <- coef(sel$final_model)[sig_vars]
#' yield_thr <- quantile(prep$data$Yield, 0.90, na.rm = TRUE)
#' data_top <- prep$data[prep$data$Yield >= yield_thr, ]
#' opt <- calculate_opt_values(prep$data, sig_vars, coefs_vars, data_top,
#'                              response_var = "Yield", top_percentile = 0.90)
#' opt$var_table
#'
#' @importFrom stats quantile
#' @export
calculate_opt_values <- function(data, vars, coefs, data_top,
                                  response_var    = "Yield",
                                  top_percentile  = 0.99,
                                  min_top_samples = 20) {
  
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  if (!is.character(vars) || length(vars) == 0) stop("'vars' must be a non-empty character vector")
  if (!is.numeric(coefs) || length(coefs) == 0) stop("'coefs' must be a named numeric vector")
  if (!is.data.frame(data_top)) stop("'data_top' must be a data frame")
  
  n_total <- nrow(data)
  n_top_original <- nrow(data_top)
  
  if (n_top_original < min_top_samples && n_total > min_top_samples) {
    new_top_percentile <- 1 - min_top_samples / n_total
    new_top_percentile <- min(new_top_percentile, 0.95)
    new_top_percentile <- max(new_top_percentile, 0.80)
    
    warning("Adjusting top percentile to ", (1 - new_top_percentile) * 100, "%")
    top_percentile <- new_top_percentile
    yield_thr <- quantile(data[[response_var]], top_percentile, na.rm = TRUE)
    data_top <- data[data[[response_var]] >= yield_thr, ]
  }
  
  var_table <- data.frame(
    Variable = vars,
    Min = NA_real_, Mean = NA_real_, Max = NA_real_, Opt = NA_real_,
    VarType = NA_character_, N_Unique = NA_integer_, Opt_Method = NA_character_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(vars)) {
    v <- vars[i]
    if (!v %in% names(data)) { warning("Variable '", v, "' not found."); next }
    if (!v %in% names(coefs)) { warning("Coefficient for '", v, "' not found."); next }
    
    x <- data[[v]]
    x_top <- data_top[[v]]
    if (all(is.na(x_top))) x_top <- x
    
    var_table$Min[i] <- min(x, na.rm = TRUE)
    var_table$Mean[i] <- mean(x, na.rm = TRUE)
    var_table$Max[i] <- max(x, na.rm = TRUE)
    
    uniq_vals <- unique(stats::na.omit(x))
    n_unique <- length(uniq_vals)
    var_table$N_Unique[i] <- n_unique
    
    # A variable with exactly two distinct values is structurally binary
    # regardless of what those two values are - 0/1, TRUE/FALSE, or two
    # arbitrary standardized values after scaling. This is deliberately
    # simpler and more general than checking uniq_vals %in% c(0, 1) (which
    # fails for a standardized binary column) or relying on a hardcoded,
    # inevitably incomplete list of name patterns (^Cultivar_, _Stress$,
    # _Infestation$) that does not cover e.g. "Pest_scaled" or
    # "Drought_scaled".
    is_binary <- n_unique == 2

    coef_val <- coefs[v]
    if (is.na(coef_val)) {
      var_table$Opt[i] <- var_table$Mean[i]
      var_table$VarType[i] <- "unknown"
      var_table$Opt_Method[i] <- "mean_fallback"
      next
    }
    
    if (is_binary) {
      var_table$VarType[i] <- "binary"
      # Use the variable's own observed Max/Min as the "positive"/"negative"
      # state, rather than hardcoding 1/0. For an unscaled 0/1 indicator
      # these are identical to 1/0. For a STANDARDIZED binary/dummy
      # variable (e.g. "Cultivar_Sardari_scaled", produced when scaling is
      # applied to all numeric columns without excluding dummy variables),
      # the two real states are NOT 0 and 1 anymore - they are whatever
      # value (x - center)/scale maps 0 and 1 to. Max/Min of the actual
      # data always correctly identify those two real states, scaled or not.
      if (coef_val > 0) { var_table$Opt[i] <- var_table$Max[i]; var_table$Opt_Method[i] <- "binary_positive_1" }
      else if (coef_val < 0) { var_table$Opt[i] <- var_table$Min[i]; var_table$Opt_Method[i] <- "binary_negative_0" }
      else { var_table$Opt[i] <- var_table$Mean[i]; var_table$Opt_Method[i] <- "binary_zero_fallback" }
    } else {
      var_table$VarType[i] <- "continuous"
      n_top <- sum(!is.na(x_top))
      if (coef_val > 0) {
        opt_val <- if (n_top >= 20) mean(x_top, na.rm = TRUE, trim = 0.05) else mean(x_top, na.rm = TRUE)
        var_table$Opt[i] <- ifelse(is.na(opt_val) || is.nan(opt_val), var_table$Mean[i], opt_val)
        var_table$Opt_Method[i] <- "continuous_positive_top_mean"
      } else if (coef_val < 0) {
        opt_val <- if (n_top >= 20) mean(x_top, na.rm = TRUE, trim = 0.05) else mean(x_top, na.rm = TRUE)
        var_table$Opt[i] <- ifelse(is.na(opt_val) || is.nan(opt_val), var_table$Min[i], opt_val)
        var_table$Opt_Method[i] <- "continuous_negative_top_mean"
      } else {
        var_table$Opt[i] <- var_table$Mean[i]
        var_table$Opt_Method[i] <- "continuous_zero_fallback"
      }
    }
  }
  
  var_table <- var_table[!is.na(var_table$Variable), , drop = FALSE]
  
  return(list(
    var_table = var_table,
    used_percentile = top_percentile,
    n_top_samples = nrow(data_top),
    n_total = n_total
  ))
}


#' Calculate Min-Max Effects of Variables on Yield
#'
#' Estimates the effect of each variable on yield by comparing predictions
#' when the variable is set to its minimum vs. maximum.
#'
#' @param model A fitted \code{lm} model object.
#' @param data A data frame used for prediction.
#' @param vars Character vector of variable names to evaluate.
#' @param use_quantiles Logical. Use 5th-95th percentile range. Default: TRUE.
#' @param quantile_range Numeric vector of length 2. Default: c(0.05, 0.95).
#'
#' @return A data frame with columns: Variable, Effect, EffectAbs, EffectPct.
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' sel <- select_vars_ftest(prep$data, response = "Yield", verbose = FALSE)
#' mm <- calculate_minmax_effects(sel$final_model, prep$data, sel$significant_vars)
#' mm
#'
#' @importFrom stats quantile predict
#' @export
calculate_minmax_effects <- function(model, data, vars,
                                      use_quantiles = TRUE,
                                      quantile_range = c(0.05, 0.95)) {
  
  if (!inherits(model, "lm")) stop("'model' must be an object of class 'lm'")
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  if (!is.character(vars) || length(vars) == 0) stop("'vars' must be a non-empty character vector")
  
  effects <- numeric(length(vars))
  valid_vars <- character(0)
  
  for (i in seq_along(vars)) {
    v <- vars[i]
    if (!v %in% names(data)) { warning("Variable '", v, "' not found."); next }
    
    x <- data[[v]]
    if (all(is.na(x))) { warning("Variable '", v, "' contains only NA values."); next }
    
    valid_vars <- c(valid_vars, v)
    
    if (use_quantiles) {
      x_min <- quantile(x, quantile_range[1], na.rm = TRUE)
      x_max <- quantile(x, quantile_range[2], na.rm = TRUE)
    } else {
      x_min <- min(x, na.rm = TRUE)
      x_max <- max(x, na.rm = TRUE)
    }
    
    data_temp <- data
    data_temp[[v]] <- x_max
    y_max_pred <- predict(model, newdata = data_temp)
    
    data_temp[[v]] <- x_min
    y_min_pred <- predict(model, newdata = data_temp)
    
    effects[i] <- mean(y_max_pred - y_min_pred, na.rm = TRUE)
  }
  
  effects <- effects[vars %in% valid_vars]
  valid_vars <- valid_vars[!is.na(effects)]
  effects <- effects[!is.na(effects)]
  
  if (length(valid_vars) == 0) {
    return(data.frame(Variable = character(0), Effect = numeric(0),
                      EffectAbs = numeric(0), EffectPct = numeric(0)))
  }
  
  effects_abs <- abs(effects)
  effects_pct <- if (sum(effects_abs) > 0) effects_abs / sum(effects_abs) * 100 else rep(0, length(effects))
  
  result <- data.frame(Variable = valid_vars, Effect = effects,
                       EffectAbs = effects_abs, EffectPct = effects_pct,
                       stringsAsFactors = FALSE)
  result <- result[order(-result$EffectPct), ]
  rownames(result) <- NULL
  
  return(result)
}


#' Calculate Relative Importance of Variables
#'
#' Computes the relative importance of predictors using LMG or standardized beta.
#'
#' @param data A data frame containing the variables.
#' @param vars Character vector of variable names.
#' @param response Character. Name of the response variable. Default: "Yield".
#' @param method Character. One of "lmg" or "std_beta". Default: "lmg".
#' @param verbose Logical. Print progress messages. Default: TRUE.
#'
#' @return A list with components: method and results.
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' sel <- select_vars_ftest(prep$data, response = "Yield", verbose = FALSE)
#' ri <- calculate_relative_importance(prep$data, sel$significant_vars,
#'                                      response = "Yield", verbose = FALSE)
#' ri$results
#'
#' @importFrom stats lm as.formula coef
#' @export
calculate_relative_importance <- function(data, vars, response = "Yield",
                                           method = c("lmg", "std_beta"),
                                           verbose = TRUE) {
  
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  if (!is.character(vars) || length(vars) < 2) stop("'vars' must have at least 2 variables")
  
  method <- match.arg(method)
  valid_vars <- vars[vars %in% names(data)]
  if (length(valid_vars) < 2) return(list(method = method, results = NULL))
  
  complete_cases <- stats::complete.cases(data[, c(response, valid_vars)])
  data_complete <- data[complete_cases, ]
  
  if (method == "lmg") {
    if (!requireNamespace("relaimpo", quietly = TRUE)) {
      if (verbose) message("Package 'relaimpo' not installed. Falling back to 'std_beta'.")
      method <- "std_beta"
    } else {
      formula_lmg <- as.formula(paste(response, "~", paste(valid_vars, collapse = " + ")))
      model_lmg <- lm(formula_lmg, data = data_complete)
      
      lmg_result <- tryCatch(
        relaimpo::calc.relimp(model_lmg, type = "lmg", rela = TRUE, boot = FALSE),
        error = function(e) { warning("LMG failed: ", e$message); NULL }
      )
      
      if (!is.null(lmg_result)) {
        lmg_values <- lmg_result@lmg
        result <- data.frame(
          Variable = names(lmg_values),
          LMG = lmg_values,
          LMG_Percent = lmg_values / sum(lmg_values) * 100,
          stringsAsFactors = FALSE
        )
        result <- result[order(-result$LMG_Percent), ]
        rownames(result) <- NULL
        return(list(method = "lmg", results = result))
      } else {
        method <- "std_beta"
      }
    }
  }
  
  # Standardized beta method (fallback)
  X <- data_complete[, valid_vars, drop = FALSE]
  y <- data_complete[[response]]
  
  X_scaled <- scale(X)
  y_scaled <- scale(y)
  
  df_std <- data.frame(y_scaled, X_scaled)
  names(df_std)[1] <- response
  
  std_model <- lm(as.formula(paste(response, "~ .")), data = df_std)
  beta_std <- coef(std_model)[-1]
  
  rel_pct <- abs(beta_std) / sum(abs(beta_std)) * 100
  
  result <- data.frame(
    Variable = valid_vars,
    StdBeta = beta_std,
    RelImportancePct = rel_pct,
    stringsAsFactors = FALSE
  )
  result <- result[order(-result$RelImportancePct), ]
  rownames(result) <- NULL
  
  return(list(method = "std_beta", results = result))
}


#' Calculate Yield Gap Components from CPA Table
#'
#' Computes contributions of each variable to the yield gap.
#'
#' @param cpa_table A data frame with columns: Variable, Beta, Mean, Opt.
#'
#' @return A list with components: cpa_table, yield_mean, yield_opt, yield_gap,
#'   gap_positive, gap_negative.
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' cpa_input <- results$cpa_table[, c("Variable", "Beta", "Mean", "Opt")]
#' yg <- yield_gap_components(cpa_input)
#' yg$yield_gap
#'
#' @export
yield_gap_components <- function(cpa_table) {
  
  if (!is.data.frame(cpa_table)) stop("'cpa_table' must be a data frame")
  required_cols <- c("Variable", "Beta", "Mean", "Opt")
  if (!all(required_cols %in% names(cpa_table))) {
    stop("'cpa_table' must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  cpa_table_adj <- cpa_table
  cpa_table_adj$Contribution_Mean <- cpa_table_adj$Beta * cpa_table_adj$Mean
  cpa_table_adj$Contribution_Opt <- cpa_table_adj$Beta * cpa_table_adj$Opt
  cpa_table_adj$Gap_Component <- cpa_table_adj$Contribution_Opt - cpa_table_adj$Contribution_Mean
  
  yield_mean <- sum(cpa_table_adj$Contribution_Mean, na.rm = TRUE)
  yield_opt <- sum(cpa_table_adj$Contribution_Opt, na.rm = TRUE)
  yield_gap <- sum(cpa_table_adj$Gap_Component, na.rm = TRUE)
  
  cpa_table_adj$Share_Percent <- NA_real_
  cpa_table_adj$Share_Percent_Abs <- NA_real_
  cpa_table_adj$Share_Cumulative <- NA_real_
  
  non_intercept <- cpa_table_adj$Variable != "Intercept"
  
  if (yield_gap != 0 && sum(non_intercept) > 0) {
    cpa_table_adj$Share_Percent[non_intercept] <-
      cpa_table_adj$Gap_Component[non_intercept] / yield_gap * 100
    
    abs_gap_sum <- sum(abs(cpa_table_adj$Gap_Component[non_intercept]), na.rm = TRUE)
    if (abs_gap_sum > 0) {
      cpa_table_adj$Share_Percent_Abs[non_intercept] <-
        abs(cpa_table_adj$Gap_Component[non_intercept]) / abs_gap_sum * 100
    }
    
    sorted_idx <- order(abs(cpa_table_adj$Gap_Component[non_intercept]), decreasing = TRUE)
    cumsum_share <- cumsum(cpa_table_adj$Share_Percent_Abs[non_intercept][sorted_idx])
    cpa_table_adj$Share_Cumulative[non_intercept][sorted_idx] <- cumsum_share
  }
  
  return(list(
    cpa_table = cpa_table_adj,
    yield_mean = yield_mean,
    yield_opt = yield_opt,
    yield_gap = yield_gap,
    gap_positive = sum(cpa_table_adj$Gap_Component[cpa_table_adj$Gap_Component > 0], na.rm = TRUE),
    gap_negative = sum(cpa_table_adj$Gap_Component[cpa_table_adj$Gap_Component < 0], na.rm = TRUE)
  ))
}


#' Pareto Analysis of Yield Gap Contributions
#'
#' Identifies the most important variables contributing to the yield gap.
#'
#' @param yg_results List. Output from \code{\link{yield_gap_components}}.
#' @param top_n Integer or NULL. Number of top variables to report.
#' @param verbose Logical. Print the Pareto table to the console.
#'   Default: TRUE.
#'
#' @return A list with components: pareto_table, top_n, top_variables.
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' cpa_input <- results$cpa_table[, c("Variable", "Beta", "Mean", "Opt")]
#' yg <- yield_gap_components(cpa_input)
#' pareto <- pareto_analysis(yg, verbose = FALSE)
#' pareto$pareto_table
#'
#' @export
pareto_analysis <- function(yg_results, top_n = NULL, verbose = TRUE) {

  if (!is.list(yg_results) || !"cpa_table" %in% names(yg_results)) {
    stop("'yg_results' must be a list from yield_gap_components()")
  }

  cpa_table <- yg_results$cpa_table
  non_intercept <- cpa_table$Variable != "Intercept"

  if (sum(non_intercept) == 0) {
    return(list(pareto_table = NULL, top_n = 0, top_variables = character(0)))
  }

  pareto_data <- cpa_table[non_intercept, c("Variable", "Share_Percent_Abs", "Share_Cumulative")]
  pareto_data <- pareto_data[order(-pareto_data$Share_Percent_Abs), ]

  if (is.null(top_n)) {
    cumsum_idx <- which(pareto_data$Share_Cumulative >= 80)
    top_n <- if (length(cumsum_idx) == 0) nrow(pareto_data) else cumsum_idx[1]
  } else {
    top_n <- min(top_n, nrow(pareto_data))
  }
  top_n <- max(top_n, 0)

  if (verbose) {
    cat("\n=== Pareto Analysis (80/20 Rule) ===\n")
    cat("Top", top_n, "variables explain",
        round(pareto_data$Share_Cumulative[max(top_n, 1)], 1), "% of yield gap\n\n")

    # seq_len(0) correctly produces an empty sequence (unlike 1:0, which
    # would produce c(1, 0) and try to index a nonexistent row).
    for (i in seq_len(top_n)) {
      cat(sprintf("  %d. %-25s: %6.1f%% (Cumulative: %6.1f%%)\n",
                  i, pareto_data$Variable[i],
                  pareto_data$Share_Percent_Abs[i],
                  pareto_data$Share_Cumulative[i]))
    }
  }

  return(list(
    pareto_table = pareto_data,
    top_n = top_n,
    top_variables = if (top_n > 0) pareto_data$Variable[seq_len(top_n)] else character(0)
  ))
}

