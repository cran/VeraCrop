# ============================================================
# File: R/preprocessing/scaling.R
# VeraCrop: Variable Scaling and Back-Transformation
# ============================================================

# ============================================================
# Internal Helpers
# ============================================================

#' Create a unique variable name safely
#'
#' @param base_name Base name for the variable
#' @param existing_names Existing column names to check against
#' @return A unique name
#' @noRd
.make_unique_name <- function(base_name, existing_names) {
  if (!base_name %in% existing_names) return(base_name)
  
  counter <- 1
  repeat {
    new_name <- paste0(base_name, "_", counter)
    if (!new_name %in% existing_names) return(new_name)
    counter <- counter + 1
  }
}

#' Internal message wrapper
#' @noRd
.sc_message <- function(...) {
  message(...)
}

# ============================================================
# Main Scaling Functions
# ============================================================

#' Scale Continuous Variables
#'
#' Standardizes (or robustly scales) numeric variables in a data frame.
#'
#' @param data A data frame.
#' @param vars Character vector of variable names to scale, or NULL to
#'   auto-detect all numeric columns (excluding \code{exclude_vars}).
#'   Default: NULL.
#' @param exclude_vars Character vector of variable names to NEVER scale,
#'   even if they are numeric and would otherwise be auto-detected. This
#'   should always include the response variable - scaling the response
#'   changes its name (e.g. "Yield" -> "Yield_scaled"), which silently
#'   breaks any downstream code (model fitting, NZV/correlation filtering,
#'   reporting) that still refers to it by its original name.
#'   Default: NULL.
#' @param method Character, "standard" (mean/sd) or "robust" (median/mad).
#'   Default: "standard".
#' @param keep_original Logical, keep the original (unscaled) column
#'   alongside the new scaled column. Default: FALSE.
#' @param verbose Logical, print progress messages. Default: TRUE.
#'
#' @return A list with:
#'   \item{data}{Data frame with scaled variables}
#'   \item{scaling_params}{List of scaling parameters for back-transformation}
#'   \item{scaled_vars}{Character vector of scaled variable names}
#'   \item{method}{Scaling method used}
#'
#' @examples
#' df <- data.frame(Yield = rnorm(30, 5000, 500), N_rate = rnorm(30, 100, 20))
#' result <- scale_variables(df, exclude_vars = "Yield", method = "standard", verbose = FALSE)
#' result$scaling_params
#'
#' @export
scale_variables <- function(data,
                            vars = NULL,
                            exclude_vars = NULL,
                            method = "standard",
                            keep_original = FALSE,
                            verbose = TRUE) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  if (nrow(data) == 0) {
    stop("data has zero rows")
  }
  
  method <- match.arg(method, c("standard", "robust"))
  
  # Check if variables already appear to be scaled
  existing_names <- names(data)
  likely_scaled <- grepl("_scaled$|_scaled_robust$", existing_names)
  if (any(likely_scaled) && verbose) {
    .sc_message("  Note: Some variables already appear to be scaled (suffix _scaled)")
  }
  
  # Identify variables to scale
  if (is.null(vars)) {
    vars <- names(data)[vapply(data, is.numeric, logical(1))]
    if (!is.null(exclude_vars)) {
      excluded_present <- intersect(vars, exclude_vars)
      if (length(excluded_present) > 0 && verbose) {
        .sc_message(sprintf("  Excluding from scaling (e.g. response variable): %s",
                           paste(excluded_present, collapse = ", ")))
      }
      vars <- setdiff(vars, exclude_vars)
    }
    if (verbose && length(vars) > 0) {
      .sc_message(sprintf("  Scaling all numeric variables: %s", 
                         paste(vars, collapse = ", ")))
    }
  } else {
    unknown_vars <- setdiff(vars, names(data))
    if (length(unknown_vars) > 0) {
      warning(sprintf(
        "Ignoring unknown variables: %s",
        paste(unknown_vars, collapse = ", ")
      ), call. = FALSE)
    }
    vars <- intersect(vars, names(data))
    if (!is.null(exclude_vars)) {
      requested_but_excluded <- intersect(vars, exclude_vars)
      if (length(requested_but_excluded) > 0) {
        warning(sprintf(
          "Variable(s) explicitly requested in 'vars' are also in 'exclude_vars' and will NOT be scaled: %s",
          paste(requested_but_excluded, collapse = ", ")
        ), call. = FALSE)
      }
      vars <- setdiff(vars, exclude_vars)
    }
  }
  
  if (length(vars) == 0) {
    if (verbose) .sc_message("  No numeric variables to scale.")
    return(list(
      data = data,
      scaling_params = list(),
      scaled_vars = character(),
      method = method
    ))
  }
  
  scaling_params <- list()
  scaled_vars <- character()
  
  for (var in vars) {
    x <- data[[var]]
    
    # Check if variable has any finite values
    finite_x <- x[is.finite(x) & !is.na(x)]
    if (length(finite_x) == 0) {
      warning(sprintf("Variable '%s' has no finite values. Skipping scaling.", var), call. = FALSE)
      next
    }
    
    # Calculate center and scale
    if (method == "robust") {
      center <- median(finite_x, na.rm = TRUE)
      scale <- stats::mad(finite_x, na.rm = TRUE)
      suffix <- "_scaled_robust"
      method_name <- "robust"
    } else {
      center <- mean(finite_x, na.rm = TRUE)
      scale <- stats::sd(finite_x, na.rm = TRUE)
      suffix <- "_scaled"
      method_name <- "standard"
    }
    
    # Handle zero or invalid scale
    if (scale == 0 || !is.finite(scale)) {
      warning(sprintf(
        "Variable '%s' has zero variance (scale = %.6f). Skipping scaling.",
        var, scale
      ), call. = FALSE)
      next
    }
    
    # Create scaled variable name safely
    scaled_name <- .make_unique_name(paste0(var, suffix), existing_names)
    
    # Scale the variable
    data[[scaled_name]] <- (x - center) / scale
    
    # Remove original if requested
    if (!keep_original) {
      data[[var]] <- NULL
    }
    
    # Store scaling parameters with comprehensive metadata
    scaling_params[[var]] <- list(
      original_name = var,
      scaled_name = scaled_name,
      method = method_name,
      center = center,
      scale = scale,
      n = length(finite_x),
      class = class(x)[1],
      type = typeof(x),
      missing_count = sum(is.na(x)),
      timestamp = Sys.time()
    )
    
    scaled_vars <- c(scaled_vars, scaled_name)
    existing_names <- c(existing_names, scaled_name)
    
    if (verbose) {
      .sc_message(sprintf("  Scaled %s -> %s: center=%.3f, scale=%.3f (%s)", 
                         var, scaled_name, center, scale, method_name))
    }
  }
  
  if (verbose) {
    .sc_message(sprintf("  Scaled %d variables using %s method.", 
                       length(scaled_vars), method))
  }
  
  return(list(
    data = data,
    scaling_params = scaling_params,
    scaled_vars = scaled_vars,
    method = method
  ))
}

#' Back-transform Coefficients to Original Scale
#'
#' Converts coefficients from standardized predictors back to the original scale
#' of the predictors.
#'
#' For standardized predictors only (X scaled, Y not scaled):
#'   beta_original = beta_standardized / sd(X)
#'
#' For both X and Y scaled:
#'   beta_original = beta_standardized * (sd_Y / sd_X)
#'
#' @param coefs Named vector of standardized coefficients.
#'   Names should match the scaled variable names.
#' @param scaling_params Scaling parameters from \code{\link{scale_variables}}.
#' @param response_sd Numeric, standard deviation of the response variable.
#'   Required if the response variable was also standardized.
#' @param keep_names Logical, keep scaled names or rename to original. Default: TRUE
#'
#' @return Named vector of back-transformed coefficients
#'
#' @examples
#' df <- data.frame(Yield = rnorm(30, 5000, 500), N_rate = rnorm(30, 100, 20))
#' scaled <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)
#' model <- lm(Yield ~ N_rate_scaled, data = scaled$data)
#' back_transform_coefficients(coef(model), scaled$scaling_params)
#'
#' @export
back_transform_coefficients <- function(coefs, 
                                         scaling_params, 
                                         response_sd = NULL,
                                         keep_names = TRUE) {
  
  if (!is.list(scaling_params) || length(scaling_params) == 0) {
    stop("scaling_params must be a non-empty list")
  }
  
  if (!is.null(response_sd) && (!is.numeric(response_sd) || response_sd <= 0)) {
    stop("response_sd must be a positive numeric value")
  }
  
  # Create a copy for results
  back_transformed <- coefs
  
  for (var in names(scaling_params)) {
    info <- scaling_params[[var]]
    scaled_name <- info$scaled_name
    
    if (scaled_name %in% names(coefs)) {
      if (!is.null(info$scale) && info$scale > 0) {
        if (!is.null(response_sd)) {
          # Both X and Y were standardized
          back_transformed[scaled_name] <- coefs[scaled_name] * (response_sd / info$scale)
        } else {
          # Only X was standardized
          back_transformed[scaled_name] <- coefs[scaled_name] / info$scale
        }
      }
    }
  }
  
  # Rename to original variable names if requested
  if (!keep_names) {
    new_names <- names(back_transformed)
    for (var in names(scaling_params)) {
      info <- scaling_params[[var]]
      scaled_name <- info$scaled_name
      idx <- which(new_names == scaled_name)
      if (length(idx) > 0) {
        new_names[idx] <- info$original_name
      }
    }
    names(back_transformed) <- new_names
  }
  
  return(back_transformed)
}

#' Adjust Intercept After Back-Transformation
#'
#' When predictors are standardized, the intercept must be adjusted to make
#' predictions in the original scale.
#'
#' For X standardized only:
#'   intercept_original = intercept_std - sum(beta_original * mean_X)
#'
#' For both X and Y standardized:
#'   intercept_original = mean_Y + intercept_std * sd_Y - sum(beta_original * mean_X)
#'
#' @param intercept_std The intercept from the standardized model
#' @param back_transformed_coefs Back-transformed coefficients
#' @param scaling_params Scaling parameters from \code{\link{scale_variables}}
#' @param response_mean Numeric, mean of the response variable.
#'   Required if the response variable was also standardized.
#' @param response_sd Numeric, standard deviation of the response variable.
#'   Required if the response variable was also standardized.
#'
#' @return Adjusted intercept for the original scale model
#'
#' @examples
#' df <- data.frame(Yield = rnorm(30, 5000, 500), N_rate = rnorm(30, 100, 20))
#' scaled <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)
#' model <- lm(Yield ~ N_rate_scaled, data = scaled$data)
#' back_coefs <- back_transform_coefficients(coef(model), scaled$scaling_params)
#' adjust_intercept(coef(model)["(Intercept)"], back_coefs[-1], scaled$scaling_params)
#'
#' @export
adjust_intercept <- function(intercept_std, 
                              back_transformed_coefs, 
                              scaling_params,
                              response_mean = NULL,
                              response_sd = NULL) {
  
  if (!is.list(scaling_params) || length(scaling_params) == 0) {
    stop("scaling_params must be a non-empty list")
  }
  
  if (!is.numeric(intercept_std)) {
    stop("intercept_std must be numeric")
  }
  
  # Check response scaling consistency
  if (xor(is.null(response_mean), is.null(response_sd))) {
    stop("Both response_mean and response_sd must be provided together, or both omitted")
  }
  
  if (!is.null(response_sd) && (!is.numeric(response_sd) || response_sd <= 0)) {
    stop("response_sd must be a positive numeric value")
  }
  
  # Calculate adjustment based on predictor scaling
  adjustment <- 0
  
  for (var in names(scaling_params)) {
    info <- scaling_params[[var]]
    scaled_name <- info$scaled_name
    
    if (scaled_name %in% names(back_transformed_coefs)) {
      if (!is.null(info$center) && !is.null(info$scale) && info$scale > 0) {
        adjustment <- adjustment + (back_transformed_coefs[scaled_name] * info$center)
      }
    }
  }
  
  # Apply adjustment
  if (!is.null(response_mean) && !is.null(response_sd)) {
    # Both X and Y were standardized
    adjusted_intercept <- response_mean + (intercept_std * response_sd) - adjustment
  } else {
    # Only X was standardized
    adjusted_intercept <- intercept_std - adjustment
  }
  
  return(adjusted_intercept)
}

#' Get Scaling Summary
#'
#' Provides a human-readable summary of scaling parameters.
#'
#' @param scaling_params Scaling parameters from \code{\link{scale_variables}}
#' @param verbose Logical, print summary. Default: TRUE
#'
#' @return A data frame with scaling parameters (invisibly if verbose=TRUE)
#'
#' @examples
#' df <- data.frame(Yield = rnorm(30, 5000, 500), N_rate = rnorm(30, 100, 20))
#' scaled <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)
#' summary_scaling(scaled$scaling_params, verbose = FALSE)
#'
#' @export
summary_scaling <- function(scaling_params, verbose = TRUE) {
  
  if (!is.list(scaling_params) || length(scaling_params) == 0) {
    if (verbose) cat("No scaling parameters available.\n")
    return(invisible(NULL))
  }
  
  summary_df <- data.frame(
    Variable = names(scaling_params),
    Scaled_Name = sapply(scaling_params, function(x) x$scaled_name %||% NA),
    Center = sapply(scaling_params, function(x) x$center %||% NA),
    Scale = sapply(scaling_params, function(x) x$scale %||% NA),
    Method = sapply(scaling_params, function(x) x$method %||% NA),
    N = sapply(scaling_params, function(x) x$n %||% NA),
    Class = sapply(scaling_params, function(x) x$class %||% NA),
    Missing = sapply(scaling_params, function(x) x$missing_count %||% 0),
    stringsAsFactors = FALSE
  )
  
  if (verbose) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("SCALING PARAMETERS SUMMARY\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
    cat(sprintf("Total variables scaled: %d\n", nrow(summary_df)))
    cat("\n")
    print(summary_df, row.names = FALSE)
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  }
  
  return(invisible(summary_df))
}

#' Back-transform Data
#'
#' Transforms scaled data back to the original scale using stored parameters.
#'
#' @param data A data frame containing scaled variables
#' @param scaling_params Scaling parameters from \code{\link{scale_variables}}
#' @param remove_scaled Logical, remove scaled variables after back-transformation.
#'   Default: FALSE
#'
#' @return Data frame with variables back-transformed to original scale
#'
#' @examples
#' df <- data.frame(Yield = rnorm(30, 5000, 500), N_rate = rnorm(30, 100, 20))
#' scaled <- scale_variables(df, exclude_vars = "Yield", verbose = FALSE)
#' restored <- back_transform_data(scaled$data, scaled$scaling_params)
#' head(restored$N_rate)
#'
#' @export
back_transform_data <- function(data, scaling_params, remove_scaled = FALSE) {
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  if (!is.list(scaling_params) || length(scaling_params) == 0) {
    return(data)
  }
  
  data_back <- data
  existing_names <- names(data_back)
  
  for (var in names(scaling_params)) {
    info <- scaling_params[[var]]
    scaled_name <- info$scaled_name
    
    if (!scaled_name %in% names(data_back)) {
      warning(sprintf("Scaled variable '%s' not found in data. Skipping.", scaled_name), call. = FALSE)
      next
    }
    
    # Check if original variable already exists
    if (var %in% names(data_back)) {
      warning(sprintf(
        "Original variable '%s' already exists. Using safe name.",
        var
      ), call. = FALSE)
      var_name <- .make_unique_name(var, existing_names)
    } else {
      var_name <- var
    }
    
    # Back-transform: x = z * scale + center
    x_scaled <- data_back[[scaled_name]]
    data_back[[var_name]] <- x_scaled * info$scale + info$center
    
    if (remove_scaled) {
      data_back[[scaled_name]] <- NULL
    }
    
    existing_names <- c(existing_names, var_name)
  }
  
  return(data_back)
}

#' Scale New Data Using Existing Parameters
#'
#' Applies the same scaling parameters to new data (e.g., test set).
#'
#' @param data A data frame with variables to scale
#' @param scaling_params Scaling parameters from \code{\link{scale_variables}}
#' @param keep_original Logical, keep original variables in the data frame. Default: FALSE
#' @param verbose Logical, print progress messages. Default: TRUE
#'
#' @return Data frame with scaled variables
#'
#' @examples
#' train <- data.frame(Yield = rnorm(30, 5000, 500), N_rate = rnorm(30, 100, 20))
#' scaled <- scale_variables(train, exclude_vars = "Yield", verbose = FALSE)
#' new_obs <- data.frame(N_rate = c(90, 110, 130))
#' scale_new_data(new_obs, scaled$scaling_params, verbose = FALSE)
#'
#' @export
scale_new_data <- function(data, 
                            scaling_params, 
                            keep_original = FALSE,
                            verbose = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  if (!is.list(scaling_params) || length(scaling_params) == 0) {
    return(data)
  }
  
  data_scaled <- data
  existing_names <- names(data_scaled)
  
  for (var in names(scaling_params)) {
    if (!var %in% names(data)) {
      warning(sprintf("Variable '%s' not found in new data. Skipping.", var), call. = FALSE)
      next
    }
    
    info <- scaling_params[[var]]
    scaled_name <- info$scaled_name
    
    if (is.null(info$center) || is.null(info$scale) || info$scale == 0) {
      warning(sprintf("Invalid scaling parameters for '%s'. Skipping.", var), call. = FALSE)
      next
    }
    
    # Create scaled variable name safely if needed
    if (scaled_name %in% existing_names) {
      scaled_name <- .make_unique_name(scaled_name, existing_names)
    }
    
    # Apply existing scaling
    data_scaled[[scaled_name]] <- (data[[var]] - info$center) / info$scale
    
    # Remove original if requested
    if (!keep_original) {
      data_scaled[[var]] <- NULL
    }
    
    existing_names <- c(existing_names, scaled_name)
    
    if (verbose) {
      .sc_message(sprintf("  Scaled %s -> %s using existing parameters", var, scaled_name))
    }
  }
  
  return(data_scaled)
}
