# ============================================================
# File: R/preprocessing/missing.R
# VeraCrop: Missing Value Handling
# ============================================================

# ============================================================
# Constants and Package Checks
# ============================================================

# Threshold for using direct non-NA extraction vs. sampling
.SMALL_DATA_THRESHOLD <- 50000

# Threshold for reservoir sampling - reduced to 1M for better memory efficiency
.RESERVOIR_THRESHOLD <- 1000000

# Mode heuristic threshold multiplier
# User can override via options(veracrop.mode_threshold = value)
.MODE_THRESHOLD_MULTIPLIER <- getOption("veracrop.mode_threshold", 1.0)

# Check if bit64 is available (once at package load)
.has_bit64 <- requireNamespace("bit64", quietly = TRUE)

# Package-level warning flag for integer64 (warn only once)
.warned_integer64 <- FALSE

# ============================================================
# Internal Helpers - Type Detection and Utilities
# ============================================================

#' Check if a value is scalar and NA (safe for any class)
#'
#' @param x Any R object
#' @return Logical, TRUE if x is a scalar NA
#' @noRd
is_scalar_na <- function(x) {
  length(x) == 1L && is.na(x)
}

#' Check if an object is a date/time type
#'
#' @param x An R object
#' @return Logical, TRUE if x is Date, POSIXct, or POSIXlt
#' @noRd
is_datetime <- function(x) {
  inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")
}

#' Check if an object is a date (not POSIX)
#'
#' @param x An R object
#' @return Logical, TRUE if x is Date
#' @noRd
is_date_only <- function(x) {
  inherits(x, "Date")
}

#' Check if an object is POSIXct or POSIXlt
#'
#' @param x An R object
#' @return Logical, TRUE if x is POSIXct or POSIXlt
#' @noRd
is_posix <- function(x) {
  inherits(x, "POSIXct") || inherits(x, "POSIXlt")
}

#' Check if an object is difftime
#'
#' @param x An R object
#' @return Logical, TRUE if x is difftime
#' @noRd
is_difftime <- function(x) {
  inherits(x, "difftime")
}

#' Check if an object has units attribute
#'
#' @param x An R object
#' @return Logical, TRUE if x has units
#' @noRd
has_units <- function(x) {
  !is.null(attr(x, "units"))
}

#' Internal message wrapper for consistent output handling
#'
#' @param ... Messages to print
#' @noRd
vc_message <- function(...) {
  message(...)
}

#' Count missing values in a data frame (memory efficient)
#'
#' Uses vapply with USE.NAMES = FALSE to avoid creating names.
#'
#' @param data A data frame
#' @return Integer, total number of missing values
#' @noRd
count_missing <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) return(0L)
  sum(vapply(data, function(x) sum(is.na(x)), integer(1), USE.NAMES = FALSE))
}

# ============================================================
# Internal Helpers - Mode Selection
# ============================================================

#' Select mode index based on ties method
#'
#' @param indices Integer vector of candidate mode indices
#' @param ties Character, how to handle ties: "first", "random", or "na"
#' @return Selected index or NA_integer_
#' @noRd
select_mode_index <- function(indices, ties) {
  n <- length(indices)
  if (n == 0L) return(NA_integer_)
  if (n == 1L) return(indices[1L])
  
  switch(
    ties,
    first = indices[1L],
    random = indices[sample.int(n, 1L)],
    na = NA_integer_
  )
}

# ============================================================
# Internal Helpers - Mode and Utilities
# ============================================================

#' Create factor with preserved class
#'
#' @noRd
make_factor <- function(level_name, levels_vec, ordered_flag) {
  if (ordered_flag) {
    ordered(level_name, levels = levels_vec)
  } else {
    factor(level_name, levels = levels_vec)
  }
}

#' Get mode (most frequent value) for a vector
#'
#' Returns the mode while preserving the original data type.
#' Uses integer-based tabulation for speed and memory efficiency.
#' For ties, respects the order of first occurrence when ties = "first".
#'
#' For character vectors, uses unique() + match() to preserve first occurrence order.
#'
#' Note: This function assumes discrete/categorical data.
#' It should not be called directly for continuous numeric data.
#'
#' @param x A vector
#' @param ties Character, how to handle ties: "first" (first encountered),
#'   "random" (randomly select one, stochastic; use set.seed() for reproducibility),
#'   or "na" (return NA)
#' @return The most frequent value with original type preserved, or NA
#' @noRd
get_mode <- function(x, ties = "first") {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) {
    return(x[NA_integer_])
  }
  
  # For factors, use integer-based tabulation for speed
  if (is.factor(x_clean)) {
    int_codes <- as.integer(x_clean)
    tab <- tabulate(int_codes, nbins = length(levels(x_clean)))
    
    max_count <- max(tab)
    if (max_count == 1) {
      return(x_clean[NA_integer_])
    }
    
    mode_indices <- which(tab == max_count)
    levels_vec <- levels(x_clean)
    ordered_flag <- is.ordered(x_clean)
    
    selected <- select_mode_index(mode_indices, ties)
    if (is.na(selected)) {
      return(x_clean[NA_integer_])
    }
    return(make_factor(levels_vec[selected], levels_vec, ordered_flag))
  }
  
  # For character: use unique() + match() to preserve first occurrence order
  # This is consistent with ties = "first" behavior
  ux <- unique(x_clean)
  tab <- tabulate(match(x_clean, ux))
  
  max_count <- max(tab)
  if (max_count == 1) {
    return(x_clean[NA_integer_])
  }
  
  mode_indices <- which(tab == max_count)
  selected <- select_mode_index(mode_indices, ties)
  if (is.na(selected)) {
    return(x_clean[NA_integer_])
  }
  return(ux[selected])
}

#' Get first non-NA value from a vector
#'
#' @param x A vector
#' @return The first non-NA value, preserving original class, or NA if all values are NA
#' @noRd
first_non_na <- function(x) {
  idx <- match(FALSE, is.na(x))
  if (is.na(idx)) return(x[NA_integer_])
  x[idx]
}

#' Format value for display
#'
#' Uses format() with user's global digits option for maximum flexibility.
#'
#' @param x A value
#' @return Formatted character string
#' @noRd
format_display <- function(x) {
  if (is.null(x) || length(x) != 1L || is_scalar_na(x)) return("NA")
  
  # Handle infinite values
  if (is.numeric(x) && !is.finite(x)) {
    return(if (x > 0) "Inf" else "-Inf")
  }
  
  if (is_date_only(x)) {
    return(as.character(x))
  }
  if (is_posix(x)) {
    return(format(x, usetz = TRUE))
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.numeric(x)) {
    digits <- getOption("digits", 7)
    formatted <- format(x, digits = digits, scientific = FALSE, trim = TRUE)
    return(trimws(formatted))
  }
  as.character(x)
}

#' Create a missing indicator name safely
#'
#' @param var Original variable name
#' @param suffix Suffix to append
#' @param existing_names Existing column names to check against
#' @return A unique indicator name
#' @noRd
safe_indicator_name <- function(var, suffix, existing_names) {
  if (is.null(suffix) || suffix == "") {
    suffix <- "_missing"
  }
  
  base_name <- paste0(var, suffix)
  if (!base_name %in% existing_names) return(base_name)
  
  counter <- 1
  repeat {
    new_name <- paste0(base_name, "_", counter)
    if (!new_name %in% existing_names) return(new_name)
    counter <- counter + 1
  }
}

#' Fast reservoir sampling
#'
#' @param x Vector to sample from
#' @param sample_size Number of samples
#' @return Sampled vector
#' @noRd
reservoir_sample <- function(x, sample_size) {
  n <- length(x)
  
  # Pre-allocate reservoir efficiently
  # Use seq_len to create a template that preserves class
  reservoir <- x[seq_len(sample_size)]
  # Fill with NA while preserving class
  reservoir[] <- x[NA_integer_]
  
  reservoir_idx <- 0L
  total_seen <- 0L
  
  # Optimized loop with local variables
  res <- reservoir
  idx <- reservoir_idx
  seen <- total_seen
  k <- sample_size
  
  for (i in seq_len(n)) {
    val <- x[i]
    if (!is.na(val)) {
      seen <- seen + 1L
      if (idx < k) {
        idx <- idx + 1L
        res[idx] <- val
      } else {
        j <- sample.int(seen, 1L)
        if (j <= k) {
          res[j] <- val
        }
      }
    }
  }
  
  if (idx < sample_size) {
    return(res[seq_len(idx)])
  }
  res
}

#' Sample non-NA values efficiently
#'
#' Uses three-stage approach with minimal scans:
#' 1. If data is small, get all non-NA values directly
#' 2. If data is medium, sample indices efficiently (single scan)
#' 3. If data is very large, use reservoir sampling (single scan, no index vector)
#'
#' The reservoir threshold is set at 1M for optimal memory/performance balance.
#'
#' Important: Preserves class and attributes of x.
#'
#' @param x Vector to sample from
#' @param sample_size Maximum number of values to sample
#' @return A sampled vector with class preserved
#' @noRd
sample_non_na <- function(x, sample_size = 1000) {
  n <- length(x)
  
  if (n <= .SMALL_DATA_THRESHOLD) {
    return(x[!is.na(x)])
  }
  
  if (n > .RESERVOIR_THRESHOLD) {
    return(reservoir_sample(x, sample_size))
  }
  
  idx <- which(!is.na(x))
  n_non_na <- length(idx)
  
  if (n_non_na == 0) return(x[0])
  if (n_non_na <= sample_size) return(x[idx])
  
  sampled_idx <- sample.int(n_non_na, sample_size)
  x[idx[sampled_idx]]
}

# ============================================================
# Internal Helpers - Imputation Result Builder
# ============================================================

#' Create a standardized imputation result
#'
#' @param value Imputed value
#' @param method Method used
#' @param x Original vector (for class detection)
#' @return A list with value, method, and class
#' @noRd
make_imputation_result <- function(value, method, x) {
  list(
    value = value,
    display_value = format_display(value),
    method = method,
    class = class(x)
  )
}

# ============================================================
# Internal Helpers - Strategy Map for Numeric Methods
# ============================================================

#' Strategy map for numeric imputation methods
#'
#' Using a strategy map instead of if/else chains makes it easy to
#' add new methods like "trimmed_mean" or "knn" in the future.
#'
#' @noRd
numeric_strategies <- list(
  mean = function(x) {
    if (length(x) == 0) return(NA_real_)
    mean(x, na.rm = TRUE)
  },
  median = function(x) {
    if (length(x) == 0) return(NA_real_)
    median(x, na.rm = TRUE)
  },
  mode = function(x, ties_method) {
    # Mode is handled separately via get_mode
    NULL  # Placeholder, handled in impute_numeric
  }
)

# ============================================================
# Internal Helpers - Core Imputation Functions
# ============================================================

#' Check if mode imputation is appropriate for numeric data
#'
#' Uses a heuristic based on unique value count. The threshold can be
#' adjusted via options("veracrop.mode_threshold").
#'
#' @param x Numeric vector to check
#' @param var Variable name for warnings
#' @param verbose Print warning message
#' @return Logical, TRUE if mode is appropriate
#' @noRd
is_mode_appropriate <- function(x, var, verbose) {
  sampled <- sample_non_na(x, 1000)
  if (length(sampled) == 0) return(TRUE)
  
  # Use unique.default for slightly better performance
  n_unique <- length(unique.default(sampled))
  base_threshold <- max(5, sqrt(length(sampled)))
  threshold <- base_threshold * .MODE_THRESHOLD_MULTIPLIER
  
  if (n_unique > threshold) {
    if (verbose) {
      vc_message(sprintf(
        "  [NOTE] Variable '%s' has %d unique values (sampled).",
        var, n_unique
      ))
      vc_message(sprintf(
        "  [NOTE] Mode imputation may not be appropriate. Consider using 'mean' or 'median'.")
      )
    }
    return(FALSE)
  }
  return(TRUE)
}

#' Impute discrete variables (factor, character, logical)
#'
#' Unified imputation for discrete/categorical variables.
#' Handles all discrete types with consistent logic.
#'
#' @noRd
impute_discrete <- function(x, var, n_missing, method, ties_method, verbose) {
  actual_method <- method
  
  if (is.factor(x) && length(levels(x)) == 0) {
    warning(sprintf("Factor variable \"%s\" has no levels. Cannot impute.", var), call. = FALSE)
    return(NULL)
  }
  
  impute_val <- get_mode(x, ties = ties_method)
  
  if (is_scalar_na(impute_val)) {
    actual_method <- "first_observed"
    impute_val <- first_non_na(x)
    
    if (is_scalar_na(impute_val)) {
      actual_method <- "none"
      impute_val <- x[NA_integer_]
      if (verbose) {
        vc_message(sprintf("  %s: %d missing values could not be imputed (all NA)", var, n_missing))
      }
      return(make_imputation_result(impute_val, "none", x))
    }
  }
  
  if (verbose) {
    vc_message(sprintf("  Imputed %s: %d values with \"%s\" (%s)", 
                       var, n_missing, format_display(impute_val), actual_method))
  }
  
  return(make_imputation_result(impute_val, actual_method, x))
}

#' Impute numeric variables (including Date, POSIXct, difftime)
#'
#' Note: integer64 (bit64 package) values are converted to double during imputation.
#' This may result in precision loss for values > 2^53.
#'
#' @noRd
impute_numeric <- function(x, var, n_missing, method, ties_method, verbose) {
  actual_method <- method
  
  # Check mode appropriateness if method is "mode"
  if (method == "mode") {
    is_mode_appropriate(x, var, verbose)
  }
  
  is_date <- is_date_only(x)
  is_posix_val <- is_posix(x)
  is_difftime_val <- is_difftime(x)
  has_units_val <- has_units(x)
  
  # Store units attribute for difftime
  units_attr <- if (is_difftime_val) attr(x, "units") else NULL
  
  # Convert to numeric for calculation
  if (is_posix_val) {
    x_posixct <- as.POSIXct(x)
    x_num <- as.numeric(x_posixct)
  } else if (is_date) {
    x_num <- as.numeric(x)
  } else if (is_difftime_val) {
    x_num <- as.numeric(x)
  } else {
    x_num <- x
  }
  
  x_num_double <- as.double(x_num)
  finite_idx <- !is.na(x_num_double) & is.finite(x_num_double)
  finite_x <- x_num[finite_idx]
  
  # Use strategy map for method dispatch
  if (method == "mode") {
    impute_val <- get_mode(x, ties = ties_method)
    if (is_scalar_na(impute_val)) {
      warning(sprintf("No mode exists for variable \"%s\"; using median of finite values.", var), call. = FALSE)
      actual_method <- "median"
      impute_val <- numeric_strategies$median(finite_x)
    }
  } else if (length(finite_x) == 0L) {
    warning(sprintf("Variable \"%s\" contains no finite values. Using NA as imputation.", var), call. = FALSE)
    impute_val <- x[NA_integer_]
    actual_method <- "none"
  } else {
    impute_val <- numeric_strategies[[method]](finite_x)
  }
  
  if (is_date && !is_scalar_na(impute_val)) {
    impute_val <- round(impute_val)
  }
  
  # Convert back to original type
  if (is_date && !is_scalar_na(impute_val)) {
    impute_val <- as.Date(impute_val, origin = "1970-01-01")
  } else if (is_posix_val && !is_scalar_na(impute_val)) {
    tz <- attr(x, "tzone")
    if (length(tz) == 0) tz <- ""
    impute_val <- as.POSIXct(impute_val, origin = "1970-01-01", tz = tz)
  } else if (is_difftime_val && !is_scalar_na(impute_val)) {
    impute_val <- as.difftime(impute_val, units = units_attr %||% "secs")
  }
  
  # Handle integer64 - use package-level warning flag
  if (inherits(x, "integer64") && !is_scalar_na(impute_val)) {
    if (.has_bit64) {
      impute_val <- bit64::as.integer64(impute_val)
    } else {
      # Use package-level flag to warn only once
      if (!.warned_integer64) {
        warning(
          "Variable \"", var, "\" is integer64 but bit64 package not installed. ",
          "Converting to double (may lose precision for values > 2^53).",
          call. = FALSE
        )
        # Assign to package environment
        assign(".warned_integer64", TRUE, envir = parent.env(environment()))
      }
      impute_val <- as.double(impute_val)
    }
  }
  
  # Preserve units attribute if present
  if (has_units_val && !is_scalar_na(impute_val) && !inherits(impute_val, "difftime")) {
    attr(impute_val, "units") <- units_attr
  }
  
  if (verbose && !is_scalar_na(impute_val)) {
    vc_message(sprintf("  Imputed %s: %d values with %s (%s)", 
                       var, n_missing, format_display(impute_val), actual_method))
  } else if (verbose && is_scalar_na(impute_val)) {
    vc_message(sprintf("  %s: %d missing values could not be imputed", var, n_missing))
  }
  
  return(make_imputation_result(impute_val, actual_method, x))
}

# ============================================================
# Internal Helpers - Imputation Dispatch
# ============================================================

#' Dispatch imputation to appropriate handler
#'
#' Uses predicate-based dispatch with optimal ordering:
#' 1. Date (most specific)
#' 2. POSIX
#' 3. difftime
#' 4. Factor
#' 5. Character
#' 6. Logical
#' 7. Numeric (most general)
#'
#' @param x Vector to impute
#' @param var Variable name
#' @param n_missing Number of missing values
#' @param method_map List of methods for each type
#' @param ties_method How to handle ties
#' @param verbose Print progress
#' @return Imputation result
#' @noRd
dispatch_imputation <- function(x, var, n_missing, method_map, ties_method, verbose) {
  
  # Date/time first (most specific)
  if (is_date_only(x)) {
    return(impute_numeric(x, var, n_missing, method_map$numeric, ties_method, verbose))
  }
  
  if (is_posix(x)) {
    return(impute_numeric(x, var, n_missing, method_map$numeric, ties_method, verbose))
  }
  
  # difftime with units
  if (is_difftime(x)) {
    return(impute_numeric(x, var, n_missing, method_map$numeric, ties_method, verbose))
  }
  
  # Factor
  if (is.factor(x)) {
    return(impute_discrete(x, var, n_missing, method_map$categorical, ties_method, verbose))
  }
  
  # Character
  if (is.character(x)) {
    return(impute_discrete(x, var, n_missing, method_map$categorical, ties_method, verbose))
  }
  
  # Logical
  if (is.logical(x)) {
    return(impute_discrete(x, var, n_missing, method_map$logical, ties_method, verbose))
  }
  
  # Numeric (covers both integer and double)
  if (is.numeric(x)) {
    return(impute_numeric(x, var, n_missing, method_map$numeric, ties_method, verbose))
  }
  
  # Unsupported - return simple structure
  return(list(ok = FALSE, class = class(x)[1], var = var))
}

# ============================================================
# Internal Helpers - Summary Builder
# ============================================================

#' Build summary text for printing
#'
#' Uses a list-based approach for efficient string collection.
#'
#' @param summary List with missing value statistics
#' @param n_imputed_total Total number of imputed values
#' @return Character vector with summary lines
#' @noRd
build_summary_text <- function(summary, n_imputed_total) {
  # Use list for dynamic growth without reallocation overhead
  lines <- list()
  idx <- 1L
  
  lines[[idx]] <- ""; idx <- idx + 1L
  lines[[idx]] <- paste(rep("=", 50), collapse = ""); idx <- idx + 1L
  lines[[idx]] <- "MISSING VALUE HANDLING SUMMARY"; idx <- idx + 1L
  lines[[idx]] <- paste(rep("=", 50), collapse = ""); idx <- idx + 1L
  
  if (summary$n_removed_rows > 0) {
    lines[[idx]] <- sprintf("  Removed rows with missing response: %d", summary$n_removed_rows)
    idx <- idx + 1L
  }
  
  if (length(summary$imputed_vars) > 0) {
    lines[[idx]] <- sprintf("  Imputed variables: %d", length(summary$imputed_vars))
    idx <- idx + 1L
    lines[[idx]] <- sprintf("  Total imputed values: %d", n_imputed_total)
    idx <- idx + 1L
    if (!is.null(summary$pct_imputed)) {
      lines[[idx]] <- sprintf("  Percent of original cells imputed: %.2f%%", summary$pct_imputed)
      idx <- idx + 1L
    }
    for (var in summary$imputed_vars) {
      info <- summary$imputation_info[[var]]
      class_str <- paste(info$class, collapse = ", ")
      lines[[idx]] <- sprintf("    %s: %d values imputed with %s (%s)", 
                              var, info$n_imputed, info$method, class_str)
      idx <- idx + 1L
    }
  }
  
  if (length(summary$all_na_vars) > 0) {
    lines[[idx]] <- sprintf("  All-NA variables (could not impute): %s", 
                            paste(summary$all_na_vars, collapse = ", "))
    idx <- idx + 1L
  }
  
  if (length(summary$unsupported_vars) > 0) {
    lines[[idx]] <- sprintf("  Unsupported variables (skipped): %s", 
                            paste(summary$unsupported_vars, collapse = ", "))
    idx <- idx + 1L
  }
  
  lines[[idx]] <- sprintf("\n  Initial missing values: %d", summary$initial_missing)
  idx <- idx + 1L
  lines[[idx]] <- sprintf("  Missing after response removal: %d", summary$missing_after_response_removal)
  idx <- idx + 1L
  lines[[idx]] <- sprintf("  Remaining missing values: %d", summary$remaining_missing)
  idx <- idx + 1L
  lines[[idx]] <- paste(rep("=", 50), collapse = "")
  
  # Convert list to character vector
  unlist(lines)
}

# ============================================================
# Main Functions
# ============================================================

#' Add Missing Indicators
#'
#' Creates binary indicator variables for missing values
#'
#' @param data A data frame
#' @param vars Character vector of variable names (default: auto-detect all)
#' @param suffix Character, suffix for indicator variables (default: "_missing")
#' @param as_numeric Logical, return indicators as numeric (1/0) or logical (TRUE/FALSE)
#' @param verbose Logical, print progress (default: TRUE)
#'
#' @return Data frame with missing indicators added
#'
#' @examples
#' df <- data.frame(Yield = rnorm(20, 5000, 500),
#'                   Nitrogen = c(rnorm(17, 120, 20), NA, NA, NA))
#' result <- add_missing_indicators(df, verbose = FALSE)
#' names(result)
#'
#' @export
add_missing_indicators <- function(data, 
                                   vars = NULL, 
                                   suffix = "_missing",
                                   as_numeric = FALSE,
                                   verbose = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  if (nrow(data) == 0) {
    warning("Data has zero rows. Returning unchanged.", call. = FALSE)
    return(data)
  }
  
  if (is.null(suffix) || suffix == "") {
    suffix <- "_missing"
  }
  
  if (is.null(vars)) {
    vars <- names(data)[vapply(data, function(x) any(is.na(x)), logical(1))]
  } else {
    unknown_vars <- setdiff(vars, names(data))
    if (length(unknown_vars) > 0) {
      warning(sprintf(
        "Ignoring unknown variables: %s",
        paste(unknown_vars, collapse = ", ")
      ), call. = FALSE)
    }
    vars <- intersect(vars, names(data))
  }
  
  if (length(vars) == 0) {
    if (verbose) vc_message("  No variables with missing values found")
    return(data)
  }
  
  existing_names <- names(data)
  
  for (var in vars) {
    indicator_name <- safe_indicator_name(var, suffix, existing_names)
    if (as_numeric) {
      data[[indicator_name]] <- as.numeric(is.na(data[[var]]))
    } else {
      data[[indicator_name]] <- is.na(data[[var]])
    }
    existing_names <- c(existing_names, indicator_name)
    if (verbose) {
      vc_message(sprintf("  Added missing indicator: %s", indicator_name))
    }
  }
  
  return(data)
}

#' Handle Missing Values
#'
#' Imputes missing values using specified methods. Different methods can be
#' used for different data types.
#'
#' Percent of imputed cells is calculated relative to the original variables
#' before indicator creation (when add_indicators = TRUE).
#'
#' Note: integer64 (bit64 package) values: if bit64 is installed, values are
#' preserved as integer64. Otherwise, they are converted to double (may lose
#' precision for values > 2^53).
#'
#' Mode appropriateness can be adjusted via options("veracrop.mode_threshold").
#' Default is 1.0. Increase to reduce warnings, decrease to be more strict.
#'
#' @param data A data frame
#' @param response_var Name of response variable (rows with missing response are removed)
#' @param numeric_method Imputation method for numeric variables: "mean", "median", or "mode" (default: "median")
#' @param categorical_method Imputation method for categorical variables: "mode" (default: "mode")
#' @param logical_method Imputation method for logical variables: "mode" (default: "mode")
#' @param missing_threshold Numeric, threshold for warning about high missing percentage (>= threshold triggers warning, default: 0.20)
#' @param ties_method Character, how to handle ties in mode: "first", "random", or "na" (default: "first")
#'   When using "random", call set.seed() before the function for reproducibility.
#' @param add_indicators Logical, add missing indicators before imputation (default: FALSE)
#' @param indicator_suffix Character, suffix for indicator variables (default: "_missing")
#' @param verbose Logical, print progress (default: TRUE)
#'
#' @return A list with:
#'   \item{data}{Imputed data frame}
#'   \item{summary}{Summary statistics of imputation}
#'   \item{imputed_vars}{Character vector of imputed variables}
#'   \item{imputation_info}{Detailed information about each imputation}
#'   \item{initial_missing}{Initial number of missing values (before adding indicators)}
#'   \item{missing_after_response_removal}{Missing values after response removal}
#'   \item{remaining_missing}{Number of remaining missing values}
#'   \item{n_imputed_total}{Total number of imputed values}
#'   \item{pct_imputed}{Percentage of original cells imputed}
#'   \item{n_removed_rows}{Number of rows removed due to missing response}
#'   \item{all_na_vars}{Character vector of all-NA variables that could not be imputed}
#'   \item{unsupported_vars}{Character vector of variables with unsupported classes}
#' @examples
#' df <- data.frame(
#'   Yield = rnorm(30, 5000, 500),
#'   Nitrogen = c(rnorm(27, 120, 20), NA, NA, NA)
#' )
#' result <- handle_missing_values(df, response_var = "Yield", verbose = FALSE)
#' sum(is.na(result$data))
#'
#' @export
handle_missing_values <- function(data, 
                                  response_var = "Yield",
                                  numeric_method = "median",
                                  categorical_method = "mode",
                                  logical_method = "mode",
                                  missing_threshold = 0.20,
                                  ties_method = "first",
                                  add_indicators = FALSE,
                                  indicator_suffix = "_missing",
                                  verbose = TRUE) {
  
  if (!is.data.frame(data)) stop("'data' must be a data frame")
  
  if (nrow(data) == 0) {
    stop("data has zero rows")
  }
  
  if (!response_var %in% names(data)) {
    stop(sprintf("Response variable \"%s\" not found in data", response_var))
  }
  
  numeric_method <- match.arg(numeric_method, c("mean", "median", "mode"))
  categorical_method <- match.arg(categorical_method, c("mode"))
  logical_method <- match.arg(logical_method, c("mode"))
  ties_method <- match.arg(ties_method, c("first", "random", "na"))
  
  if (!is.numeric(missing_threshold) || length(missing_threshold) != 1 ||
      missing_threshold < 0 || missing_threshold > 1) {
    stop("missing_threshold must be a single numeric value between 0 and 1")
  }
  
  if (is.null(indicator_suffix) || indicator_suffix == "") {
    indicator_suffix <- "_missing"
  }
  
  n_cols_original <- ncol(data)
  initial_missing <- count_missing(data)
  
  if (add_indicators) {
    if (verbose) vc_message("\n--- Adding missing indicators ---")
    data <- add_missing_indicators(
      data = data,
      suffix = indicator_suffix,
      as_numeric = TRUE,
      verbose = verbose
    )
  }
  
  n_removed_rows <- 0
  if (any(is.na(data[[response_var]]))) {
    n_removed_rows <- sum(is.na(data[[response_var]]))
    data <- data[!is.na(data[[response_var]]), ]
    if (verbose) {
      vc_message(sprintf("  Removed %d rows with missing response variable", n_removed_rows))
    }
  }
  
  missing_after_response_removal <- count_missing(data)
  
  missing_count <- vapply(data, function(x) sum(is.na(x)), integer(1), USE.NAMES = FALSE)
  names(missing_count) <- names(data)
  missing_vars <- names(missing_count[missing_count > 0])
  
  if (length(missing_vars) == 0) {
    if (verbose) vc_message("No missing values found.")
    summary <- list(
      initial_missing = initial_missing,
      missing_after_response_removal = missing_after_response_removal,
      remaining_missing = 0,
      n_removed_rows = n_removed_rows,
      imputed_vars = character(),
      imputation_info = list(),
      all_na_vars = character(),
      unsupported_vars = character(),
      pct_imputed = 0
    )
    return(list(
      data = data,
      summary = summary,
      imputed_vars = character(),
      imputation_info = list(),
      initial_missing = initial_missing,
      missing_after_response_removal = missing_after_response_removal,
      remaining_missing = 0,
      n_imputed_total = 0,
      pct_imputed = 0,
      n_removed_rows = n_removed_rows,
      all_na_vars = character(),
      unsupported_vars = character()
    ))
  }
  
  n_rows <- nrow(data)
  high_missing_vars <- names(missing_count)[missing_count / n_rows >= missing_threshold]
  
  if (length(high_missing_vars) > 0) {
    warning(sprintf(
      "Variables with high missing percentage (>= %.0f%%): %s. Imputation may introduce bias.",
      missing_threshold * 100,
      paste(high_missing_vars, collapse = ", ")
    ), call. = FALSE)
  }
  
  if (verbose) {
    vc_message("\n", paste(rep("=", 50), collapse = ""))
    vc_message("HANDLING MISSING VALUES")
    vc_message(paste(rep("=", 50), collapse = ""))
    vc_message(sprintf("  Numeric method: %s", numeric_method))
    vc_message(sprintf("  Categorical method: %s", categorical_method))
    vc_message(sprintf("  Ties method: %s", ties_method))
    if (numeric_method == "mode") {
      vc_message(sprintf("  Mode threshold multiplier: %.2f", .MODE_THRESHOLD_MULTIPLIER))
    }
    vc_message("  Variables with missing values:")
    for (var in missing_vars) {
      pct <- 100 * missing_count[var] / n_rows
      vc_message(sprintf("    %s: %d missing (%.1f%%)%s", 
                         var, missing_count[var], pct,
                         if (var %in% high_missing_vars) " [WARNING: high]" else ""))
    }
  }
  
  data_imputed <- data
  imputation_info <- list()
  imputed_vars <- character()
  all_na_vars <- character()
  unsupported_vars <- character()
  n_imputed_total <- 0
  total_cells_after_removal <- n_rows * n_cols_original
  
  method_map <- list(
    numeric = numeric_method,
    categorical = categorical_method,
    logical = logical_method
  )
  
  missing_indices <- lapply(data_imputed[, missing_vars, drop = FALSE], is.na)
  
  for (var in missing_vars) {
    missing_idx <- missing_indices[[var]]
    n_missing <- sum(missing_idx)
    
    if (n_missing == 0) next    
    x <- data_imputed[[var]]
    
    # Use any(!is.na(x)) for early exit on large vectors
    if (!any(!is.na(x))) {
      all_na_vars <- c(all_na_vars, var)
      warning(sprintf("Variable \"%s\" is all NA. Cannot impute.", var), call. = FALSE)
      next
    }
    
    impute_result <- dispatch_imputation(
      x = x,
      var = var,
      n_missing = n_missing,
      method_map = method_map,
      ties_method = ties_method,
      verbose = verbose
    )
    
    if (is.list(impute_result) && !is.null(impute_result$ok) && !impute_result$ok) {
      warning(sprintf(
        "Variable '%s' has unsupported class '%s'. Skipping imputation.",
        impute_result$var, impute_result$class
      ), call. = FALSE)
      unsupported_vars <- c(unsupported_vars, var)
      next
    }
    
    if (!is.null(impute_result) && impute_result$method != "none") {
      data_imputed[[var]][missing_idx] <- impute_result$value
      n_imputed_total <- n_imputed_total + n_missing
      imputed_vars <- c(imputed_vars, var)
      
      imputation_info[[var]] <- list(
        n_imputed = n_missing,
        method = impute_result$method,
        value = impute_result$value,
        display_value = impute_result$display_value,
        class = impute_result$class
      )
    } else if (verbose && !is.null(impute_result) && impute_result$method == "none") {
      vc_message(sprintf("  %s: %d missing values could not be imputed", var, n_missing))
    }
  }
  
  remaining_missing <- count_missing(data_imputed)
  pct_imputed <- if (total_cells_after_removal > 0) {
    100 * n_imputed_total / total_cells_after_removal
  } else {
    0
  }
  
  summary <- list(
    initial_missing = initial_missing,
    missing_after_response_removal = missing_after_response_removal,
    remaining_missing = remaining_missing,
    n_removed_rows = n_removed_rows,
    imputed_vars = imputed_vars,
    imputation_info = imputation_info,
    all_na_vars = all_na_vars,
    unsupported_vars = unsupported_vars,
    pct_imputed = pct_imputed
  )
  
  if (verbose) {
    summary_text <- build_summary_text(summary, n_imputed_total)
    vc_message(paste(summary_text, collapse = "\n"))
  }
  
  return(list(
    data = data_imputed,
    summary = summary,
    imputed_vars = imputed_vars,
    imputation_info = imputation_info,
    initial_missing = initial_missing,
    missing_after_response_removal = missing_after_response_removal,
    remaining_missing = remaining_missing,
    n_imputed_total = n_imputed_total,
    pct_imputed = pct_imputed,
    n_removed_rows = n_removed_rows,
    all_na_vars = all_na_vars,
    unsupported_vars = unsupported_vars
  ))
}

# ============================================================
# Convenience Function
# ============================================================

#' Quick Missing Value Summary
#'
#' Provides a quick summary of missing values in a data frame
#'
#' @param data A data frame
#' @param verbose Logical, print summary (default: TRUE)
#'
#' @return A list with missing value statistics (invisibly if verbose=TRUE)
#'
#' @examples
#' df <- data.frame(Yield = rnorm(20, 5000, 500),
#'                   Nitrogen = c(rnorm(17, 120, 20), NA, NA, NA))
#' summary_result <- missing_summary(df, verbose = FALSE)
#' summary_result$total_missing
#'
#' @export
missing_summary <- function(data, verbose = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  if (nrow(data) == 0) {
    warning("Data has zero rows", call. = FALSE)
    result <- list(
      n_rows = 0,
      n_cols = ncol(data),
      total_missing = 0,
      missing_percent = 0,
      n_vars_with_missing = 0,
      missing_by_var = integer(0)
    )
    if (verbose) {
      vc_message("\n", paste(rep("=", 50), collapse = ""))
      vc_message("MISSING VALUE SUMMARY")
      vc_message(paste(rep("=", 50), collapse = ""))
      vc_message("  Data has zero rows")
      vc_message(paste(rep("=", 50), collapse = ""))
    }
    return(invisible(result))
  }
  
  missing_count <- vapply(data, function(x) sum(is.na(x)), integer(1), USE.NAMES = FALSE)
  names(missing_count) <- names(data)
  missing_vars <- names(missing_count[missing_count > 0])
  
  total_missing <- count_missing(data)
  total_cells <- nrow(data) * ncol(data)
  
  missing_by_var <- sort(missing_count[missing_count > 0], decreasing = TRUE)
  
  result <- list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    total_missing = total_missing,
    missing_percent = if (total_cells > 0) 100 * total_missing / total_cells else 0,
    n_vars_with_missing = length(missing_vars),
    missing_by_var = missing_by_var
  )
  
  if (verbose) {
    vc_message("\n", paste(rep("=", 50), collapse = ""))
    vc_message("MISSING VALUE SUMMARY")
    vc_message(paste(rep("=", 50), collapse = ""))
    vc_message(sprintf("  Total cells: %d", total_cells))
    vc_message(sprintf("  Missing cells: %d (%.2f%%)", total_missing, result$missing_percent))
    vc_message(sprintf("  Variables with missing: %d", length(missing_vars)))
    
    if (length(missing_vars) > 0) {
      vc_message("\n  Variables with missing values:")
      for (var in names(missing_by_var)) {
        pct <- 100 * missing_by_var[var] / nrow(data)
        vc_message(sprintf("    %s: %d (%.1f%%)", var, missing_by_var[var], pct))
      }
    }
    vc_message(paste(rep("=", 50), collapse = ""))
  }
  
  return(invisible(result))
}
