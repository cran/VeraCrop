# ============================================================
# File: R/preprocessing/filtering.R
# VeraCrop: Variable Filtering (IDs, Constant, NZV, Correlation)
# ============================================================

# ============================================================
# Internal Helpers
# ============================================================

#' Create standard filtered result structure
#'
#' @param data Filtered data frame
#' @param removed Character vector of removed variable names
#' @return List with data and removed fields
#' @noRd
filter_result <- function(data, removed) {
  list(
    data = data,
    removed = sort(unique(removed))
  )
}

#' Trim leading/trailing whitespace from character columns
#'
#' Real-world CSV/Excel exports very commonly introduce accidental leading
#' or trailing whitespace on text values (from copy-paste, cell formatting,
#' trailing tabs, etc.). Left untrimmed, this silently splits what should
#' be a single category into two distinct levels - e.g. "Sirvan" (22 rows)
#' and "Sirvan " (4 rows) being treated as two different cultivars - with
#' no error or warning anywhere downstream, since both are perfectly valid
#' (if different) character strings from R's point of view. This function
#' is called automatically and by default at the start of
#' \code{\link{prep_yield_gap}}.
#'
#' @param data A data frame
#' @param verbose Logical. Report which columns/values were affected.
#'   Default: TRUE.
#'
#' @return The data frame with whitespace trimmed from all character
#'   columns. Factor and non-character columns are left unchanged.
#'
#' @examples
#' df <- data.frame(Cultivar = c("Sirvan", "Sirvan ", " Sirvan"),
#'                   Yield = c(5000, 4900, 5100),
#'                   stringsAsFactors = FALSE)
#' trimmed <- trim_whitespace(df, verbose = FALSE)
#' length(unique(trimmed$Cultivar))
#'
#' @export
trim_whitespace <- function(data, verbose = TRUE) {

  if (!is.data.frame(data)) stop("'data' must be a data frame")

  char_cols <- names(data)[vapply(data, is.character, logical(1))]
  affected <- character(0)

  for (col in char_cols) {
    x <- data[[col]]
    x_trimmed <- trimws(x)
    # Only report/act on columns where trimming actually changes at least
    # one non-NA value (avoids noisy false positives for already-clean data).
    changed <- !is.na(x) & (x != x_trimmed)
    if (any(changed)) {
      affected <- c(affected, col)
      data[[col]] <- x_trimmed
    }
  }

  if (verbose && length(affected) > 0) {
    message(sprintf(
      "  Trimmed leading/trailing whitespace in: %s",
      paste(affected, collapse = ", ")
    ))
  }

  data
}


#' Remove columns from data frame and return filtered result
#'
#' @param data A data frame
#' @param removed Character vector of column names to remove
#' @return Filtered result list
#' @noRd
remove_columns <- function(data, removed) {
  removed <- unique(removed)
  if (length(removed) > 0) {
    data <- data[, !names(data) %in% removed, drop = FALSE]
  }
  filter_result(data, removed)
}

#' Check if a variable is constant (all non-NA values are identical)
#'
#' Note: list-columns and other non-atomic types are not supported by most
#' statistical modeling functions in R. This function returns FALSE for
#' non-atomic vectors, effectively ignoring them for the purpose of
#' constant variable removal.
#'
#' @param x A vector
#' @return Logical, TRUE if variable is constant
#' @noRd
is_constant <- function(x) {
  # Only works on atomic vectors; list-columns are ignored
  if (!is.atomic(x)) return(FALSE)

  x_clean <- x[!is.na(x)]

  # Fast path for small or empty vectors
  if (length(x_clean) <= 1) return(TRUE)

  # For most types, use unique() which handles NaN correctly
  length(unique(x_clean)) == 1
}

#' Calculate near-zero variance statistics
#'
#' Implements the same logic as caret::nearZeroVar (freqRatio /
#' percentUnique concepts), but returns fields named \code{freq_ratio} and
#' \code{percent_unique} (snake_case), NOT \code{freqRatio}/\code{percentUnique}.
#' Callers must use these exact field names - a earlier version of this
#' package accessed a nonexistent \code{$percentUnique} field, which
#' silently evaluated to NULL and caused \code{if (NA)} errors downstream.
#'
#' @param x A vector
#' @return List with \code{freq_ratio} and \code{percent_unique}, or NULL if
#'   the statistics cannot be computed (e.g. constant or all-NA input).
#' @noRd
nzv_stats <- function(x) {
  # Only works on atomic vectors
  if (!is.atomic(x)) return(NULL)

  x_clean <- x[!is.na(x)]

  # For factors, drop unused levels to avoid counting them
  if (is.factor(x_clean)) {
    x_clean <- droplevels(x_clean)
  }

  if (length(x_clean) < 2) return(NULL)

  freq <- table(x_clean, useNA = "no")
  if (length(freq) == 0) return(NULL)

  sorted_freq <- sort(freq, decreasing = TRUE)

  # Frequency ratio: most common / second most common
  if (length(sorted_freq) >= 2) {
    freq_ratio <- sorted_freq[1] / sorted_freq[2]
  } else {
    # Only one unique value -> constant (handled elsewhere)
    return(NULL)
  }

  # Percent unique
  percent_unique <- length(freq) / length(x_clean) * 100

  list(
    freq_ratio = as.numeric(freq_ratio),
    percent_unique = percent_unique
  )
}

#' Print filtering summary
#'
#' @param removed List of removed variables by category
#' @param n_initial Initial number of columns
#' @param n_final Final number of columns
#' @noRd
print_filtering_summary <- function(removed, n_initial, n_final) {
  message("\n", paste(rep("=", 60), collapse = ""))
  message("FILTERING SUMMARY")
  message(paste(rep("=", 60), collapse = ""))

  total_removed <- n_initial - n_final

  if (length(removed$non_predictors) > 0) {
    message(sprintf("  Removed non-predictor variables: %d", length(removed$non_predictors)))
  }
  if (length(removed$constant) > 0) {
    message(sprintf("  Removed constant variables: %d", length(removed$constant)))
  }
  if (length(removed$nzv) > 0) {
    message(sprintf("  Removed near-zero variance variables: %d", length(removed$nzv)))
  }
  if (length(removed$correlated) > 0) {
    message(sprintf("  Removed highly correlated variables: %d", length(removed$correlated)))
  }

  # Safe reduction calculation (avoid division by zero)
  reduction <- if (n_initial > 0) {
    100 * (n_initial - n_final) / n_initial
  } else {
    0
  }

  message(sprintf("\n  Variables removed: %d", total_removed))
  message(sprintf("  Reduction: %.1f%%", reduction))
  message(sprintf("  Initial columns: %d", n_initial))
  message(sprintf("  Final columns: %d", n_final))
  message(paste(rep("=", 60), collapse = ""))
}

# ============================================================
# 1. Remove Non-Predictor Variables
# ============================================================

#' Remove Non-Predictor Variables
#'
#' Removes variables that should not be used as predictors, such as ID columns.
#' Date and location variables are NOT removed by default.
#'
#' @param data A data frame
#' @param exclude_vars Character vector of variable names to exclude
#' @param exclude_patterns Character vector of regex patterns (default: ID-related)
#' @param verbose Logical, print progress (default: TRUE)
#'
#' @return A list with:
#'   \item{data}{Filtered data frame}
#'   \item{removed}{Character vector of removed variables}
#'
#' @examples
#' df <- data.frame(Field_ID = 1:10, Yield = rnorm(10, 5000, 500),
#'                   Nitrogen = rnorm(10, 120, 20))
#' result <- remove_non_predictors(df, verbose = FALSE)
#' names(result$data)
#'
#' @export
remove_non_predictors <- function(data,
                                  exclude_vars = NULL,
                                  exclude_patterns = NULL,
                                  verbose = TRUE) {

  if (is.null(exclude_patterns)) {
    # Caret-like ID patterns: ID must be at start or after separator
    exclude_patterns <- c(
      "(^|[_.])ID$",      # Ends with ID, preceded by start, underscore, or dot
      "(^|[_.])id$",      # Ends with id, preceded by start, underscore, or dot
      "^ID[_.]",          # Starts with ID_ or ID.
      "^id[_.]",          # Starts with id_ or id.
      "^(sample|plot|field|farm|row)[_.]?id$"  # Common agricultural ID patterns
    )
  }

  vars_to_remove <- character()

  if (!is.null(exclude_vars)) {
    vars_to_remove <- c(vars_to_remove, intersect(exclude_vars, names(data)))
  }

  # Combine patterns for single pass grep (more efficient)
  combined_pattern <- paste(exclude_patterns, collapse = "|")
  matched <- grep(combined_pattern, names(data), value = TRUE, ignore.case = TRUE)
  vars_to_remove <- unique(c(vars_to_remove, matched))

  if (verbose && length(vars_to_remove) > 0) {
    message(sprintf("  Removed non-predictor variables: %s",
                    paste(vars_to_remove, collapse = ", ")))
  }

  return(remove_columns(data, vars_to_remove))
}

# ============================================================
# 2. Remove Constant Variables
# ============================================================

#' Remove Constant Variables
#'
#' Removes variables that have only one unique non-NA value (including
#' all-NA columns). Works for ALL data types (numeric, factor, character).
#'
#' @param data A data frame
#' @param response_var Name of response variable (excluded from removal)
#' @param verbose Logical, print progress (default: TRUE)
#'
#' @return A list with:
#'   \item{data}{Filtered data frame}
#'   \item{removed}{Character vector of removed variables}
#'
#' @examples
#' df <- data.frame(Yield = rnorm(20, 5000, 500), AlwaysFive = 5)
#' result <- remove_constant_vars(df, response_var = "Yield", verbose = FALSE)
#' names(result$data)
#'
#' @export
remove_constant_vars <- function(data, response_var = "Yield", verbose = TRUE) {

  if (!response_var %in% names(data)) {
    warning(sprintf("Response variable '%s' not found in data. Skipping constant variable removal.", response_var))
    return(filter_result(data, character()))
  }

  if (verbose) message("\n--- Removing constant variables ---")

  removed <- character()

  for (var in names(data)) {
    if (var == response_var) next

    if (is_constant(data[[var]])) {
      removed <- c(removed, var)
      if (verbose) {
        x_clean <- data[[var]][!is.na(data[[var]])]
        if (length(x_clean) == 0) {
          message(sprintf("  %s: all NA, treating as constant", var))
        } else {
          message(sprintf("  %s: constant variable", var))
        }
      }
    }
  }

  if (verbose) {
    if (length(removed) > 0) {
      message(sprintf("  Removed %d constant variables", length(removed)))
    } else {
      message("  No constant variables found")
    }
  }

  return(remove_columns(data, removed))
}

# ============================================================
# 3. Remove Near Zero Variance Variables
# ============================================================

#' Remove Near Zero Variance Variables
#'
#' Identifies and removes variables with near zero variance using the same
#' logic as caret::nearZeroVar:
#' - freqRatio: ratio of most common to second most common value
#' - percentUnique: percentage of unique values
#'
#' A variable is considered near-zero variance if:
#'   freqRatio > freq_cut AND percentUnique < unique_cut
#'
#' @param data A data frame
#' @param response_var Name of response variable (excluded from removal)
#' @param freq_cut Numeric, frequency ratio cutoff (default: 19)
#' @param unique_cut Numeric, percent unique cutoff (default: 10)
#' @param verbose Logical, print progress (default: TRUE)
#'
#' @return A list with:
#'   \item{data}{Filtered data frame}
#'   \item{removed}{Character vector of removed variables}
#'
#' @examples
#' df <- data.frame(Yield = rnorm(30, 5000, 500),
#'                   AlmostConstant = c(rep(1, 28), 2, 3))
#' result <- remove_near_zero_variance(df, response_var = "Yield", verbose = FALSE)
#' names(result$data)
#'
#' @export
remove_near_zero_variance <- function(data,
                                      response_var = "Yield",
                                      freq_cut = 19,
                                      unique_cut = 10,
                                      verbose = TRUE) {

  # Validate parameters
  if (!is.numeric(freq_cut) || length(freq_cut) != 1 || freq_cut <= 1) {
    stop("freq_cut must be a single numeric value > 1")
  }
  if (!is.numeric(unique_cut) || length(unique_cut) != 1 || unique_cut <= 0 || unique_cut >= 100) {
    stop("unique_cut must be a single numeric value between 0 and 100")
  }

  if (!response_var %in% names(data)) {
    warning(sprintf("Response variable '%s' not found in data. Skipping near-zero variance removal.", response_var))
    return(filter_result(data, character()))
  }

  if (verbose) message("\n--- Removing near-zero variance variables ---")

  removed <- character()

  for (var in names(data)) {
    if (var == response_var) next

    stats <- nzv_stats(data[[var]])
    if (is.null(stats)) next

    if (stats$freq_ratio > freq_cut && stats$percent_unique < unique_cut) {
      removed <- c(removed, var)
      if (verbose) {
        message(sprintf("  %s: near-zero variance (freqRatio: %.1f, percentUnique: %.1f%%)",
                        var, stats$freq_ratio, stats$percent_unique))
      }
    }
  }

  if (verbose) {
    if (length(removed) > 0) {
      message(sprintf("  Removed %d near-zero variance variables", length(removed)))
    } else {
      message("  No near-zero variance variables found")
    }
  }

  return(remove_columns(data, removed))
}

# ============================================================
# 4. Remove Highly Correlated Variables
# ============================================================

#' Remove Highly Correlated Variables
#'
#' Identifies and removes highly correlated numeric variables using an
#' iterative greedy algorithm similar to caret::findCorrelation().
#'
#' The algorithm:
#' 1. Finds the pair with the highest absolute correlation above threshold
#' 2. Removes one variable from the pair (based on keep_method)
#' 3. Recalculates correlations and repeats until no pair exceeds threshold
#'
#' @param data A data frame
#' @param response_var Name of response variable (used to decide which variable to keep)
#' @param threshold Numeric, correlation threshold (default: 0.90)
#' @param keep_method Character, "higher_correlation" (keep variable with higher cor to response),
#'   or "first" (keep the first variable encountered by position)
#' @param cor_method Character, correlation method: "pearson", "spearman", "kendall"
#' @param cor_use Character, method for handling missing values
#'   (default: "pairwise.complete.obs")
#' @param verbose Logical, print progress (default: TRUE)
#'
#' @return A list with:
#'   \item{data}{Filtered data frame}
#'   \item{removed}{Character vector of removed variables}
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(30, 100, 10)
#' df <- data.frame(Yield = rnorm(30, 5000, 500),
#'                   Nitrogen = x,
#'                   Nitrogen_copy = x + rnorm(30, 0, 0.01))
#' result <- remove_highly_correlated(df, response_var = "Yield", verbose = FALSE)
#' names(result$data)
#'
#' @export
remove_highly_correlated <- function(data,
                                     response_var = "Yield",
                                     threshold = 0.90,
                                     keep_method = "higher_correlation",
                                     cor_method = "pearson",
                                     cor_use = "pairwise.complete.obs",
                                     verbose = TRUE) {

  # Validate arguments
  keep_method <- match.arg(keep_method, c("higher_correlation", "first"))
  cor_method <- match.arg(cor_method, c("pearson", "spearman", "kendall"))
  cor_use <- match.arg(cor_use, c("pairwise.complete.obs", "complete.obs", "everything"))

  # Validate numeric parameters
  if (!is.numeric(threshold) || length(threshold) != 1 || threshold <= 0 || threshold >= 1) {
    stop("threshold must be a single numeric value between 0 and 1")
  }

  if (verbose) message("\n--- Removing highly correlated variables ---")

  # Get numeric variables (excluding response)
  numeric_vars <- names(data)[vapply(data, is.numeric, logical(1))]
  numeric_vars <- numeric_vars[numeric_vars != response_var]

  # Remove constant variables to avoid correlation issues
  numeric_vars <- numeric_vars[
    !vapply(data[numeric_vars], is_constant, logical(1))
  ]

  if (length(numeric_vars) <= 1) {
    if (verbose) message("  Not enough numeric variables to check correlation")
    return(filter_result(data, character()))
  }

  # Validate response variable and method compatibility
  if (keep_method == "higher_correlation") {
    if (!response_var %in% names(data)) {
      if (verbose) message("  Response variable not found. Switching to 'first' keep method.")
      keep_method <- "first"
    } else if (!is.numeric(data[[response_var]])) {
      if (verbose) message("  Response is not numeric. Switching to 'first' keep method.")
      keep_method <- "first"
    }
  }

  removed <- character()
  vars_remaining <- numeric_vars

  # Pre-compute correlation with response (for keep_method)
  cor_with_response <- numeric()
  if (keep_method == "higher_correlation") {
    # Check if response has zero variance
    if (is_constant(data[[response_var]])) {
      if (verbose) message("  Response variable is constant. Switching to 'first' keep method.")
      keep_method <- "first"
    } else {
      # Suppress warnings for zero-variance variables
      for (var in numeric_vars) {
        cor_val <- suppressWarnings(
          cor(data[[var]], data[[response_var]], use = cor_use, method = cor_method)
        )
        # Ensure finite values
        cor_with_response[var] <- if (is.finite(cor_val)) cor_val else NA_real_
      }
    }
  }

  # Tolerance for floating point comparisons
  tol <- sqrt(.Machine$double.eps)

  # Iterative greedy algorithm (no fixed iteration limit)
  while (TRUE) {
    if (length(vars_remaining) <= 1) break

    # Compute correlation matrix for remaining variables
    cor_matrix <- tryCatch({
      suppressWarnings(
        cor(data[, vars_remaining, drop = FALSE], use = cor_use, method = cor_method)
      )
    }, error = function(e) {
      if (verbose) message(sprintf("  Correlation calculation failed: %s", e$message))
      return(NULL)
    })

    if (is.null(cor_matrix)) break

    # Find highest absolute correlation (ignoring diagonal)
    cor_abs <- abs(cor_matrix)
    diag(cor_abs) <- NA

    # Check for all NA correlations
    if (all(is.na(cor_abs))) break

    max_cor <- max(cor_abs, na.rm = TRUE)

    # Check for invalid max correlation
    if (!is.finite(max_cor) || max_cor <= threshold) break

    # Get the pair with highest correlation
    max_idx <- which(cor_abs >= max_cor - tol, arr.ind = TRUE)
    if (nrow(max_idx) == 0) break
    max_idx <- max_idx[1, ]
    if (length(max_idx) < 2) break

    var1 <- vars_remaining[max_idx[1]]
    var2 <- vars_remaining[max_idx[2]]

    # Decide which to remove
    if (keep_method == "higher_correlation") {
      cor1 <- cor_with_response[var1]
      cor2 <- cor_with_response[var2]

      if (is.na(cor1) && is.na(cor2)) {
        to_remove <- var2
      } else if (is.na(cor1)) {
        to_remove <- var1
      } else if (is.na(cor2)) {
        to_remove <- var2
      } else if (abs(cor1) >= abs(cor2)) {
        to_remove <- var2
      } else {
        to_remove <- var1
      }
    } else {
      # Keep the first variable by position in vars_remaining
      pos1 <- match(var1, vars_remaining)
      pos2 <- match(var2, vars_remaining)
      to_remove <- if (pos1 < pos2) var2 else var1
    }

    removed <- c(removed, to_remove)
    vars_remaining <- setdiff(vars_remaining, to_remove)

    if (verbose && length(removed) <= 10) {
      message(sprintf("  Removing %s (correlated with %s, r = %.3f)",
                      to_remove, if (to_remove == var1) var2 else var1, max_cor))
    }
  }

  if (verbose) {
    if (length(removed) > 0) {
      message(sprintf("  Removed %d highly correlated variables (threshold: %.2f)",
                      length(removed), threshold))
    } else {
      message("  No highly correlated variables found")
    }
  }

  return(remove_columns(data, removed))
}

# ============================================================
# 5. Combined Filtering Function
# ============================================================

#' Apply All Filtering Steps
#'
#' Convenience function that applies all filtering steps in the recommended order:
#' 1. Remove non-predictors (IDs)
#' 2. Remove constant variables
#' 3. Remove near-zero variance variables
#' 4. Remove highly correlated variables
#'
#' Returns a list with filtered data and detailed removal records.
#'
#' @param data A data frame
#' @param response_var Name of response variable
#' @param exclude_vars Character vector of variable names to exclude
#' @param exclude_patterns Character vector of regex patterns for ID removal
#' @param freq_cut Numeric, frequency ratio cutoff for NZV (default: 19)
#' @param unique_cut Numeric, percent unique cutoff for NZV (default: 10)
#' @param corr_threshold Numeric, correlation threshold (default: 0.90)
#' @param keep_method Character, method for correlated variable removal
#' @param cor_method Character, correlation method
#' @param cor_use Character, method for handling missing values
#' @param verbose Logical, print progress (default: TRUE)
#'
#' @return A list with:
#'   \item{data}{Filtered data frame}
#'   \item{removed}{List of removed variables by category}
#'   \item{n_removed}{Total number of variables removed}
#'   \item{n_initial}{Initial number of columns}
#'   \item{n_final}{Final number of columns}
#'
#' @examples
#' data(wheat1)
#' result <- filter_variables(wheat1, response_var = "Yield", verbose = FALSE)
#' result$n_initial
#' result$n_final
#'
#' @export
filter_variables <- function(data,
                             response_var = "Yield",
                             exclude_vars = NULL,
                             exclude_patterns = NULL,
                             freq_cut = 19,
                             unique_cut = 10,
                             corr_threshold = 0.90,
                             keep_method = "higher_correlation",
                             cor_method = "pearson",
                             cor_use = "pairwise.complete.obs",
                             verbose = TRUE) {

  # Validate input
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }

  if (verbose) {
    message("\n", paste(rep("=", 60), collapse = ""))
    message("VARIABLE FILTERING")
    message(paste(rep("=", 60), collapse = ""))
    message(sprintf("Input: %d rows, %d columns", nrow(data), ncol(data)))
  }

  n_initial <- ncol(data)
  removed <- list(
    non_predictors = character(),
    constant = character(),
    nzv = character(),
    correlated = character()
  )

  # Step 1: Remove non-predictors
  result <- remove_non_predictors(
    data = data,
    exclude_vars = exclude_vars,
    exclude_patterns = exclude_patterns,
    verbose = verbose
  )
  data <- result$data
  removed$non_predictors <- result$removed

  # Step 2: Remove constant variables
  result <- remove_constant_vars(
    data = data,
    response_var = response_var,
    verbose = verbose
  )
  data <- result$data
  removed$constant <- result$removed

  # Step 3: Remove near-zero variance variables
  result <- remove_near_zero_variance(
    data = data,
    response_var = response_var,
    freq_cut = freq_cut,
    unique_cut = unique_cut,
    verbose = verbose
  )
  data <- result$data
  removed$nzv <- result$removed

  # Step 4: Remove highly correlated variables
  result <- remove_highly_correlated(
    data = data,
    response_var = response_var,
    threshold = corr_threshold,
    keep_method = keep_method,
    cor_method = cor_method,
    cor_use = cor_use,
    verbose = verbose
  )
  data <- result$data
  removed$correlated <- result$removed

  n_final <- ncol(data)
  n_total_removed <- n_initial - n_final

  if (verbose) {
    print_filtering_summary(removed, n_initial, n_final)
  }

  return(list(
    data = data,
    removed = removed,
    n_removed = n_total_removed,
    n_initial = n_initial,
    n_final = n_final
  ))
}
