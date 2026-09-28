# ============================================================
# File: R/preprocessing/validation.R
# Validation Utilities
# ============================================================

# ============================================================
# Constants
# ============================================================

.LINE_WIDTH <- 50
.LINE <- paste(rep("=", .LINE_WIDTH), collapse = "")

# ============================================================
# Internal Helpers - Data Scanning
# ============================================================

#' Internal helper for safe missing value counting (single vector)
#'
#' NOTE: intentionally named distinctly from missing.R's count_missing(),
#' which operates on an entire data frame and returns a single total count.
#' This helper operates on ONE vector at a time and is used here via
#' vapply() to build a per-column missing count. Package-wide, no two
#' top-level functions may share a name.
#'
#' @noRd
.count_missing_vec <- function(x) {
  if (is.numeric(x)) {
    sum(is.na(x) | is.nan(x))
  } else {
    sum(is.na(x))
  }
}

# NOTE: is_constant() is intentionally NOT redefined here. The canonical
# definition lives in filtering.R and is available package-wide within the
# same namespace (it additionally guards against non-atomic list-columns).
# Defining it a second time here would silently overwrite one version with
# the other depending on file load order - exactly the kind of collision
# this package must avoid.

#' Scan duplicate column names
#'
#' @noRd
scan_duplicate_columns <- function(data) {
  if (!is.data.frame(data)) return(character())
  unique(names(data)[duplicated(names(data))])
}

#' Scan missing values
#'
#' @noRd
scan_missing <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) return(character())
  names(data)[vapply(data, function(x) {
    if (is.numeric(x)) any(is.na(x) | is.nan(x)) else any(is.na(x))
  }, logical(1))]
}

#' Scan infinite values
#'
#' @noRd
scan_infinite <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) return(character())
  names(data)[vapply(data, function(x) {
    is.numeric(x) && any(is.infinite(x))
  }, logical(1))]
}

#' Scan constant variables
#'
#' @noRd
scan_constant <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) return(character())
  names(data)[vapply(data, is_constant, logical(1))]
}

#' Main data scanner - coordinates all individual scans
#'
#' @noRd
scan_data <- function(data) {
  list(
    duplicate = scan_duplicate_columns(data),
    missing = scan_missing(data),
    infinite = scan_infinite(data),
    constant = scan_constant(data)
  )
}

# ============================================================
# Internal Helpers - Validation (returns raw data)
# ============================================================

#' Check response variable existence
#'
#' @noRd
check_response <- function(data, response_var) {
  !is.null(response_var) &&
  length(response_var) == 1 &&
  !is.na(response_var) &&
  nzchar(response_var) &&
  response_var %in% names(data)
}

#' Inspect encoding information
#'
#' @noRd
inspect_encoding <- function(encoding_info) {
  result <- list(valid_count = 0L, invalid_vars = character())
  if (is.null(encoding_info) || length(encoding_info) == 0) return(result)
  
  for (var in names(encoding_info)) {
    info <- encoding_info[[var]]
    is_valid <- !is.null(info$original) && 
                !is.null(info$encoded) && 
                length(info$encoded) > 0 && 
                !all(is.na(info$encoded)) &&
                !is.null(info$type)
    if (is_valid) result$valid_count <- result$valid_count + 1L
    else result$invalid_vars <- c(result$invalid_vars, var)
  }
  result
}

#' Inspect scaling information
#'
#' @noRd
inspect_scaling <- function(scaling_params) {
  result <- list(
    valid_count = 0L,
    scale_zero = character(),
    incomplete = character()
  )
  if (is.null(scaling_params) || length(scaling_params) == 0) return(result)
  
  for (var in names(scaling_params)) {
    info <- scaling_params[[var]]
    
    # Check for scale <= 0 (critical) - with proper NA handling
    if (!is.null(info$scale) && 
        length(info$scale) == 1 && 
        is.finite(info$scale) &&
        info$scale <= 0) {
      result$scale_zero <- c(result$scale_zero, var)
      next
    }
    
    # Check for incomplete info
    is_complete <- !is.null(info$center) && 
                   !is.null(info$scale) &&
                   length(info$center) == 1 &&
                   length(info$scale) == 1 &&
                   is.finite(info$center) && 
                   is.finite(info$scale)
    
    if (is_complete) result$valid_count <- result$valid_count + 1L
    else result$incomplete <- c(result$incomplete, var)
  }
  result
}

# ============================================================
# Internal Helpers - Message Building (raw data to messages)
# ============================================================

#' Build validation messages from raw inspection results
#'
#' @noRd
build_validation_messages <- function(scan_result, response_exists, response_var,
                                       encoding_result = NULL, scaling_result = NULL) {
  
  issues <- character()
  warnings <- character()
  
  # Errors
  if (length(scan_result$duplicate) > 0) {
    issues <- c(issues, sprintf("Duplicate column names: %s", 
                                paste(scan_result$duplicate, collapse = ", ")))
  }
  if (length(scan_result$missing) > 0) {
    issues <- c(issues, sprintf("Missing values (NA/NaN) in: %s", 
                                paste(scan_result$missing, collapse = ", ")))
  }
  if (length(scan_result$infinite) > 0) {
    issues <- c(issues, sprintf("Infinite values in: %s", 
                                paste(scan_result$infinite, collapse = ", ")))
  }
  if (!response_exists) {
    issues <- c(issues, sprintf("Response variable '%s' not found", 
                                response_var %||% "NULL"))
  }
  
  # Warnings
  if (length(scan_result$constant) > 0) {
    warnings <- c(warnings, sprintf("Constant variables: %s", 
                                    paste(scan_result$constant, collapse = ", ")))
  }
  
  if (!is.null(encoding_result) && length(encoding_result$invalid_vars) > 0) {
    warnings <- c(warnings, sprintf("Encoding info incomplete for: %s", 
                                    paste(encoding_result$invalid_vars, collapse = ", ")))
  }
  
  if (!is.null(scaling_result)) {
    if (length(scaling_result$scale_zero) > 0) {
      issues <- c(issues, sprintf("Invalid scale (<= 0) for variable(s): %s", 
                                  paste(scaling_result$scale_zero, collapse = ", ")))
    }
    if (length(scaling_result$incomplete) > 0) {
      warnings <- c(warnings, sprintf("Incomplete scaling info for: %s", 
                                      paste(scaling_result$incomplete, collapse = ", ")))
    }
  }
  
  list(
    issues = issues,
    warnings = warnings,
    n_errors = length(issues),
    n_warnings = length(warnings)
  )
}

# ============================================================
# Internal Helpers - Formatting
# ============================================================

#' Format numeric value for display
#'
#' @noRd
format_numeric <- function(x, digits = 3) {
  if (is.null(x) || length(x) != 1 || !is.finite(x) || is.na(x)) return("NA")
  sprintf(paste0("%.", digits, "f"), x)
}

#' Wrap text for display
#'
#' @noRd
wrap_text <- function(text, width = 70, indent = 2) {
  if (length(text) == 0 || text == "") return("")
  if (indent >= width) indent <- max(0, width - 10)
  
  wrapped <- strwrap(text, width = width - indent, initial = "", prefix = "")
  if (length(wrapped) == 0) return("")
  
  indent_str <- paste(rep(" ", indent), collapse = "")
  paste(paste0(indent_str, wrapped), collapse = "\n")
}

#' Format issue list for display
#'
#' @noRd
format_issue_list <- function(issues, max_display = 5) {
  if (length(issues) == 0) return("  None")
  
  first <- paste0("  - ", utils::head(issues, max_display))
  more <- if (length(issues) > max_display) {
    sprintf("  (+%d more)", length(issues) - max_display)
  } else NULL
  paste(c(first, more), collapse = "\n")
}

# ============================================================
# Internal Helpers - Summary
# ============================================================

#' Create preprocessing summary
#'
#' @noRd
summarize_preprocessing <- function(data, var_types, encoding_result, scaling_result) {
  
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  missing_cells <- if (n_rows > 0 && n_cols > 0) {
    sum(vapply(data, .count_missing_vec, integer(1)))
  } else 0L
  
  missing_percent <- if (n_rows > 0 && n_cols > 0) {
    100 * missing_cells / (n_rows * n_cols)
  } else NA_real_
  
  summary <- list(
    n_rows = n_rows,
    n_cols = n_cols,
    missing_cells = missing_cells,
    missing_percent = missing_percent,
    n_encoded = encoding_result$valid_count %||% 0L,
    n_scaled = scaling_result$valid_count %||% 0L,
    type_counts = NULL
  )
  
  if (!is.null(var_types) && length(var_types) > 0) {
    summary$type_counts <- table(vapply(
      var_types,
      function(x) x$type %||% "unknown",
      character(1)
    ))
  }
  
  summary
}

# ============================================================
# Internal Helpers - Printing
# ============================================================

#' Print validation failure
#'
#' @noRd
print_validation_failure <- function(issues) {
  cat("\n", .LINE, "\n")
  cat("PREPROCESSING VALIDATION FAILED\n")
  cat(.LINE, "\n")
  cat("  [ERROR] Critical issues found:\n")
  for (issue in issues) cat(sprintf("    - %s\n", issue))
}

#' Print validation summary
#'
#' NOTE: status flags (has_missing, has_duplicate, etc.) are derived
#' directly from scan_result / response_exists - the same structured data
#' used to build the issue/warning messages - rather than by re-parsing the
#' formatted message text with grepl(). Re-parsing text is fragile: if the
#' wording in build_validation_messages() ever changes, a grepl()-based
#' check would silently stop matching and always report "no issues" here,
#' even when real issues exist.
#'
#' @noRd
print_validation_summary <- function(summary, scan_result, response_exists,
                                     issues, warnings, response_var) {
  cat("\n", .LINE, "\n")
  cat("PREPROCESSING VALIDATION\n")
  cat(.LINE, "\n")
  
  cat(sprintf("  Data: %d rows, %d columns\n", summary$n_rows, summary$n_cols))
  if (!is.na(summary$missing_percent)) {
    cat(sprintf("  Missing cells: %d (%.2f%%)\n", summary$missing_cells, summary$missing_percent))
  } else {
    cat(sprintf("  Missing cells: %d\n", summary$missing_cells))
  }
  
  has_response <- isTRUE(response_exists)
  cat(sprintf("  Response: %s\n", if (has_response) response_var else "MISSING"))
  cat("\n")
  
  n_errors <- length(issues)
  n_warnings <- length(warnings)
  
  cat("  Validation Status:\n")
  cat(sprintf("    Valid: %s\n", if (n_errors == 0) "TRUE" else "FALSE"))
  cat(sprintf("    Errors: %d\n", n_errors))
  cat(sprintf("    Warnings: %d\n", n_warnings))
  cat("\n")
  
  has_missing   <- length(scan_result$missing) > 0
  has_duplicate <- length(scan_result$duplicate) > 0
  has_infinite  <- length(scan_result$infinite) > 0
  has_constant  <- length(scan_result$constant) > 0
  
  cat("  Summary:\n")
  cat(sprintf("    [%s] Data structure: %s\n",
              if (summary$n_rows > 0 && summary$n_cols > 0 && !has_duplicate) "OK" else "ISSUE",
              if (summary$n_rows > 0 && summary$n_cols > 0 && !has_duplicate) "OK" else "Issues"))
  cat(sprintf("    [%s] Missing values: %s\n",
              if (!has_missing) "OK" else "ISSUE", if (!has_missing) "None" else "Present"))
  cat(sprintf("    [%s] Infinite values: %s\n",
              if (!has_infinite) "OK" else "ISSUE", if (!has_infinite) "None" else "Present"))
  cat(sprintf("    [%s] Constant variables: %s\n",
              if (!has_constant) "OK" else "NOTE",
              if (!has_constant) "None" else "Present (warning)"))
  cat(sprintf("    [%s] Response variable: %s\n",
              if (has_response) "OK" else "ISSUE", if (has_response) "OK" else "Missing"))
  cat(sprintf("    [OK] Encoding: %d variables\n", summary$n_encoded))
  cat(sprintf("    [OK] Scaling: %d variables\n", summary$n_scaled))
  cat("\n")
  
  if (n_errors == 0 && n_warnings == 0) {
    cat("  [OK] All validation checks passed!\n")
    cat("  Data ready for analysis.\n")
  } else {
    if (n_warnings > 0) {
      cat("  [WARNING] Issues found:\n")
      for (w in warnings) cat(sprintf("    - %s\n", w))
    }
    if (n_errors > 0) {
      cat("  [ERROR] Critical issues found:\n")
      for (issue in issues) cat(sprintf("    - %s\n", issue))
    }
    if (n_errors == 0 && n_warnings > 0) {
      cat("  Data can be used with caution.\n")
    } else if (n_errors > 0) {
      cat("  Data requires preprocessing before analysis.\n")
    }
  }
}

# ============================================================
# Main Exported Functions
# ============================================================

#' Build a validate_preprocessing() result with a consistent schema
#'
#' Ensures every return path of validate_preprocessing() - including the
#' early-return error paths - produces a list with exactly the same set of
#' keys. Without this, callers doing e.g. `if (val$has_encoding)` after a
#' failed validation would hit `argument is of length zero`, since that key
#' would simply be missing rather than FALSE.
#'
#' @noRd
.make_validation_result <- function(valid, issues, warnings, summary, response_var,
                                    has_encoding = FALSE, has_scaling = FALSE) {
  list(
    valid = valid,
    issues = issues,
    warnings = warnings,
    n_errors = length(issues),
    n_warnings = length(warnings),
    summary = summary,
    response_var = response_var,
    has_encoding = has_encoding,
    has_scaling = has_scaling
  )
}

#' Validate Preprocessing Results
#'
#' Performs comprehensive validation of preprocessing output.
#'
#' @param preprocessed_result Output from prep_yield_gap()
#' @param verbose Logical, print validation results
#'
#' @return List with validation results, always including: valid, issues,
#'   warnings, n_errors, n_warnings, summary, response_var, has_encoding,
#'   has_scaling.
#'
#' @examples
#' df <- data.frame(Yield = rnorm(30, 5000, 500), Nitrogen = rnorm(30, 120, 20))
#' prep <- prep_yield_gap(df, response_var = "Yield", verbose = FALSE)
#' val <- validate_preprocessing(prep, verbose = FALSE)
#' val$valid
#'
#' @export
validate_preprocessing <- function(preprocessed_result, verbose = TRUE) {

  empty_summary <- list(n_rows = NA_integer_, n_cols = NA_integer_,
                         missing_cells = NA_integer_, missing_percent = NA_real_,
                         n_encoded = 0, n_scaled = 0, type_counts = NULL)

  # Input validation
  if (!is.list(preprocessed_result)) stop("preprocessed_result must be a list")

  required_fields <- c("data", "response_var", "var_types")
  missing_fields <- required_fields[!required_fields %in% names(preprocessed_result)]

  if (length(missing_fields) > 0) {
    issues <- sprintf("Missing required fields: %s", paste(missing_fields, collapse = ", "))
    if (verbose) print_validation_failure(issues)
    return(.make_validation_result(
      valid = FALSE, issues = issues, warnings = character(),
      summary = empty_summary, response_var = NA_character_
    ))
  }

  data <- preprocessed_result$data
  var_types <- preprocessed_result$var_types
  encoding_info <- preprocessed_result$encoding_info
  scaling_params <- preprocessed_result$scaling_params
  response_var <- preprocessed_result$response_var

  if (!is.data.frame(data)) {
    issues <- "data field must be a data frame"
    if (verbose) print_validation_failure(issues)
    return(.make_validation_result(
      valid = FALSE, issues = issues, warnings = character(),
      summary = empty_summary, response_var = response_var
    ))
  }

  # Scan and validate
  scan_result <- scan_data(data)
  response_exists <- check_response(data, response_var)
  encoding_result <- inspect_encoding(encoding_info)
  scaling_result <- inspect_scaling(scaling_params)

  # Build messages
  messages <- build_validation_messages(
    scan_result, response_exists, response_var,
    encoding_result, scaling_result
  )

  # Summary
  summary <- summarize_preprocessing(data, var_types, encoding_result, scaling_result)

  if (verbose) {
    print_validation_summary(summary, scan_result, response_exists,
                             messages$issues, messages$warnings, response_var)
  }

  .make_validation_result(
    valid = messages$n_errors == 0,
    issues = messages$issues,
    warnings = messages$warnings,
    summary = summary,
    response_var = response_var,
    has_encoding = summary$n_encoded > 0,
    has_scaling = summary$n_scaled > 0
  )
}

#' Create a Preprocessing Report
#'
#' @param preprocessed_result Output from prep_yield_gap()
#' @param validation_result Optional output from validate_preprocessing()
#' @param output_file Path to output file (optional)
#' @param verbose Logical, print report to console
#'
#' @return Character string with report (invisibly if verbose=TRUE)
#'
#' @examples
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' report <- preprocessing_report(prep, verbose = FALSE)
#' cat(report[1:5], sep = "\n")
#'
#' @export
preprocessing_report <- function(preprocessed_result, 
                                  validation_result = NULL,
                                  output_file = NULL, 
                                  verbose = TRUE) {
  
  if (!is.list(preprocessed_result) || !"data" %in% names(preprocessed_result)) {
    stop("Invalid preprocessing result")
  }
  
  data <- preprocessed_result$data
  var_types <- preprocessed_result$var_types
  encoding_info <- preprocessed_result$encoding_info
  scaling_params <- preprocessed_result$scaling_params
  response_var <- preprocessed_result$response_var
  
  # Use validation result or compute
  if (!is.null(validation_result) && is.list(validation_result) && 
      !is.null(validation_result$summary)) {
    summary <- validation_result$summary
    issues <- validation_result$issues %||% character()
    warnings <- validation_result$warnings %||% character()
    valid <- validation_result$valid %||% FALSE
    n_errors <- validation_result$n_errors %||% length(issues)
    n_warnings <- validation_result$n_warnings %||% length(warnings)
  } else {
    scan_result <- scan_data(data)
    response_exists <- check_response(data, response_var)
    encoding_result <- inspect_encoding(encoding_info)
    scaling_result <- inspect_scaling(scaling_params)
    messages <- build_validation_messages(
      scan_result, response_exists, response_var,
      encoding_result, scaling_result
    )
    summary <- summarize_preprocessing(data, var_types, encoding_result, scaling_result)
    issues <- messages$issues
    warnings <- messages$warnings
    valid <- messages$n_errors == 0
    n_errors <- messages$n_errors
    n_warnings <- messages$n_warnings
  }
  
  report <- character()
  
  # ---- Header ----
  report <- c(report, "========================================")
  report <- c(report, "PREPROCESSING REPORT")
  report <- c(report, "========================================", "")
  
  # ---- Overview ----
  report <- c(report, "--- Overview ---")
  report <- c(report, sprintf("  Response variable: %s", response_var %||% "unknown"))
  report <- c(report, sprintf("  Data size: %s", preprocessed_result$data_size %||% "unknown"))
  report <- c(report, sprintf("  Final rows: %d", summary$n_rows))
  report <- c(report, sprintf("  Final columns: %d", summary$n_cols))
  report <- c(report, sprintf("  Missing cells: %d", summary$missing_cells))
  report <- c(report, sprintf("  Variables encoded: %d", summary$n_encoded))
  report <- c(report, sprintf("  Variables scaled: %d", summary$n_scaled))
  report <- c(report, "")
  
  # ---- Validation Status ----
  report <- c(report, "--- Validation Status ---")
  report <- c(report, sprintf("  Valid: %s", if (valid) "TRUE" else "FALSE"))
  report <- c(report, sprintf("  Errors: %d", n_errors))
  report <- c(report, sprintf("  Warnings: %d", n_warnings))
  report <- c(report, "")
  
  # ---- Issues ----
  if (n_errors > 0) {
    report <- c(report, "--- Errors ---")
    report <- c(report, format_issue_list(issues))
    report <- c(report, "")
  }
  if (n_warnings > 0) {
    report <- c(report, "--- Warnings ---")
    report <- c(report, format_issue_list(warnings))
    report <- c(report, "")
  }
  
  # ---- Variable Types ----
  report <- c(report, "--- Variable Types ---")
  if (!is.null(summary$type_counts) && length(summary$type_counts) > 0) {
    for (type in names(summary$type_counts)) {
      report <- c(report, sprintf("  %s: %d", type, summary$type_counts[[type]]))
    }
  } else {
    report <- c(report, "  No variable type information available")
  }
  report <- c(report, "")
  
  # ---- Encoding Summary ----
  report <- c(report, "--- Encoding Summary ---")
  if (!is.null(encoding_info) && length(encoding_info) > 0) {
    any_encoded <- FALSE
    for (var in names(encoding_info)) {
      info <- encoding_info[[var]]
      encoded_vec <- info$encoded
      if (is.null(encoded_vec) || length(encoded_vec) == 0 || all(is.na(encoded_vec))) next
      any_encoded <- TRUE
      encoded_str <- paste(encoded_vec, collapse = ", ")
      if (nchar(encoded_str) > 50) encoded_str <- paste0(substr(encoded_str, 1, 47), "...")
      report <- c(report, sprintf("  %s -> %s (%s)", 
                                  info$original %||% var, encoded_str, info$type %||% "unknown"))
    }
    if (!any_encoded) report <- c(report, "  No variables successfully encoded")
  } else {
    report <- c(report, "  No variables encoded")
  }
  report <- c(report, "")
  
  # ---- Scaling Summary ----
  report <- c(report, "--- Scaling Summary ---")
  if (!is.null(scaling_params) && length(scaling_params) > 0) {
    any_scaled <- FALSE
    for (var in names(scaling_params)) {
      info <- scaling_params[[var]]
      if (is.null(info$center) || is.null(info$scale) || 
          !is.finite(info$center) || !is.finite(info$scale)) next
      any_scaled <- TRUE
      report <- c(report, sprintf("  %s: center=%s, scale=%s", 
                                  var, format_numeric(info$center), format_numeric(info$scale)))
    }
    if (!any_scaled) report <- c(report, "  No variables successfully scaled")
  } else {
    report <- c(report, "  No variables scaled")
  }
  report <- c(report, "")
  
  # ---- Column Names ----
  report <- c(report, "--- Final Columns ---")
  if (summary$n_cols <= 20) {
    report <- c(report, wrap_text(paste(names(data), collapse = ", "), width = 70, indent = 2))
  } else {
    report <- c(report, sprintf("  %d columns (first 10):", summary$n_cols))
    report <- c(report, wrap_text(paste(utils::head(names(data), 10), collapse = ", "), width = 70, indent = 4))
  }
  report <- c(report, "")
  
  # ---- Footer ----
  report <- c(report, "========================================")
  report <- c(report, sprintf("Report generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  report <- c(report, "========================================")
  
  if (verbose) cat(paste(report, collapse = "\n"), "\n")
  if (!is.null(output_file)) {
    writeLines(report, output_file)
    if (verbose) cat(sprintf("\nReport saved to: %s\n", output_file))
  }
  
  invisible(report)
}

#' Quick Check for Data Readiness
#'
#' @param data A data frame
#' @param response_var Name of response variable
#' @param verbose Logical, print results
#' @param details Logical, return detailed results (default: FALSE)
#'
#' @return If details=TRUE, returns a list with ready, issues, and warnings.
#'         Otherwise returns a logical.
#'
#' @examples
#' df <- data.frame(Yield = rnorm(20, 5000, 500), Nitrogen = rnorm(20, 120, 20))
#' check_data_ready(df, response_var = "Yield", verbose = FALSE)
#'
#' @export
check_data_ready <- function(data, response_var = NULL, verbose = TRUE, details = FALSE) {
  
  if (!is.data.frame(data)) {
    if (verbose) cat("  [ERROR] Not a data frame\n")
    return(if (details) list(ready = FALSE, issues = "Not a data frame", warnings = character()) else FALSE)
  }
  
  # Use consistent validation logic (no encoding/scaling for quick check)
  scan_result <- scan_data(data)
  response_exists <- check_response(data, response_var)
  messages <- build_validation_messages(scan_result, response_exists, response_var)
  
  if (verbose) {
    if (messages$n_errors == 0 && messages$n_warnings == 0) {
      cat("  [OK] Data ready for analysis\n")
    } else {
      if (messages$n_warnings > 0) {
        cat("  [WARNING] Issues found:\n")
        for (w in messages$warnings) cat(sprintf("    - %s\n", w))
      }
      if (messages$n_errors > 0) {
        cat("  [ERROR] Critical issues found:\n")
        for (issue in messages$issues) cat(sprintf("    - %s\n", issue))
      }
    }
  }
  
  if (details) {
    return(list(
      ready = messages$n_errors == 0,
      issues = messages$issues,
      warnings = messages$warnings
    ))
  }
  
  messages$n_errors == 0
}
