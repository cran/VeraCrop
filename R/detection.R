# ============================================================
# File: R/preprocessing/detection.R
# VeraCrop: Variable Type Detection
# ============================================================

# ============================================================
# Detection Order:
#   1. logical -> binary
#   2. character/factor -> categorical detection
#   3. Date/POSIX -> date
#   4. numeric/integer64 -> numeric detection
#   5. other -> unknown
# ============================================================
#
# Categorical Detection Priority:
#   ordered factor -> ordinal
#   ordinal by name (with explicit level order supplied) -> ordinal
#   ordinal by name (no explicit level order supplied) -> nominal (with warning)
#   binary (n_unique == 2) -> binary
#   nominal (n_unique <= max_categorical_levels) -> nominal
#   high-cardinality (n_unique > max_categorical_levels) -> nominal_high
# ============================================================
#
# Numeric Detection Priority:
#   binary (n_unique == 2) -> binary
#   ordinal by name -> ordinal (natural numeric order; no ambiguity)
#   ordinal (if treat_numeric_as_ordinal & sequential) -> ordinal
#   nominal -> nominal
#   continuous -> continuous
# ============================================================
#
# Levels Policy (consistent across all types):
#   - Ordered factor: observed levels only, preserving factor's defined order
#   - Character/factor ordinal (name-based, explicit order given): the
#     explicit order supplied by the caller
#   - Character/factor nominal: observed unique values, sorted alphabetically
#     (deterministic; does NOT depend on row order in the input data)
#   - High-cardinality (character/factor): observed unique values, sorted
#     alphabetically (deterministic, consistent with nominal policy)
#   - Numeric (nominal/ordinal/binary): sorted unique values
#
#   Rationale: relying on "order of first appearance" makes dummy coding and
#   reference-level selection depend on row order in the raw data, which is
#   not reproducible across re-imports, re-orderings, or subsets of the same
#   underlying dataset. All levels are therefore deterministic functions of
#   the *set* of observed values, never of their order of occurrence.
# ============================================================

# ------------------------------------------------------------
# NOTE ON DEPENDENCIES:
# `%||%` is defined below for standalone use of this file. If the package
# already defines this operator in a shared file (e.g. utils.R), remove the
# definition below to avoid a duplicate-definition NOTE from R CMD check,
# and ensure Collate order in DESCRIPTION loads utils.R first.
# ------------------------------------------------------------

#' Default value operator
#' @noRd
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) lhs else rhs
}

# Default keywords for ordinal detection
.ORDINAL_KEYWORDS <- c("score", "level", "severity", "grade", "rank", "stage", "phase")

#' Internal helper for verbose output
#'
#' @param verbose Logical, print message if TRUE
#' @param type Character, detection type (e.g., "BINARY", "ORDINAL")
#' @param var Character, variable name
#' @param extra Optional extra information
#' @noRd
print_detection <- function(verbose, type, var, extra = NULL) {
  if (!verbose) return(invisible())
  if (is.null(extra)) {
    cat(sprintf("  [%s] %s\n", type, var))
  } else {
    cat(sprintf("  [%s] %s %s\n", type, var, extra))
  }
}

#' Normalize variable name for keyword detection
#'
#' Converts snake_case, camelCase, dot.case, kebab-case to underscore format.
#'
#' @param var_name Character, variable name to normalize
#'
#' @return Normalized variable name in lowercase with underscores
#' @noRd
normalize_variable_name <- function(var_name) {
  # 1. Split camelCase: "SoilSeverity" -> "Soil_Severity"
  var_name <- gsub("([a-z])([A-Z])", "\\1_\\2", var_name)

  # 2. Handle digits: "Grade2" -> "Grade_2"
  var_name <- gsub("([[:alpha:]])([[:digit:]])", "\\1_\\2", var_name)
  var_name <- gsub("([[:digit:]])([[:alpha:]])", "\\1_\\2", var_name)

  # 3. Spaces, dots, dashes -> underscores
  var_name <- gsub("[[:space:].-]", "_", var_name)

  # 4. Remove duplicate underscores
  var_name <- gsub("_+", "_", var_name)

  # 5. Remove leading/trailing underscores
  var_name <- gsub("^_|_$", "", var_name)

  # 6. Convert to lowercase
  var_name <- tolower(var_name)

  return(var_name)
}

#' Auto-detect ordinal variables from variable names
#'
#' This only decides WHETHER a variable name suggests an ordinal scale.
#' It never decides the level ORDER - that is a separate, more sensitive
#' decision handled by the caller (see detect_categorical_type()), because
#' guessing an order from name alone can silently reverse the true
#' ordinal relationship.
#'
#' @param var_name Character, variable name
#' @param score_keywords Character vector of keywords indicating ordinal variables
#' @param mode Character, "strict" (exact match on token) or "contains" (substring match)
#'
#' @return Logical, TRUE if variable name suggests an ordinal scale
#' @noRd
auto_detect_ordinal_from_name <- function(var_name,
                                          score_keywords = .ORDINAL_KEYWORDS,
                                          mode = "strict") {

  normalized <- normalize_variable_name(var_name)

  if (mode == "strict") {
    # Exact match on tokens
    parts <- strsplit(normalized, "_")[[1]]
    for (keyword in score_keywords) {
      if (keyword %in% parts) {
        return(TRUE)
      }
    }
  } else {
    # "contains" mode: substring match
    # Note: this may match substrings within larger words (e.g. "grade" in
    # "upgrade"); use "strict" mode to avoid false positives.
    for (keyword in score_keywords) {
      if (grepl(keyword, normalized, fixed = TRUE)) {
        return(TRUE)
      }
    }
  }

  return(FALSE)
}

#' Create metadata for variable type
#'
#' This is the single source of truth for the metadata structure.
#' All downstream functions should rely on this structure.
#'
#' CAUTION: this function uses modifyList() internally. In base R,
#' modifyList(x, list(key = NULL)) REMOVES `key` from `x` entirely rather
#' than setting it to NULL. None of the make_*_metadata() helpers below
#' currently pass NULL for any field (missing reference values use
#' NA_character_, not NULL), so this is currently safe - but if you add a
#' new field that may legitimately be NULL, do NOT rely on modifyList() to
#' preserve it; set it directly on the returned list instead.
#'
#' @param base_meta List with base metadata fields
#' @param ... Additional fields to add
#'
#' @return List with complete metadata
#' @noRd
make_metadata <- function(base_meta = list(), ...) {

  default_meta <- list(
    type = NA_character_,
    n_unique = NA_integer_,
    missing = NA_integer_,
    storage = NA_character_,
    class = NA_character_,
    auto_ordinal = FALSE,
    is_discrete = NA,
    is_integer = NA,
    needs_grouping = FALSE,
    grouping_method = NA_character_,
    reference = NA_character_,
    levels = NULL,
    min_val = NA,
    max_val = NA,
    skewness = NA_real_
  )

  result <- utils::modifyList(default_meta, base_meta)

  extra <- list(...)
  result <- utils::modifyList(result, extra)

  return(result)
}

#' Helper to create binary metadata
#' @noRd
make_binary_metadata <- function(n_unique, missing, storage, class,
                                  levels, is_integer = TRUE) {
  make_metadata(
    list(
      type = "binary",
      n_unique = n_unique,
      missing = missing,
      storage = storage,
      class = class,
      is_discrete = TRUE,
      is_integer = is_integer
    ),
    levels = levels
  )
}

#' Helper to create ordinal metadata
#' @noRd
make_ordinal_metadata <- function(n_unique, missing, storage, class,
                                  levels, is_integer = TRUE, auto_ordinal = FALSE) {
  make_metadata(
    list(
      type = "ordinal",
      n_unique = n_unique,
      missing = missing,
      storage = storage,
      class = class,
      auto_ordinal = auto_ordinal,
      is_discrete = TRUE,
      is_integer = is_integer
    ),
    levels = levels
  )
}

#' Helper to create nominal metadata
#' @noRd
make_nominal_metadata <- function(n_unique, missing, storage, class,
                                  levels, reference, is_integer = TRUE) {
  make_metadata(
    list(
      type = "nominal",
      n_unique = n_unique,
      missing = missing,
      storage = storage,
      class = class,
      auto_ordinal = FALSE,
      is_discrete = TRUE,
      is_integer = is_integer
    ),
    levels = levels,
    reference = reference
  )
}

#' Helper to create high-cardinality metadata
#' @noRd
make_high_cardinality_metadata <- function(n_unique, missing, storage, class,
                                           levels, is_integer = TRUE) {
  make_metadata(
    list(
      type = "nominal_high",
      n_unique = n_unique,
      missing = missing,
      storage = storage,
      class = class,
      auto_ordinal = FALSE,
      is_discrete = TRUE,
      is_integer = is_integer,
      needs_grouping = TRUE,
      grouping_method = NA_character_
    ),
    levels = levels
  )
}

#' Helper to create continuous metadata
#' @noRd
make_continuous_metadata <- function(n_unique, missing, storage, class,
                                     skewness, is_integer, min_val, max_val) {
  make_metadata(
    list(
      type = "continuous",
      n_unique = n_unique,
      missing = missing,
      storage = storage,
      class = class,
      auto_ordinal = FALSE,
      is_discrete = FALSE,
      is_integer = is_integer,
      skewness = skewness
    ),
    min_val = min_val,
    max_val = max_val
  )
}

#' Helper to create date metadata
#' @noRd
make_date_metadata <- function(n_unique, missing, storage, class,
                               min_val, max_val) {
  make_metadata(
    list(
      type = "date",
      n_unique = n_unique,
      missing = missing,
      storage = storage,
      class = class,
      is_discrete = FALSE,
      is_integer = FALSE
    ),
    min_val = min_val,
    max_val = max_val
  )
}

#' Helper to create unknown metadata
#' @noRd
make_unknown_metadata <- function(n_unique, missing, storage, class) {
  make_metadata(
    list(
      type = "unknown",
      n_unique = n_unique,
      missing = missing,
      storage = storage,
      class = class,
      is_discrete = NA,
      is_integer = NA
    )
  )
}

#' Get range of values (min/max) as numeric
#'
#' @param x_clean Numeric vector with NAs removed
#'
#' @return List with min_val and max_val
#' @noRd
get_range <- function(x_clean) {
  if (length(x_clean) > 0) {
    list(
      min_val = min(x_clean, na.rm = TRUE),
      max_val = max(x_clean, na.rm = TRUE)
    )
  } else {
    list(
      min_val = NA_real_,
      max_val = NA_real_
    )
  }
}

#' Check if values are sequential with step 1
#'
#' @param vals Numeric vector (assumed sorted)
#' @param tol Numeric, tolerance for floating point comparison (default: 1e-8)
#'
#' @return Logical, TRUE if values are sequential
#' @noRd
is_sequential <- function(vals, tol = 1e-8) {
  if (length(vals) < 2) return(FALSE)
  all(abs(diff(vals) - 1) < tol)
}

#' Get reference level based on method
#'
#' Always returns a character value for consistent metadata. This value is
#' intended for metadata / reporting purposes; the actual reference level
#' used during encoding is re-derived deterministically from `levels`
#' (see encoding.R), so this function only needs to be internally
#' deterministic, not the sole source of truth for encoding.
#'
#' @param x_clean Vector of cleaned values (full data, not just unique)
#' @param method Character, "mode", "first", or "alphabetical"
#'
#' @return Reference value as character
#' @noRd
get_reference <- function(x_clean, method = "mode") {
  if (length(x_clean) == 0) return(NA_character_)

  if (method == "mode") {
    # Use droplevels() for factors to ignore unused levels
    if (is.factor(x_clean)) {
      x_clean <- droplevels(x_clean)
    }
    freq <- table(x_clean)
    max_freq <- max(freq)
    candidates <- names(freq[freq == max_freq])
    # Break ties alphabetically for determinism
    return(sort(candidates)[1])
  } else if (method == "first") {
    return(as.character(x_clean[1]))
  } else {
    # alphabetical
    vals <- as.character(unique(x_clean))
    return(sort(vals)[1])
  }
}

#' Check if values are integer-like
#'
#' @param x_clean Numeric vector with NAs removed
#' @param tolerance Numeric, tolerance for comparison
#'
#' @return Logical, TRUE if all values are integer-like; FALSE for empty vectors
#' @noRd
is_integer_like <- function(x_clean, tolerance = 1e-8) {
  if (length(x_clean) == 0) return(FALSE)
  all(abs(x_clean - round(x_clean)) < tolerance)
}

#' Collect detection warnings
#'
#' @param result Detection result from categorical/numeric detection
#' @param var Variable name
#' @param ordinal_warnings Current ordinal warnings vector
#' @param high_cardinality_warnings Current high-cardinality warnings vector
#' @param n_unique Number of unique values
#'
#' @return List with updated warning vectors
#' @noRd
collect_detection_warnings <- function(result,
                                       var,
                                       ordinal_warnings,
                                       high_cardinality_warnings,
                                       n_unique) {

  if (isTRUE(result$auto_ordinal)) {
    ordinal_warnings <- c(ordinal_warnings, var)
  }

  if (isTRUE(result$needs_grouping)) {
    high_cardinality_warnings <- c(
      high_cardinality_warnings,
      sprintf("%s (%d levels)", var, n_unique)
    )
  }

  list(
    ordinal = ordinal_warnings,
    high = high_cardinality_warnings
  )
}

#' Get observed levels for categorical data (deterministic)
#'
#' For factors, returns only levels that appear in the data, preserving the
#' factor's own defined level order (this is intentional: an ordered or
#' nominal factor's `levels()` attribute already encodes a deliberate
#' choice made when the factor was created, and that choice should be
#' respected rather than overridden).
#'
#' For character vectors, returns unique values sorted alphabetically.
#' This is deterministic and does NOT depend on the order in which values
#' first appear in the rows of the input data, which is essential for
#' reproducibility across re-imports or re-orderings of the same dataset.
#'
#' @param x Factor or character vector
#'
#' @return Character vector of observed levels
#' @noRd
get_observed_levels <- function(x) {
  if (is.factor(x)) {
    obs <- unique(x[!is.na(x)])
    levels(x)[levels(x) %in% obs]
  } else {
    sort(unique(x[!is.na(x)]))
  }
}

#' Extract unique values and count once
#'
#' @param x Vector
#'
#' @return List with unique values and count
#' @noRd
get_unique_info <- function(x) {
  unique_vals <- unique(x[!is.na(x)])
  list(
    values = unique_vals,
    count = length(unique_vals)
  )
}

#' Detect variable types
#'
#' @param data A data frame
#' @param response_var Name of response variable
#' @param max_categorical_levels Maximum levels for categorical (default: 10)
#' @param treat_numeric_as_ordinal Logical, treat numeric with small levels as
#'   ordinal (default: FALSE)
#' @param auto_detect_ordinal_from_name Logical, detect ordinal from variable
#'   names (default: TRUE)
#' @param auto_detect_ordinal_mode Character, "strict" or "contains" (default: "strict")
#' @param ordinal_level_orders Named list mapping variable names to an explicit,
#'   correctly-ordered character vector of levels (low to high), e.g.
#'   \code{list(severity_level = c("Low", "Medium", "High"))}. Required to
#'   treat a CHARACTER or FACTOR variable as ordinal based on its name alone;
#'   without an explicit order, such variables are conservatively classified
#'   as nominal instead of guessing an order from row order in the data
#'   (which is not reproducible and can silently reverse the true ordinal
#'   relationship). Not needed for already-ordered factors, and not needed
#'   for numeric variables (numeric ordinal order is unambiguous - it is the
#'   natural numeric order).
#' @param reference_method Character, "mode", "first", or "alphabetical" (default: "mode")
#' @param verbose Logical
#'
#' @return List with variable type information
#' @examples
#' df <- data.frame(
#'   Yield = rnorm(30, 5000, 500),
#'   Nitrogen = rnorm(30, 120, 20),
#'   Region = sample(c("North", "South", "East"), 30, replace = TRUE)
#' )
#' types <- detect_variable_types(df, response_var = "Yield", verbose = FALSE)
#' types$Region$type
#'
#' @export
detect_variable_types <- function(data,
                                  response_var = "Yield",
                                  max_categorical_levels = 10,
                                  treat_numeric_as_ordinal = FALSE,
                                  auto_detect_ordinal_from_name = TRUE,
                                  auto_detect_ordinal_mode = "strict",
                                  ordinal_level_orders = list(),
                                  reference_method = "mode",
                                  verbose = TRUE) {

  # Validate arguments
  auto_detect_ordinal_mode <- match.arg(
    auto_detect_ordinal_mode,
    c("strict", "contains")
  )

  reference_method <- match.arg(
    reference_method,
    c("mode", "first", "alphabetical")
  )

  if (!is.data.frame(data)) stop("data must be a data frame")

  if (!response_var %in% names(data)) {
    stop(sprintf("Response variable \"%s\" not found in data", response_var))
  }

  if (!is.list(ordinal_level_orders)) {
    stop("'ordinal_level_orders' must be a named list")
  }

  predictors <- names(data)[names(data) != response_var]
  var_types <- list()
  ordinal_warnings <- character()
  downgraded_ordinal_warnings <- character()
  high_cardinality_warnings <- character()
  integer64_vars <- character()

  for (var in predictors) {
    x <- data[[var]]
    x_clean <- x[!is.na(x)]

    # Get unique info once
    unique_info <- get_unique_info(x)
    unique_vals <- unique_info$values
    n_unique <- unique_info$count

    n_missing <- sum(is.na(x))
    storage <- typeof(x)
    class_name <- class(x)[1]

    # --- Logical -> binary ---
    if (is.logical(x)) {
      var_types[[var]] <- make_binary_metadata(
        n_unique = n_unique,
        missing = n_missing,
        storage = storage,
        class = class_name,
        levels = sort(unique_vals)
      )
      print_detection(verbose, "BINARY", var)
      next
    }

    # --- Character/Factor ---
    if (is.character(x) || is.factor(x)) {
      detect_out <- detect_categorical_type(
        x = x,
        x_clean = x_clean,
        unique_vals = unique_vals,
        var = var,
        n_unique = n_unique,
        n_missing = n_missing,
        storage = storage,
        class = class_name,
        max_categorical_levels = max_categorical_levels,
        auto_detect_ordinal = auto_detect_ordinal_from_name,
        auto_detect_ordinal_mode = auto_detect_ordinal_mode,
        ordinal_level_orders = ordinal_level_orders,
        reference_method = reference_method,
        verbose = verbose
      )
      var_types[[var]] <- detect_out$result

      if (isTRUE(detect_out$downgraded_from_ordinal)) {
        downgraded_ordinal_warnings <- c(downgraded_ordinal_warnings, var)
      }

      warnings_acc <- collect_detection_warnings(
        detect_out$result, var, ordinal_warnings, high_cardinality_warnings, n_unique
      )
      ordinal_warnings <- warnings_acc$ordinal
      high_cardinality_warnings <- warnings_acc$high
      next
    }

    # --- Date/Time - MUST be checked BEFORE numeric ---
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      if (length(x_clean) > 0) {
        min_val <- min(x_clean, na.rm = TRUE)
        max_val <- max(x_clean, na.rm = TRUE)
      } else {
        min_val <- NA
        max_val <- NA
      }

      date_storage <- class(x)[1]

      var_types[[var]] <- make_date_metadata(
        n_unique = n_unique,
        missing = n_missing,
        storage = date_storage,
        class = class_name,
        min_val = min_val,
        max_val = max_val
      )
      print_detection(verbose, "DATE", var)
      next
    }

    # --- Numeric (including integer64) ---
    if (is.numeric(x) || inherits(x, "integer64")) {
      if (inherits(x, "integer64")) {
        integer64_vars <- c(integer64_vars, var)
        x <- as.numeric(x)
        x_clean <- x[!is.na(x)]
        unique_info <- get_unique_info(x)
        unique_vals <- unique_info$values
        n_unique <- unique_info$count
        storage <- "integer64"
        class_name <- "integer64"
      }

      result <- detect_numeric_type(
        x = x,
        x_clean = x_clean,
        unique_vals = unique_vals,
        var = var,
        n_unique = n_unique,
        n_missing = n_missing,
        storage = storage,
        class = class_name,
        max_categorical_levels = max_categorical_levels,
        treat_numeric_as_ordinal = treat_numeric_as_ordinal,
        auto_detect_ordinal = auto_detect_ordinal_from_name,
        auto_detect_ordinal_mode = auto_detect_ordinal_mode,
        reference_method = reference_method,
        verbose = verbose
      )
      var_types[[var]] <- result

      warnings_acc <- collect_detection_warnings(
        result, var, ordinal_warnings, high_cardinality_warnings, n_unique
      )
      ordinal_warnings <- warnings_acc$ordinal
      high_cardinality_warnings <- warnings_acc$high
      next
    }

    # --- Anything else ---
    var_types[[var]] <- make_unknown_metadata(
      n_unique = n_unique,
      missing = n_missing,
      storage = storage,
      class = class_name
    )
    print_detection(verbose, "UNKNOWN", var)
  }

  # Show all warnings at once (always shown, independent of `verbose`)
  if (length(ordinal_warnings) > 0) {
    warning(sprintf(
      "Variables auto-detected as ordinal based on name: %s.\nPlease verify if they are truly ordinal.",
      paste(unique(ordinal_warnings), collapse = ", ")
    ), call. = FALSE)
  }

  if (length(downgraded_ordinal_warnings) > 0) {
    warning(sprintf(
      "Variables whose name suggests an ordinal scale were classified as NOMINAL instead, because no explicit level order was supplied via 'ordinal_level_orders': %s.\nIf these are truly ordinal, re-run with e.g. ordinal_level_orders = list(%s = c(\"Low\", \"Medium\", \"High\")) using the correct low-to-high order.",
      paste(unique(downgraded_ordinal_warnings), collapse = ", "),
      unique(downgraded_ordinal_warnings)[1]
    ), call. = FALSE)
  }

  if (length(high_cardinality_warnings) > 0) {
    warning(sprintf(
      "Variables with high cardinality (using level grouping):\n  %s",
      paste(unique(high_cardinality_warnings), collapse = "\n  ")
    ), call. = FALSE)
  }

  if (length(integer64_vars) > 0) {
    warning(sprintf(
      "integer64 variables converted to numeric (precision may be lost): %s",
      paste(integer64_vars, collapse = ", ")
    ), call. = FALSE)
  }

  if (verbose) {
    type_counts <- table(vapply(
      var_types,
      function(x) x$type %||% "unknown",
      character(1)
    ))
    cat("\nVariable type detection complete:\n")
    for (type in names(type_counts)) {
      cat(sprintf("  %s: %d\n", type, type_counts[type]))
    }
  }

  return(var_types)
}

#' Detect categorical variable type
#'
#' IMPORTANT: for name-based ordinal auto-detection on CHARACTER/FACTOR
#' variables, an explicit level order MUST be supplied via
#' `ordinal_level_orders`. Without it, the level order cannot be inferred
#' safely (row order of first appearance is not a reliable proxy for rank),
#' so the variable is classified as NOMINAL instead, with a warning raised
#' by the caller. This avoids silently reversing or scrambling the true
#' ordinal relationship.
#'
#' @return A list with:
#'   \item{result}{The metadata list for this variable}
#'   \item{downgraded_from_ordinal}{Logical; TRUE if name-based ordinal
#'     detection fired but no explicit level order was available, so the
#'     variable was classified as nominal instead}
#' @noRd
detect_categorical_type <- function(x, x_clean, unique_vals, var, n_unique, n_missing,
                                    storage, class, max_categorical_levels,
                                    auto_detect_ordinal, auto_detect_ordinal_mode,
                                    ordinal_level_orders, reference_method, verbose) {

  # --- Explicitly ordered factor -> ordinal (order is already well-defined) ---
  if (is.factor(x) && is.ordered(x)) {
    print_detection(verbose, "ORDINAL", var, "(explicitly ordered)")
    return(list(
      result = make_ordinal_metadata(
        n_unique = n_unique,
        missing = n_missing,
        storage = storage,
        class = class,
        levels = get_observed_levels(x)
      ),
      downgraded_from_ordinal = FALSE
    ))
  }

  # --- Auto-detect ordinal from name ---
  if (auto_detect_ordinal && auto_detect_ordinal_from_name(var, mode = auto_detect_ordinal_mode)) {

    explicit_order <- ordinal_level_orders[[var]]
    observed <- get_observed_levels(x)

    if (!is.null(explicit_order) && all(observed %in% explicit_order)) {
      # Caller supplied a validated, correct low-to-high order: use it.
      # Keep only levels that are actually observed, preserving the
      # supplied order.
      ordered_levels <- explicit_order[explicit_order %in% observed]

      print_detection(verbose, "ORDINAL", var, "(auto-detected from name, explicit order supplied)")
      return(list(
        result = make_ordinal_metadata(
          n_unique = n_unique,
          missing = n_missing,
          storage = storage,
          class = class,
          levels = ordered_levels,
          auto_ordinal = TRUE
        ),
        downgraded_from_ordinal = FALSE
      ))
    }

    # No explicit order supplied (or it did not cover all observed levels):
    # do NOT guess an order from row order in the data. Classify as nominal
    # instead, using the same deterministic policy as other nominal
    # variables. The caller raises a warning explaining this downgrade.
    ref <- get_reference(x_clean, method = reference_method)

    print_detection(verbose, "NOMINAL", var,
                     "(name suggests ordinal, but no explicit level order supplied - see warnings)")
    return(list(
      result = make_nominal_metadata(
        n_unique = n_unique,
        missing = n_missing,
        storage = storage,
        class = class,
        levels = observed,
        reference = ref
      ),
      downgraded_from_ordinal = TRUE
    ))
  }

  # --- Binary (any variable with exactly 2 unique values) ---
  if (n_unique == 2) {
    print_detection(verbose, "BINARY", var)
    return(list(
      result = make_binary_metadata(
        n_unique = n_unique,
        missing = n_missing,
        storage = storage,
        class = class,
        levels = get_observed_levels(x)
      ),
      downgraded_from_ordinal = FALSE
    ))
  }

  # --- Nominal (categorical) ---
  if (n_unique <= max_categorical_levels) {
    levels_observed <- get_observed_levels(x)
    ref <- get_reference(x_clean, method = reference_method)

    print_detection(verbose, "NOMINAL", var, sprintf("(%d levels)", n_unique))
    return(list(
      result = make_nominal_metadata(
        n_unique = n_unique,
        missing = n_missing,
        storage = storage,
        class = class,
        levels = levels_observed,
        reference = ref
      ),
      downgraded_from_ordinal = FALSE
    ))
  }

  # --- High cardinality ---
  # Levels are sorted alphabetically for determinism, consistent with the
  # nominal policy above (see file header "Levels Policy").
  # grouping_method is set later by the preprocessing/encoding step.
  levels_sorted <- sort(as.character(unique_vals))

  print_detection(verbose, "HIGH-CARD", var, sprintf("(%d levels)", n_unique))
  return(list(
    result = make_high_cardinality_metadata(
      n_unique = n_unique,
      missing = n_missing,
      storage = storage,
      class = class,
      levels = levels_sorted
    ),
    downgraded_from_ordinal = FALSE
  ))
}

#' Detect numeric variable type
#'
#' Note: any numeric variable with exactly 2 unique values is treated as
#' binary, regardless of the actual values (e.g. 1/1000 or 0/999).
#'
#' Unlike character/factor variables, numeric ordinal order is never
#' ambiguous: it is simply the natural numeric order of the sorted unique
#' values, so no explicit level order needs to be supplied here.
#'
#' @noRd
detect_numeric_type <- function(x, x_clean, unique_vals, var, n_unique, n_missing,
                                storage, class, max_categorical_levels,
                                treat_numeric_as_ordinal, auto_detect_ordinal,
                                auto_detect_ordinal_mode, reference_method,
                                verbose) {

  tolerance <- 1e-8

  range_vals <- get_range(x_clean)
  is_integer <- is_integer_like(x_clean, tolerance)
  all_integer <- is_integer

  # --- Binary numeric (any 2 unique values) ---
  if (n_unique == 2) {
    print_detection(verbose, "BINARY", var, "(numeric, 2 levels)")
    return(make_binary_metadata(
      n_unique = n_unique,
      missing = n_missing,
      storage = storage,
      class = class,
      levels = sort(unique_vals),
      is_integer = is_integer
    ))
  }

  # --- Numeric with small number of unique values (categorical) ---
  if (n_unique <= max_categorical_levels && all_integer) {
    sorted_vals <- sort(unique_vals)

    # Auto-detect ordinal from name: safe for numeric, since the sorted
    # numeric order IS the natural rank order (no guessing involved).
    if (auto_detect_ordinal && auto_detect_ordinal_from_name(var, mode = auto_detect_ordinal_mode)) {
      print_detection(verbose, "ORDINAL?", var, "(auto-detected from name)")
      return(make_ordinal_metadata(
        n_unique = n_unique,
        missing = n_missing,
        storage = storage,
        class = class,
        levels = sorted_vals,
        is_integer = is_integer,
        auto_ordinal = TRUE
      ))
    }

    # Treat as ordinal if requested and levels are sequential (step = 1)
    if (treat_numeric_as_ordinal) {
      if (length(sorted_vals) > 1 && is_sequential(sorted_vals, tol = tolerance)) {
        print_detection(verbose, "ORDINAL", var, "(numeric, sequential)")
        return(make_ordinal_metadata(
          n_unique = n_unique,
          missing = n_missing,
          storage = storage,
          class = class,
          levels = sorted_vals,
          is_integer = is_integer
        ))
      } else {
        ref <- get_reference(x_clean, method = reference_method)
        print_detection(verbose, "NOMINAL", var, "(numeric, non-sequential)")
        return(make_nominal_metadata(
          n_unique = n_unique,
          missing = n_missing,
          storage = storage,
          class = class,
          levels = sorted_vals,
          reference = ref,
          is_integer = is_integer
        ))
      }
    }

    # Default: treat as nominal (safer default when scale is unknown)
    ref <- get_reference(x_clean, method = reference_method)
    print_detection(verbose, "NOMINAL", var, "(numeric, treated as categorical)")
    return(make_nominal_metadata(
      n_unique = n_unique,
      missing = n_missing,
      storage = storage,
      class = class,
      levels = sorted_vals,
      reference = ref,
      is_integer = is_integer
    ))
  }

  # --- Continuous ---
  # Minimum sample size for reliable skewness estimation (rule of thumb: n >= 8)
  # Consistent moment-based skewness: E[(X-mu)^3] / (E[(X-mu)^2])^(3/2)
  skewness <- NA_real_
  if (length(x_clean) >= 8) {
    centered <- x_clean - mean(x_clean)
    skewness <- tryCatch({
      m2 <- mean(centered^2, na.rm = TRUE)
      m3 <- mean(centered^3, na.rm = TRUE)
      if (m2 == 0) 0 else m3 / (m2^(3/2))
    }, error = function(e) NA_real_)
  }

  if (verbose) {
    if (is.na(skewness)) {
      cat(sprintf("  [CONTINUOUS] %s (skewness: not estimated, n<8)\n", var))
    } else {
      cat(sprintf("  [CONTINUOUS] %s (skewness: %.2f)\n", var, skewness))
    }
  }

  return(make_continuous_metadata(
    n_unique = n_unique,
    missing = n_missing,
    storage = storage,
    class = class,
    skewness = skewness,
    is_integer = is_integer,
    min_val = range_vals$min_val,
    max_val = range_vals$max_val
  ))
}
