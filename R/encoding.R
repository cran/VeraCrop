# ============================================================
# File: R/preprocessing/encoding.R
# VeraCrop: Variable Encoding for Statistical Models
# ============================================================

# ============================================================
# Internal Helpers - High Cardinality Grouping
# ============================================================

#' Generate a unique label that doesn't conflict with existing levels
#'
#' @noRd
generate_unique_label <- function(base_label, existing_levels) {
  label <- base_label
  counter <- 1
  while (label %in% existing_levels) {
    label <- paste0(base_label, "_", counter)
    counter <- counter + 1
  }
  label
}

#' Generate a stable unique identifier from a string without external dependencies
#'
#' Uses a rolling hash algorithm (31 * hash + byte) which is:
#' - Deterministic (same input always yields same output)
#' - Fast (O(n) with simple integer arithmetic)
#' - Safe (uses modulo to prevent overflow)
#' - No external dependencies
#'
#' @noRd
stable_hash <- function(x) {
  raw_vec <- charToRaw(as.character(x))
  h <- 0L
  for (b in raw_vec) {
    h <- (31L * h + as.integer(b)) %% 2147483647L
  }
  sprintf("lvl_%09d", h)
}

#' Clean level name for column naming
#'
#' Preserves ASCII alphanumeric characters and underscores only.
#' For non-ASCII characters, generates a readable placeholder
#' using a stable hash. This ensures consistent behavior across
#' different locales (especially Windows vs Linux).
#'
#' @noRd
clean_level_name <- function(lvl) {
  original_lvl <- as.character(lvl)

  # Use ASCII-only regex for cross-platform consistency
  cleaned <- gsub("[^A-Za-z0-9_]+", "_", original_lvl)
  cleaned <- gsub("^_+|_+$", "", cleaned)

  if (nchar(cleaned) > 0) {
    return(cleaned)
  }

  paste0("lvl_", stable_hash(original_lvl))
}

#' Group levels of high-cardinality categorical variables
#'
#' @param x Vector to group
#' @param min_freq Minimum frequency (proportion) for a level to be kept (default: 0.05)
#' @param min_count Minimum count for a level to be kept (default: 5)
#' @param other_label Label for grouped levels (default: "..OTHER..")
#' @param rule Character, "or" (keep if freq OR count) or "and" (keep if freq AND count)
#'
#' @return Factor with grouped levels
#'
#' @examples
#' x <- c(rep("A", 20), rep("B", 15), rep("C", 2), rep("D", 1))
#' grouped <- group_high_cardinality(x, min_freq = 0.05, min_count = 5)
#' table(grouped)
#'
#' @export
group_high_cardinality <- function(x,
                                   min_freq = 0.05,
                                   min_count = 5,
                                   other_label = "..OTHER..",
                                   rule = "or") {

  rule <- match.arg(rule, c("or", "and"))

  if (!is.character(x) && !is.factor(x)) {
    return(x)
  }

  if (all(is.na(x))) {
    warning("All values are NA. Returning original vector.")
    return(x)
  }

  # Drop unused levels for factors to get accurate frequency
  if (is.factor(x)) {
    x <- droplevels(x)
  }

  non_na_x <- x[!is.na(x)]
  if (length(non_na_x) == 0) {
    warning("No non-NA values found. Returning original vector.")
    return(x)
  }

  freq_table <- table(non_na_x)
  n_total <- sum(freq_table)

  if (rule == "or") {
    keep_levels <- names(freq_table)[
      (freq_table / n_total >= min_freq) | (freq_table >= min_count)
    ]
  } else {
    keep_levels <- names(freq_table)[
      (freq_table / n_total >= min_freq) & (freq_table >= min_count)
    ]
  }

  if (length(keep_levels) == 0) {
    keep_levels <- names(freq_table)[which.max(freq_table)]
    if (length(keep_levels) == 0) {
      warning("No valid levels found. Returning original vector.")
      return(x)
    }
    warning(sprintf("No levels meet frequency criteria. Keeping only \"%s\"", keep_levels[1]))
  }

  # Preserve original level order for factors
  if (is.factor(x)) {
    original_levels <- levels(x)
    keep_levels <- original_levels[original_levels %in% keep_levels]
  }

  all_original_levels <- unique(non_na_x)
  other_label <- generate_unique_label(other_label, all_original_levels)

  is_missing <- is.na(x)
  result <- as.character(x)
  result[!is_missing & !result %in% keep_levels] <- other_label
  result[is_missing] <- NA_character_

  all_levels <- unique(c(keep_levels, other_label))
  result <- factor(result, levels = all_levels)

  return(result)
}

#' Get mode (most frequent value) for reference level
#'
#' NOTE: intentionally named distinctly from missing.R's get_mode(), which
#' has a different signature (extra `ties` argument) and a different return
#' type (preserves the input class, e.g. factor). Package-wide, no two
#' top-level functions may share a name - whichever file is sourced last
#' would silently overwrite the other in the package namespace. This helper
#' always returns a plain character value, which is exactly what
#' resolve_reference_level() below needs.
#'
#' @noRd
.encoding_mode_level <- function(x) {
  if (length(x) == 0) return(NULL)

  if (is.factor(x)) {
    freq <- table(x)
    maxfreq <- max(freq)
    modes <- names(freq)[freq == maxfreq]
    return(modes[1])
  } else {
    x <- factor(x)
    freq <- table(x)
    maxfreq <- max(freq)
    modes <- names(freq)[freq == maxfreq]
    return(modes[1])
  }
}

#' Resolve the reference level to use for dummy encoding
#'
#' @noRd
resolve_reference_level <- function(info, x_no_na, levels_vec, var) {

  candidate <- info$reference

  if (!is.null(candidate) && !is.na(candidate) && candidate %in% levels_vec) {
    return(as.character(candidate))
  }

  candidate <- .encoding_mode_level(x_no_na)
  if (!is.null(candidate) && candidate %in% levels_vec) {
    return(as.character(candidate))
  }

  candidate <- levels_vec[1]
  warning(sprintf(
    "Could not resolve a valid reference level for variable \"%s\"; using first level '%s'.",
    var, candidate
  ), call. = FALSE)
  return(as.character(candidate))
}

#' Get top N levels by frequency (excluding specified levels)
#'
#' @noRd
get_top_levels <- function(x, n, exclude = NULL) {
  freq <- sort(table(x), decreasing = TRUE)

  if (!is.null(exclude)) {
    freq <- freq[!(names(freq) %in% exclude)]
  }

  if (length(freq) > 1) {
    # NOTE: order() with a vector `decreasing` argument (one value per sort
    # key) is ONLY supported by method = "radix" - R does not fall back to
    # it automatically, and omitting this raises:
    #   Error in order(...) : 'decreasing' of length > 1 is only for
    #   method = "radix"
    # This affects every nominal variable with more than one level, i.e.
    # essentially all real categorical data, so this must be explicit.
    freq <- freq[order(freq, names(freq), decreasing = c(TRUE, FALSE), method = "radix")]
  }

  names(utils::head(freq, n))
}

#' Create dummy variables from factor levels
#'
#' @noRd
create_dummy_variables <- function(x, levels_to_encode, var_name) {

  levels_to_encode <- unique(levels_to_encode)

  if (length(levels_to_encode) == 0) {
    return(list(
      data = list(),
      mapping = list(),
      cleaned_mapping = list()
    ))
  }

  x <- droplevels(x)

  col_names <- character(length(levels_to_encode))
  for (i in seq_along(levels_to_encode)) {
    lvl <- levels_to_encode[i]
    clean_lvl <- clean_level_name(lvl)
    col_names[i] <- make.names(paste(var_name, clean_lvl, sep = "_"), unique = TRUE)
  }

  if (any(duplicated(col_names))) {
    col_names <- make.names(col_names, unique = TRUE)
  }

  data <- list()
  mapping <- list()
  cleaned_mapping <- list()

  for (i in seq_along(levels_to_encode)) {
    lvl <- levels_to_encode[i]
    col_name <- col_names[i]
    clean_lvl <- clean_level_name(lvl)

    data[[col_name]] <- as.numeric(x == lvl)
    mapping[[col_name]] <- lvl
    cleaned_mapping[[col_name]] <- clean_lvl
  }

  list(
    data = data,
    mapping = mapping,
    cleaned_mapping = cleaned_mapping
  )
}

# ============================================================
# Core Factor Encoding (Shared Logic)
# ============================================================

#' Core factor encoding logic
#'
#' @noRd
encode_factor_levels <- function(x, var, info, levels_vec,
                                  max_levels, create_other = TRUE,
                                  original_levels = NULL, verbose = FALSE) {

  x <- factor(x, levels = levels_vec)

  ref_level <- resolve_reference_level(info, x[!is.na(x)], levels_vec, var)

  n_dummies <- max_levels - if (create_other) 1 else 0
  top_levels <- get_top_levels(x[!is.na(x)], n = n_dummies, exclude = ref_level)

  all_other_levels <- setdiff(levels_vec, c(ref_level, top_levels))
  has_other <- create_other && length(all_other_levels) > 0

  if (has_other) {
    # Use a safe label for "Other" that won't conflict
    other_label <- generate_unique_label("Other", c(levels_vec, ref_level, top_levels))
    other_dummy <- as.numeric(!x %in% c(ref_level, top_levels) & !is.na(x))
    other_name <- make.names(paste(var, other_label, sep = "_"), unique = TRUE)

    if (verbose) message(sprintf(
      "  %d levels grouped into '%s' dummy",
      length(all_other_levels), other_label
    ))
  }

  if (verbose && length(top_levels) < n_dummies) {
    message(sprintf("  only %d levels available for dummies", length(top_levels)))
  }

  dummy_result <- create_dummy_variables(x, top_levels, var)

  if (has_other) {
    dummy_result$data[[other_name]] <- other_dummy
    dummy_result$mapping[[other_name]] <- other_label
    dummy_result$cleaned_mapping[[other_name]] <- clean_level_name(other_label)
  }

  # Build info with separate reference (not in mapping)
  info_list <- list(
    original = var,
    encoded = names(dummy_result$data),
    type = if (is.null(original_levels)) "nominal" else "nominal_grouped",
    reference = ref_level,
    level_mapping = dummy_result$mapping,
    cleaned_level_mapping = dummy_result$cleaned_mapping,
    ignored_levels = all_other_levels,
    has_other = has_other
  )

  if (!is.null(original_levels)) {
    # For high-cardinality: track original levels
    grouped_levels <- levels_vec
    info_list$grouped_levels <- grouped_levels
    info_list$collapsed_levels <- setdiff(original_levels, grouped_levels)
    info_list$keep_levels <- intersect(original_levels, grouped_levels)
  }

  list(
    data = dummy_result$data,
    info = info_list
  )
}

# ============================================================
# Individual Encoders
# ============================================================

#' Encode binary variable
#'
#' Always maps TRUE -> 1 and FALSE -> 0 for consistency.
#'
#' @noRd
encode_binary <- function(x, var, info, verbose) {
  levels_vec <- info$levels

  if (is.null(levels_vec) || length(levels_vec) != 2) {
    stop(sprintf("Binary variable \"%s\" must have exactly 2 levels, got %d",
                 var, length(levels_vec)))
  }

  if (is.logical(x)) {
    encoded <- as.numeric(x)
    encoded_name <- var
    reference <- "FALSE"
    positive_level <- "TRUE"
  } else {
    x <- factor(x, levels = levels_vec)
    reference <- levels_vec[1]
    positive_level <- levels_vec[2]
    encoded <- as.numeric(x == positive_level)
    encoded_name <- var
  }

  if (verbose) message(sprintf("  [BINARY] %s -> 0/1", var))

  list(
    data = stats::setNames(list(encoded), encoded_name),
    info = list(
      original = var,
      encoded = encoded_name,
      type = "binary",
      reference = reference,
      positive_level = positive_level
    )
  )
}

#' Encode ordinal variable
#'
#' @noRd
encode_ordinal <- function(x, var, info, verbose) {
  levels_vec <- info$levels

  if (is.null(levels_vec) || length(levels_vec) < 2) {
    stop(sprintf("Ordinal variable \"%s\" must have at least 2 levels", var))
  }

  x <- factor(x, levels = levels_vec, ordered = TRUE)
  encoded <- as.numeric(x)
  encoded_name <- var

  if (verbose) message(sprintf("  [ORDINAL] %s -> numeric", var))

  list(
    data = stats::setNames(list(encoded), encoded_name),
    info = list(original = var, encoded = encoded_name, type = "ordinal")
  )
}

#' Encode nominal variable
#'
#' @noRd
encode_nominal <- function(x, var, info, max_dummy_levels, verbose) {
  levels_vec <- info$levels

  if (is.null(levels_vec) || length(levels_vec) < 2) {
    stop(sprintf("Nominal variable \"%s\" must have at least 2 levels", var))
  }

  result <- encode_factor_levels(
    x = x,
    var = var,
    info = info,
    levels_vec = levels_vec,
    max_levels = max_dummy_levels,
    create_other = TRUE,
    original_levels = NULL,
    verbose = verbose
  )

  if (verbose) {
    message(sprintf(
      "  [NOMINAL] %s -> %d dummies (ref: %s)",
      var, length(result$data), result$info$reference
    ))
  }

  return(result)
}

#' Encode high-cardinality variable
#'
#' @param rule Character, "or" or "and" for frequency grouping
#' @noRd
encode_high_cardinality <- function(x, var, info, min_freq, min_count,
                                     rule = "or", verbose) {

  rule <- match.arg(rule, c("or", "and"))

  if (verbose) message(sprintf(
    "  [HIGH] %s: grouping rare levels (freq: %.1f%%, count: %d, rule: %s)",
    var, min_freq * 100, min_count, rule
  ))

  grouped_x <- group_high_cardinality(x, min_freq = min_freq, min_count = min_count,
                                       rule = rule)
  n_unique <- length(levels(grouped_x))

  # Store original levels for tracking collapsed levels
  original_levels <- unique(x[!is.na(x)])

  # If constant, drop
  if (n_unique == 1) {
    if (verbose) message("    -> Variable became constant after grouping. Skipping.")
    return(NULL)
  }

  # If binary after grouping, use binary encoder directly
  if (n_unique == 2) {
    if (verbose) message(sprintf("    -> %d levels grouped into binary", n_unique))

    grouped_levels <- levels(grouped_x)
    ref_level <- resolve_reference_level(info, grouped_x[!is.na(grouped_x)], grouped_levels, var)
    positive_level <- setdiff(grouped_levels, ref_level)[1]

    encoded <- as.numeric(grouped_x == positive_level)
    encoded[is.na(x)] <- NA_integer_
    encoded_name <- var

    return(list(
      data = stats::setNames(list(encoded), encoded_name),
      info = list(
        original = var,
        encoded = encoded_name,
        type = "binary",
        reference = ref_level,
        positive_level = positive_level,
        grouped_levels = grouped_levels,
        collapsed_levels = setdiff(original_levels, grouped_levels),
        keep_levels = intersect(original_levels, grouped_levels),
        note = "Grouped high-cardinality variable (binary result)"
      )
    ))
  }

  # Use shared factor encoding for remaining cases (n_unique > 2)
  result <- encode_factor_levels(
    x = grouped_x,
    var = var,
    info = info,
    levels_vec = levels(grouped_x),
    max_levels = length(levels(grouped_x)),
    create_other = FALSE,
    original_levels = original_levels,
    verbose = verbose
  )

  # Preserve NAs
  is_missing <- is.na(x)
  for (col_name in names(result$data)) {
    result$data[[col_name]][is_missing] <- NA_integer_
  }

  if (verbose) {
    message(sprintf(
      "    -> %d levels -> %d dummies (ref: %s)",
      n_unique, length(result$data), result$info$reference
    ))
  }

  return(result)
}

#' Encode continuous variable
#'
#' @noRd
encode_continuous <- function(x, var, verbose) {
  if (verbose) message(sprintf("  [CONTINUOUS] %s (will be scaled)", var))

  list(
    data = stats::setNames(list(x), var),
    info = list(
      original = var,
      encoded = var,
      type = "continuous_to_scale",
      needs_scaling = TRUE
    )
  )
}

#' Encode date variable
#'
#' @noRd
encode_date <- function(x, var, verbose) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    if (all(is.na(x))) {
      if (verbose) message(sprintf("  [DATE] %s: all NA, returning as is", var))
      return(list(
        data = stats::setNames(list(x), var),
        info = list(original = var, encoded = var, type = "date",
                    note = "All values are NA")
      ))
    }

    min_date <- min(x, na.rm = TRUE)
    encoded_name <- make.names(paste0(var, "_days"), unique = TRUE)
    encoded <- as.numeric(difftime(x, min_date, units = "days"))

    if (verbose) message(sprintf("  [DATE] %s -> %s (days)", var, encoded_name))

    return(list(
      data = stats::setNames(list(encoded), encoded_name),
      info = list(
        original = var,
        encoded = encoded_name,
        type = "date",
        origin = min_date,
        units = "days"
      )
    ))
  }

  list(
    data = stats::setNames(list(x), var),
    info = list(original = var, encoded = var, type = "date")
  )
}

# ============================================================
# Main Exported Function
# ============================================================

#' Encode variables for statistical models
#'
#' @param data A data frame
#' @param var_types Variable types from detect_variable_types()
#' @param response_var Name of response variable
#' @param max_dummy_levels Maximum dummies per variable (default: 10)
#' @param min_freq_for_encoding Minimum frequency for grouping high-cardinality variables (default: 0.05)
#' @param min_count_for_encoding Minimum count for grouping high-cardinality variables (default: 5)
#' @param grouping_rule Character, "or" or "and" for high-cardinality grouping (default: "or")
#' @param verbose Logical
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Encoded data frame}
#'   \item{encoding_info}{Metadata describing each encoding}
#'   \item{dropped_vars}{Character vector of variables dropped during encoding}
#' }
#'
#' @examples
#' df <- data.frame(
#'   Yield = rnorm(30, 5000, 500),
#'   Region = sample(c("North", "South", "East"), 30, replace = TRUE)
#' )
#' types <- detect_variable_types(df, response_var = "Yield", verbose = FALSE)
#' result <- encode_variables(df, types, response_var = "Yield", verbose = FALSE)
#' names(result$data)
#'
#' @export
encode_variables <- function(data,
                             var_types,
                             response_var = "Yield",
                             max_dummy_levels = 10,
                             min_freq_for_encoding = 0.05,
                             min_count_for_encoding = 5,
                             grouping_rule = "or",
                             verbose = TRUE) {

  # ---- Input Validation ----
  if (!response_var %in% names(data)) {
    stop(sprintf("Response variable \"%s\" not found in data", response_var))
  }

  if (!is.numeric(max_dummy_levels) || length(max_dummy_levels) != 1 ||
      max_dummy_levels < 2 || max_dummy_levels != as.integer(max_dummy_levels)) {
    stop("max_dummy_levels must be a single integer >= 2")
  }

  if (!is.numeric(min_freq_for_encoding) || length(min_freq_for_encoding) != 1 ||
      min_freq_for_encoding <= 0 || min_freq_for_encoding >= 1) {
    stop("min_freq_for_encoding must be a single number between 0 and 1")
  }

  if (!is.numeric(min_count_for_encoding) || length(min_count_for_encoding) != 1 ||
      min_count_for_encoding < 1 || min_count_for_encoding != as.integer(min_count_for_encoding)) {
    stop("min_count_for_encoding must be a single integer >= 1")
  }

  grouping_rule <- match.arg(grouping_rule, c("or", "and"))

  if (!is.list(var_types) || length(var_types) == 0) {
    warning("var_types is empty or not a list. Returning data unchanged.")
    return(list(data = data, encoding_info = list(), dropped_vars = character()))
  }

  # ---- Encoder Dispatch Table ----
  encoders <- list(
    binary = encode_binary,
    ordinal = encode_ordinal,
    nominal = encode_nominal,
    nominal_high = encode_high_cardinality,
    continuous = encode_continuous,
    date = encode_date
  )

  # ---- Initialize ----
  encoded_data <- data.frame(row.names = rownames(data))
  encoding_info <- list()
  dropped_vars <- character()

  encoded_data[[response_var]] <- data[[response_var]]

  # ---- Encode Each Variable ----
  for (var in names(var_types)) {
    if (var == response_var) next

    if (!var %in% names(data)) {
      warning(sprintf("Variable \"%s\" not found in data. Skipping.", var))
      next
    }

    info <- var_types[[var]]

    if (is.null(info$type)) {
      warning(sprintf("Variable \"%s\" has no type information. Skipping.", var))
      next
    }

    type <- info$type
    x <- data[[var]]

    encoder <- encoders[[type]]
    if (is.null(encoder)) {
      warning(sprintf("Unknown type \"%s\" for variable \"%s\". Skipping.", type, var))
      next
    }

    args <- switch(type,
      binary = list(x = x, var = var, info = info, verbose = verbose),
      ordinal = list(x = x, var = var, info = info, verbose = verbose),
      nominal = list(x = x, var = var, info = info,
                     max_dummy_levels = max_dummy_levels, verbose = verbose),
      nominal_high = list(x = x, var = var, info = info,
                          min_freq = min_freq_for_encoding,
                          min_count = min_count_for_encoding,
                          rule = grouping_rule,
                          verbose = verbose),
      continuous = list(x = x, var = var, verbose = verbose),
      date = list(x = x, var = var, verbose = verbose),
      stop(sprintf("Unexpected type \"%s\" in switch", type))
    )

    result <- tryCatch({
      do.call(encoder, args)
    }, error = function(e) {
      warning(sprintf("Error encoding variable \"%s\": %s. Skipping.", var, e$message))
      return(NULL)
    })

    if (is.null(result)) {
      dropped_vars <- c(dropped_vars, var)
      next
    }

    if (is.null(result$data) || is.null(result$info)) {
      warning(sprintf("Encoding failed for variable \"%s\". Skipping.", var))
      dropped_vars <- c(dropped_vars, var)
      next
    }

    if (is.null(result$info$original) || is.null(result$info$encoded)) {
      warning(sprintf("Encoding info for variable \"%s\" missing required fields. Skipping.", var))
      dropped_vars <- c(dropped_vars, var)
      next
    }

    if (is.null(names(result$data))) {
      warning(sprintf("Encoding result for variable \"%s\" has no column names. Skipping.", var))
      dropped_vars <- c(dropped_vars, var)
      next
    }

    original_encoded_names <- result$info$encoded
    actual_col_names <- character()

    for (source_col_name in names(result$data)) {
      final_col_name <- source_col_name

      if (final_col_name %in% names(encoded_data)) {
        final_col_name <- make.names(
          c(names(encoded_data), source_col_name), unique = TRUE
        )[length(names(encoded_data)) + 1]
        if (verbose) {
          message(sprintf("  [INFO] Duplicate column name detected. Renamed to '%s'",
                          final_col_name))
        }
      }

      encoded_data[[final_col_name]] <- result$data[[source_col_name]]
      actual_col_names <- c(actual_col_names, final_col_name)
    }

    if (!identical(original_encoded_names, actual_col_names)) {
      result$info$encoded <- actual_col_names
      if (length(original_encoded_names) == 1 && length(actual_col_names) == 1) {
        result$info$renamed_from <- original_encoded_names
      }
    }

    encoding_info[[var]] <- result$info
  }

  if (verbose && length(dropped_vars) > 0) {
    message(sprintf("  [INFO] Dropped variables during encoding: %s",
                    paste(dropped_vars, collapse = ", ")))
  }

  list(
    data = encoded_data,
    encoding_info = encoding_info,
    dropped_vars = dropped_vars
  )
}
