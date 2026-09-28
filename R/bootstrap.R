#' Bootstrap Confidence Intervals for the CPA Yield Gap Decomposition
#'
#' Uses case resampling (nonparametric bootstrap) to estimate percentile
#' confidence intervals for the overall yield gap and for each significant
#' variable's contribution to it (\code{Gap_Component} and
#' \code{Share_Percent} in the CPA table). The set of significant variables
#' is held fixed across all resamples -- by default it is determined once,
#' from the original (non-resampled) data -- so that the reported intervals
#' describe the uncertainty of the CPA decomposition itself, not of variable
#' selection. Optionally, variable selection is also re-run independently on
#' each resample purely as a diagnostic, to report how often each fixed
#' variable would be re-selected on its own (\code{selection_stability}).
#'
#' @param data A data frame, already preprocessed (e.g. via
#'   \code{\link{prep_yield_gap}}) and ready for \code{\link{yield_gap_analysis}}.
#' @param response Character. Name of the response variable. Default: "Yield".
#' @param sig_vars Character vector of significant variable names to hold
#'   fixed across all bootstrap resamples. If \code{NULL} (default), this is
#'   determined once from the original data using \code{selection_method}
#'   (mirroring \code{\link{yield_gap_analysis}}'s default of
#'   \code{use_bonferroni = FALSE}). If you already have a
#'   \code{\link{yield_gap_analysis}} result, it is recommended to pass its
#'   variables explicitly here (e.g.
#'   \code{result$cpa_table$Variable[result$cpa_table$Variable != "Intercept"]})
#'   so the bootstrap exactly matches that analysis.
#' @param selection_method One of \code{"ftest"} or \code{"aic"}, used only
#'   when \code{sig_vars} is \code{NULL}, to select variables once from the
#'   original data. Default: \code{"ftest"}.
#' @param n_boot Integer. Number of bootstrap resamples. Default: 1000.
#' @param conf_level Numeric. Confidence level for the percentile interval.
#'   Default: 0.95.
#' @param top_percentile Numeric or \code{NULL}. Percentile used to define
#'   the high-yield subset, passed through to
#'   \code{\link{calculate_opt_values}}. If \code{NULL} (default), it is
#'   auto-tuned from the sample size using the same thresholds as
#'   \code{\link{yield_gap_analysis}} (0.99 for n >= 100, 0.95 for n >= 50,
#'   0.90 for n >= 30, 0.85 otherwise), so the default reproduces that
#'   function's behavior on the same data.
#' @param min_top_samples Integer or \code{NULL}. Minimum number of rows
#'   required in the high-yield subset before \code{\link{calculate_opt_values}}
#'   relaxes \code{top_percentile}. If \code{NULL} (default), it is
#'   auto-tuned from the sample size using the same thresholds as
#'   \code{\link{yield_gap_analysis}} (20 for n >= 100, 10 for n >= 50, 5 for
#'   n >= 30, 3 otherwise).
#' @param check_selection_stability Logical. If \code{TRUE}, also re-run
#'   variable selection independently on each bootstrap resample and report
#'   how often each fixed variable is re-selected. Default: \code{TRUE}.
#' @param seed Integer or \code{NULL}. Random seed for reproducibility; the
#'   caller's global random state is saved before resampling and restored
#'   afterward. Default: \code{NULL}.
#' @param verbose Logical. Print progress and a summary table.
#'   Default: \code{TRUE}.
#'
#' @return A list with components:
#'   \item{sig_vars}{The fixed variable set used across all resamples.}
#'   \item{summary}{A data frame with one row per quantity (overall yield
#'     mean/optimal/gap, plus each variable's Gap_Component and
#'     Share_Percent), giving the point estimate from the original data,
#'     the bootstrap mean and standard error, and the percentile
#'     confidence interval.}
#'   \item{boot_gap_component}{\code{n_boot} x \code{length(sig_vars)} matrix
#'     of bootstrap \code{Gap_Component} draws (one column per variable).}
#'   \item{boot_share_percent}{\code{n_boot} x \code{length(sig_vars)} matrix
#'     of bootstrap \code{Share_Percent} draws.}
#'   \item{boot_yield_mean, boot_yield_opt, boot_yield_gap}{Numeric vectors
#'     of length \code{n_boot} with the bootstrap draws of each overall
#'     quantity.}
#'   \item{selection_stability}{If requested, a data frame giving, for each
#'     fixed variable, how often it was independently re-selected across
#'     resamples (\code{Times_Selected}, \code{Selection_Rate}).}
#'   \item{n_boot, n_failed, conf_level}{Bookkeeping: resamples requested,
#'     resamples that failed and were skipped (e.g. due to a singular fit),
#'     and the confidence level used.}
#'
#' @examples
#' \donttest{
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' result <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' sig_vars <- result$cpa_table$Variable[result$cpa_table$Variable != "Intercept"]
#' boot <- bootstrap_cpa(prep$data, response = "Yield", sig_vars = sig_vars,
#'                        n_boot = 200, verbose = FALSE)
#' boot$summary
#' }
#'
#' @importFrom stats sd setNames
#' @export
bootstrap_cpa <- function(data,
                           response = "Yield",
                           sig_vars = NULL,
                           selection_method = c("ftest", "aic"),
                           n_boot = 1000,
                           conf_level = 0.95,
                           top_percentile = NULL,
                           min_top_samples = NULL,
                           check_selection_stability = TRUE,
                           seed = NULL,
                           verbose = TRUE) {

  selection_method <- match.arg(selection_method)

  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  if (!response %in% names(data)) {
    stop(sprintf("Response variable '%s' not found in data.", response))
  }

  n <- nrow(data)
  if (n < 20) {
    warning("Bootstrapping with fewer than 20 observations may give unstable results.")
  }

  # ---- Auto-tune top_percentile / min_top_samples from sample size, ----
  # ---- mirroring yield_gap_analysis()'s own defaults exactly, so that ----
  # ---- bootstrap_cpa()'s default behavior matches that function's on ----
  # ---- the same data unless the caller overrides these explicitly.   ----
  if (is.null(top_percentile)) {
    if (n >= 100) {
      top_percentile <- 0.99
    } else if (n >= 50) {
      top_percentile <- 0.95
    } else if (n >= 30) {
      top_percentile <- 0.90
    } else {
      top_percentile <- 0.85
    }
  }
  if (is.null(min_top_samples)) {
    if (n >= 100) {
      min_top_samples <- 20
    } else if (n >= 50) {
      min_top_samples <- 10
    } else if (n >= 30) {
      min_top_samples <- 5
    } else {
      min_top_samples <- 3
    }
  }

  # ---- Step 1: fixed variable set, from the ORIGINAL data if not supplied ----
  # NOTE: if you already have a yield_gap_analysis() result, prefer passing
  # its variables explicitly via sig_vars (e.g.
  # `result$cpa_table$Variable[result$cpa_table$Variable != "Intercept"]`) so
  # the bootstrap exactly matches that analysis. The automatic fallback below
  # mirrors yield_gap_analysis()'s own default (use_bonferroni = FALSE), but
  # it re-runs selection independently and is not guaranteed to reproduce a
  # specific prior call made with non-default arguments.
  if (is.null(sig_vars)) {
    if (verbose) cat("Selecting variables on the original data (", selection_method, ")...\n", sep = "")
    sel <- if (selection_method == "ftest") {
      select_vars_ftest(data, response = response, use_bonferroni = FALSE, verbose = FALSE)
    } else {
      select_vars_aic(data, response = response, verbose = FALSE)
    }
    sig_vars <- sel$significant_vars
  }

  if (length(sig_vars) == 0) {
    stop("No significant variables to bootstrap (sig_vars is empty).")
  }
  missing_vars <- setdiff(sig_vars, names(data))
  if (length(missing_vars) > 0) {
    stop("These sig_vars are not columns of 'data': ", paste(missing_vars, collapse = ", "))
  }

  if (verbose) {
    cat("Fixed variable set (", length(sig_vars), " variables): ",
        paste(sig_vars, collapse = ", "), "\n\n", sep = "")
    cat("Running", n_boot, "bootstrap resamples...\n")
  }

  # ---- Reproducibility: save & restore the caller's random state ----
  if (!is.null(seed)) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
      if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }

  formula_fixed <- stats::as.formula(paste(response, "~", paste(sig_vars, collapse = " + ")))

  boot_gap   <- matrix(NA_real_, nrow = n_boot, ncol = length(sig_vars), dimnames = list(NULL, sig_vars))
  boot_share <- matrix(NA_real_, nrow = n_boot, ncol = length(sig_vars), dimnames = list(NULL, sig_vars))
  boot_yield_gap  <- rep(NA_real_, n_boot)
  boot_yield_mean <- rep(NA_real_, n_boot)
  boot_yield_opt  <- rep(NA_real_, n_boot)

  selection_hits <- stats::setNames(rep(0L, length(sig_vars)), sig_vars)
  n_failed <- 0L
  pb_step <- max(1, floor(n_boot / 20))

  for (b in seq_len(n_boot)) {
    idx <- sample.int(n, n, replace = TRUE)
    d_boot <- data[idx, , drop = FALSE]

    boot_result <- tryCatch({
      m_boot <- stats::lm(formula_fixed, data = d_boot)
      coefs_boot <- stats::coef(m_boot)

      cutoff <- stats::quantile(d_boot[[response]], top_percentile, na.rm = TRUE)
      data_top <- d_boot[d_boot[[response]] >= cutoff, , drop = FALSE]

      opt <- suppressWarnings(calculate_opt_values(
        d_boot, sig_vars, coefs_boot, data_top,
        response_var = response, top_percentile = top_percentile,
        min_top_samples = min_top_samples
      ))
      # calculate_opt_values() warns every time it relaxes top_percentile for
      # a resample with too few high-yield rows; on small datasets this is
      # routine and expected on nearly every resample, so it is suppressed
      # here (unlike the one-time point-estimate computation below, where a
      # single relaxation notice is informative rather than noisy).

      cpa_in <- data.frame(
        Variable = c("Intercept", opt$var_table$Variable),
        Beta     = c(coefs_boot[["(Intercept)"]], coefs_boot[opt$var_table$Variable]),
        Mean     = c(1, opt$var_table$Mean),
        Opt      = c(1, opt$var_table$Opt),
        stringsAsFactors = FALSE
      )
      yg <- yield_gap_components(cpa_in)
      list(cpa_table = yg$cpa_table, yield_mean = yg$yield_mean,
           yield_opt = yg$yield_opt, yield_gap = yg$yield_gap)
    }, error = function(e) NULL)

    if (is.null(boot_result)) {
      n_failed <- n_failed + 1L
      next
    }

    ct <- boot_result$cpa_table
    for (v in sig_vars) {
      row_v <- ct[ct$Variable == v, , drop = FALSE]
      if (nrow(row_v) == 1) {
        boot_gap[b, v]   <- row_v$Gap_Component
        boot_share[b, v] <- row_v$Share_Percent
      }
    }
    boot_yield_gap[b]  <- boot_result$yield_gap
    boot_yield_mean[b] <- boot_result$yield_mean
    boot_yield_opt[b]  <- boot_result$yield_opt

    if (check_selection_stability) {
      sel_b <- tryCatch({
        if (selection_method == "ftest") {
          select_vars_ftest(d_boot, response = response, verbose = FALSE)$significant_vars
        } else {
          select_vars_aic(d_boot, response = response, verbose = FALSE)$significant_vars
        }
      }, error = function(e) character(0))
      hits <- sig_vars %in% sel_b
      selection_hits[sig_vars[hits]] <- selection_hits[sig_vars[hits]] + 1L
    }

    if (verbose && b %% pb_step == 0) {
      cat(sprintf("  %3d%% done (%d/%d)\r", round(100 * b / n_boot), b, n_boot))
      utils::flush.console()
    }
  }
  if (verbose) cat("\n")

  if (n_failed > 0 && verbose) {
    cat(sprintf("Note: %d of %d resamples failed and were skipped (%.1f%%).\n",
                n_failed, n_boot, 100 * n_failed / n_boot))
  }

  alpha <- 1 - conf_level
  probs <- c(alpha / 2, 1 - alpha / 2)

  summarize_vec <- function(x, label, estimate) {
    x <- x[!is.na(x)]
    ci <- if (length(x) >= 2) stats::quantile(x, probs, na.rm = TRUE) else c(NA, NA)
    data.frame(
      Quantity  = label,
      Estimate  = estimate,
      Boot_Mean = if (length(x)) mean(x) else NA_real_,
      Boot_SE   = if (length(x)) stats::sd(x) else NA_real_,
      CI_Lower  = unname(ci[1]),
      CI_Upper  = unname(ci[2]),
      N_Valid   = length(x),
      stringsAsFactors = FALSE
    )
  }

  # Point estimate: a single fit on the full (original) data with the fixed variable set
  m_orig <- stats::lm(formula_fixed, data = data)
  coefs_orig <- stats::coef(m_orig)
  cutoff_orig <- stats::quantile(data[[response]], top_percentile, na.rm = TRUE)
  data_top_orig <- data[data[[response]] >= cutoff_orig, , drop = FALSE]
  opt_orig <- calculate_opt_values(data, sig_vars, coefs_orig, data_top_orig,
                                    response_var = response, top_percentile = top_percentile,
                                    min_top_samples = min_top_samples)
  cpa_orig_in <- data.frame(
    Variable = c("Intercept", opt_orig$var_table$Variable),
    Beta     = c(coefs_orig[["(Intercept)"]], coefs_orig[opt_orig$var_table$Variable]),
    Mean     = c(1, opt_orig$var_table$Mean),
    Opt      = c(1, opt_orig$var_table$Opt),
    stringsAsFactors = FALSE
  )
  yg_orig <- yield_gap_components(cpa_orig_in)
  ct_orig <- yg_orig$cpa_table

  gap_rows <- lapply(sig_vars, function(v) {
    est <- ct_orig$Gap_Component[ct_orig$Variable == v]
    summarize_vec(boot_gap[, v], paste0(v, " (Gap_Component)"), est)
  })
  share_rows <- lapply(sig_vars, function(v) {
    est <- ct_orig$Share_Percent[ct_orig$Variable == v]
    summarize_vec(boot_share[, v], paste0(v, " (Share_Percent %)"), est)
  })
  overall_rows <- list(
    summarize_vec(boot_yield_mean, "Yield_Mean", yg_orig$yield_mean),
    summarize_vec(boot_yield_opt,  "Yield_Opt",  yg_orig$yield_opt),
    summarize_vec(boot_yield_gap,  "Yield_Gap",  yg_orig$yield_gap)
  )

  summary_df <- do.call(rbind, c(overall_rows, gap_rows, share_rows))
  rownames(summary_df) <- NULL

  selection_stability <- NULL
  if (check_selection_stability) {
    n_valid_sel <- n_boot - n_failed
    selection_stability <- data.frame(
      Variable = sig_vars,
      Times_Selected = as.integer(selection_hits[sig_vars]),
      Selection_Rate = round(100 * selection_hits[sig_vars] / max(1, n_valid_sel), 1),
      stringsAsFactors = FALSE
    )
    selection_stability <- selection_stability[order(-selection_stability$Selection_Rate), ]
    rownames(selection_stability) <- NULL
  }

  if (verbose) {
    cat("\n============================================================\n")
    cat("BOOTSTRAP CPA SUMMARY (", conf_level * 100, "% CI, ", n_boot - n_failed,
        " valid resamples)\n", sep = "")
    cat("============================================================\n")
    print(summary_df, row.names = FALSE)
    if (!is.null(selection_stability)) {
      cat("\nSelection stability (independent re-selection rate per resample):\n")
      print(selection_stability, row.names = FALSE)
    }
  }

  list(
    sig_vars = sig_vars,
    summary = summary_df,
    boot_gap_component = boot_gap,
    boot_share_percent = boot_share,
    boot_yield_mean = boot_yield_mean,
    boot_yield_opt = boot_yield_opt,
    boot_yield_gap = boot_yield_gap,
    selection_stability = selection_stability,
    n_boot = n_boot,
    n_failed = n_failed,
    conf_level = conf_level
  )
}
