
# ============================================================
# File: R/plots.R
# VeraCrop: Visualization Functions
# ============================================================

# ------------------------------------------------------------
# All data-frame columns inside ggplot2::aes() calls in this file are
# referenced via the .data[["colname"]] pronoun instead of bare names.
# This is the standard tidy-eval way to satisfy R CMD check's static
# usage analysis without utils::globalVariables(): .data is imported
# below as a real, visible binding (it is exported by ggplot2 for
# exactly this purpose), so no package name needs to go on the
# suppression list.
# ------------------------------------------------------------

#' Plot Variable Contributions to Yield Gap
#'
#' Horizontal bar chart showing the percentage contribution of each
#' selected variable to the total yield gap.
#'
#' @param results List. Output from \code{\link{yield_gap_analysis}}.
#' @param top_percentile Numeric. Used in subtitle label. Default: 0.99.
#' @param save_plot Logical. Save plot to file. Default: FALSE.
#' @param filename Character. Output file name. Default: "contribution_shares.png".
#'
#' @return A \code{ggplot2} object, or \code{NULL} if no data available.
#'
#' @examples
#' \donttest{
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' plot_contribution_shares(results)
#' }
#'
#' @importFrom ggplot2 .data
#' @export
plot_contribution_shares <- function(results,
                                      top_percentile = 0.99,
                                      save_plot      = FALSE,
                                      filename       = "contribution_shares.png") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' required.")
  if (is.null(results$cpa_table)) return(NULL)

  gap_share <- results$cpa_table
  gap_share <- gap_share[gap_share$Variable != "Intercept", ]
  if (nrow(gap_share) == 0) { warning("No variables to plot"); return(NULL) }

  share_col <- if ("Share_Percent_Abs" %in% names(gap_share)) "Share_Percent_Abs" else "Share_Percent"

  p <- ggplot2::ggplot(gap_share, ggplot2::aes(x = stats::reorder(.data[["Variable"]], .data[[share_col]]),
                                                y = .data[[share_col]])) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", .data[[share_col]])),
                       hjust = -0.1, size = 3.5) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "Contribution of Variables to Yield Gap",
      subtitle = paste0("Top ", (1 - top_percentile) * 100, "% yields used as optimum"),
      x = "Variable", y = "Contribution to Yield Gap (%)"
    ) +
    ggplot2::theme_minimal(base_size = 14)

  if (save_plot) ggplot2::ggsave(filename, p, width = 10, height = 7, dpi = 300)
  return(p)
}


#' Plot Observed vs. Predicted Yield
#'
#' Scatter plot comparing observed and model-predicted yields,
#' including a 1:1 line, regression line, and R-squared/RMSE annotations.
#'
#' @param results List. Output from \code{\link{yield_gap_analysis}}.
#' @param save_plot Logical. Save plot to file. Default: FALSE.
#' @param filename Character. Output file name. Default: "observed_vs_predicted.png".
#'
#' @return A \code{ggplot2} object, or \code{NULL} if no data available.
#'
#' @examples
#' \donttest{
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' plot_observed_vs_predicted(results)
#' }
#'
#' @export
plot_observed_vs_predicted <- function(results,
                                        save_plot = FALSE,
                                        filename  = "observed_vs_predicted.png") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' required.")
  if (is.null(results$predictions) || nrow(results$predictions) == 0) return(NULL)

  pred_df <- results$predictions
  r2   <- results$metrics$R2
  rmse <- results$metrics$RMSE

  p <- ggplot2::ggplot(pred_df, ggplot2::aes(x = .data[["Observed"]], y = .data[["Predicted"]])) +
    ggplot2::geom_point(size = 2, alpha = 0.5, color = "steelblue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
    ggplot2::geom_smooth(method = "lm", se = TRUE, linewidth = 1, color = "red", alpha = 0.2) +
    ggplot2::annotate("text",
                      x = min(pred_df$Observed, na.rm = TRUE),
                      y = max(pred_df$Predicted, na.rm = TRUE),
                      label = paste0("R\u00b2 = ", round(r2, 3), "\nRMSE = ", round(rmse, 1)),
                      hjust = 0, vjust = 1, size = 4) +
    ggplot2::labs(title = "Observed vs Predicted Yield",
                  x = "Observed Yield (kg/ha)", y = "Predicted Yield (kg/ha)") +
    ggplot2::theme_minimal(base_size = 12)

  if (save_plot) ggplot2::ggsave(filename, p, width = 7, height = 6, dpi = 300)
  return(p)
}


#' Plot Distribution of Yield Gap Across Fields
#'
#' Histogram with density curve showing the distribution of per-field
#' yield gaps, with mean and median reference lines.
#'
#' @param results List. Output from \code{\link{yield_gap_analysis}}.
#' @param save_plot Logical. Save plot to file. Default: FALSE.
#' @param filename Character. Output file name. Default: "yield_gap_distribution.png".
#'
#' @return A \code{ggplot2} object, or \code{NULL} if no data available.
#'
#' @examples
#' \donttest{
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' plot_yield_gap_distribution(results)
#' }
#'
#' @importFrom stats density median
#' @export
plot_yield_gap_distribution <- function(results,
                                         save_plot = FALSE,
                                         filename  = "yield_gap_distribution.png") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' required.")
  if (is.null(results$predictions) || nrow(results$predictions) == 0) return(NULL)

  gap_df     <- results$predictions
  mean_gap   <- mean(gap_df$Yield_Gap_Obs, na.rm = TRUE)
  median_gap <- median(gap_df$Yield_Gap_Obs, na.rm = TRUE)
  max_y      <- max(density(gap_df$Yield_Gap_Obs, na.rm = TRUE)$y) * 0.9

  p <- ggplot2::ggplot(gap_df, ggplot2::aes(x = as.numeric(.data[["Yield_Gap_Obs"]]))) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
    ggplot2::geom_density(color = "red", linewidth = 1) +
    ggplot2::geom_vline(xintercept = mean_gap,   linetype = "dashed", color = "darkblue",  linewidth = 1) +
    ggplot2::geom_vline(xintercept = median_gap, linetype = "dotted", color = "darkgreen", linewidth = 1) +
    ggplot2::annotate("text", x = mean_gap,   y = max_y,
                      label = paste("Mean =", round(mean_gap, 0)),   hjust = -0.1, size = 3.5) +
    ggplot2::annotate("text", x = median_gap, y = max_y * 0.88,
                      label = paste("Median =", round(median_gap, 0)), hjust = -0.1, size = 3.5) +
    ggplot2::labs(title    = "Distribution of Yield Gap Across Fields",
                  subtitle = paste("Total fields:", nrow(gap_df)),
                  x = "Yield Gap (kg/ha)", y = "Density") +
    ggplot2::theme_minimal(base_size = 12)

  if (save_plot) ggplot2::ggsave(filename, p, width = 8, height = 6, dpi = 300)
  return(p)
}


#' Plot Min-Max Effects of Variables on Yield
#'
#' Diverging horizontal bar chart showing how much yield changes when
#' each variable moves from its minimum to maximum value.
#'
#' @param results List. Output from \code{\link{yield_gap_analysis}}.
#' @param save_plot Logical. Save plot to file. Default: FALSE.
#' @param filename Character. Output file name. Default: "minmax_effects.png".
#'
#' @return A \code{ggplot2} object, or \code{NULL} if no data available.
#'
#' @examples
#' \donttest{
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' plot_minmax_effects(results)
#' }
#'
#' @export
plot_minmax_effects <- function(results,
                                 save_plot = FALSE,
                                 filename  = "minmax_effects.png") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' required.")
  if (is.null(results$minmax_results)) return(NULL)

  mm_data            <- results$minmax_results
  mm_data            <- mm_data[order(mm_data$Effect), ]
  mm_data$Direction  <- ifelse(mm_data$Effect > 0, "Positive", "Negative")

  p <- ggplot2::ggplot(mm_data, ggplot2::aes(x = stats::reorder(.data[["Variable"]], .data[["Effect"]]),
                                              y = .data[["Effect"]], fill = .data[["Direction"]])) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("Positive" = "steelblue", "Negative" = "coral")) +
    ggplot2::labs(title    = "Impact of Each Variable (Min to Max Change)",
                  subtitle = "Effect on yield when variable moves from min to max",
                  x = "Variable", y = "Effect on Yield (kg/ha)") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  if (save_plot) ggplot2::ggsave(filename, p, width = 9, height = 6, dpi = 300)
  return(p)
}


#' Plot Relative Importance of Variables
#'
#' Horizontal bar chart of relative importance percentages, using either
#' the LMG or standardized beta method.
#'
#' @param results List. Output from \code{\link{yield_gap_analysis}}.
#' @param save_plot Logical. Save plot to file. Default: FALSE.
#' @param filename Character. Output file name. Default: "relative_importance.png".
#'
#' @return A \code{ggplot2} object, or \code{NULL} if no data available.
#'
#' @examples
#' \donttest{
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' plot_relative_importance(results)
#' }
#'
#' @export
plot_relative_importance <- function(results,
                                      save_plot = FALSE,
                                      filename  = "relative_importance.png") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' required.")
  if (is.null(results$relative_importance)) return(NULL)

  ri_data <- results$relative_importance$results

  if ("LMG_Percent" %in% names(ri_data)) {
    ri_data$Importance <- ri_data$LMG_Percent
    title_method       <- "LMG Method"
  } else if ("RelImportancePct" %in% names(ri_data)) {
    ri_data$Importance <- ri_data$RelImportancePct
    title_method       <- "Standardized Beta Method"
  } else {
    return(NULL)
  }

  ri_data <- ri_data[order(ri_data$Importance), ]

  p <- ggplot2::ggplot(ri_data, ggplot2::aes(x = stats::reorder(.data[["Variable"]], .data[["Importance"]]),
                                              y = .data[["Importance"]])) +
    ggplot2::geom_bar(stat = "identity", fill = "darkgreen", alpha = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(title    = paste("Relative Importance of Variables -", title_method),
                  subtitle = "Percentage contribution to explained variance",
                  x = "Variable", y = "Relative Importance (%)") +
    ggplot2::theme_minimal(base_size = 12)

  if (save_plot) ggplot2::ggsave(filename, p, width = 9, height = 6, dpi = 300)
  return(p)
}


#' Generate All Yield Gap Plots
#'
#' Convenience function that calls all five plotting functions and
#' optionally combines them into a single figure using \pkg{patchwork}.
#'
#' @param results List. Output from \code{\link{yield_gap_analysis}}.
#' @param top_percentile Numeric. Passed to \code{\link{plot_contribution_shares}}. Default: 0.99.
#' @param save_plots Logical. Save individual and combined plots. Default: FALSE.
#' @param output_dir Character. Directory for saved plots. Default:
#'   \code{tempdir()}.
#' @param verbose Logical. Print progress messages while generating plots.
#'   Default: TRUE.
#'
#' @return Invisibly returns a named list of all five \code{ggplot2} objects.
#'
#' @examples
#' \donttest{
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' plot_all_graphs(results, top_percentile = 0.95,
#'                  save_plots = TRUE, output_dir = tempdir(), verbose = FALSE)
#' }
#'
#' @export
plot_all_graphs <- function(results,
                             top_percentile = 0.99,
                             save_plots     = FALSE,
                             output_dir     = tempdir(),
                             verbose        = TRUE) {

  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("GENERATING ALL PLOTS\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }

  if (verbose) cat("\n  Plot 1: Contribution shares...\n")
  p1 <- plot_contribution_shares(results, top_percentile, save_plots,
                                  file.path(output_dir, "01_contribution_shares.png"))
  if (verbose) cat("  Plot 2: Observed vs Predicted...\n")
  p2 <- plot_observed_vs_predicted(results, save_plots,
                                    file.path(output_dir, "02_observed_vs_predicted.png"))
  if (verbose) cat("  Plot 3: Yield gap distribution...\n")
  p3 <- plot_yield_gap_distribution(results, save_plots,
                                     file.path(output_dir, "03_yield_gap_distribution.png"))
  if (verbose) cat("  Plot 4: Min-Max effects...\n")
  p4 <- plot_minmax_effects(results, save_plots,
                             file.path(output_dir, "04_minmax_effects.png"))
  if (verbose) cat("  Plot 5: Relative importance...\n")
  p5 <- plot_relative_importance(results, save_plots,
                                  file.path(output_dir, "05_relative_importance.png"))

  all_plot_list <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5)
  available <- !vapply(all_plot_list, is.null, logical(1))
  missing_names <- names(all_plot_list)[!available]

  if (verbose && length(missing_names) > 0) {
    cat(sprintf("\n  Note: %d plot(s) unavailable and will be skipped: %s\n",
                length(missing_names), paste(missing_names, collapse = ", ")))
  }

  if (!any(available)) {
    if (verbose) cat("\n  No plots could be generated (all inputs unavailable).\n")
    return(invisible(all_plot_list))
  }

  if (requireNamespace("patchwork", quietly = TRUE) && sum(available) > 1) {
    # Combine only the plots that are actually available, in their
    # original relative order (p1..p5), rather than assuming all five
    # exist - a NULL plot combined via `+ patchwork::plot_layout(...)`
    # fails with a confusing "non-numeric argument to binary operator"
    # error instead of a clear message.
    available_plots <- all_plot_list[available]
    all_plots <- Reduce(`+`, available_plots) +
      patchwork::plot_layout(ncol = 2) +
      patchwork::plot_annotation(
        title = "Yield Gap Analysis - Complete Report",
        theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 16))
      )

    if (save_plots) {
      ggplot2::ggsave(file.path(output_dir, "00_all_plots_combined.png"),
                      all_plots, width = 14, height = 16, dpi = 300)
      if (verbose) cat("\n  Combined plot saved to:", file.path(output_dir, "00_all_plots_combined.png"), "\n")
    }
    print(all_plots)
  } else {
    if (verbose && sum(available) > 1) {
      cat("\n  'patchwork' not installed. Displaying plots individually.\n")
    }
    plot_files <- c(
      p1 = "01_contribution_shares.png", p2 = "02_observed_vs_predicted.png",
      p3 = "03_yield_gap_distribution.png", p4 = "04_minmax_effects.png",
      p5 = "05_relative_importance.png"
    )
    avail_names <- names(all_plot_list)[available]
    for (idx in seq_along(avail_names)) {
      nm <- avail_names[idx]
      print(all_plot_list[[nm]])
      if (save_plots && verbose) cat("  Saved:", file.path(output_dir, plot_files[[nm]]), "\n")
      if (idx < length(avail_names) && interactive()) {
        readline("Press Enter for next plot...")
      }
    }
  }

  if (verbose) cat("\n  All plots generated successfully!\n")
  invisible(all_plot_list)
}

