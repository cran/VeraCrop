
# ============================================================
# File: R/export.R
# VeraCrop: Export Functions for Excel Output
# ============================================================

#' Save Yield Gap Analysis Results to Excel
#'
#' Exports all analysis results (CPA table, predictions, variable statistics,
#' relative importance, Pareto analysis, cross-validation, and model summary)
#' to a multi-sheet Excel file.
#'
#' @param results List. Output from \code{\link{yield_gap_analysis}}.
#' @param output_path Character. Full path for the output Excel file.
#'   Default: \code{file.path(tempdir(), "yield_gap_analysis_full.xlsx")}.
#' @param verbose Logical. Print a confirmation message with the saved
#'   file path and sheet names. Default: TRUE.
#'
#' @return Invisibly returns the output path.
#'
#' @details Requires the \pkg{writexl} package. Please install it with:
#'   \code{install.packages("writexl")}
#'
#' @examples
#' \donttest{
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' save_results_excel(results,
#'                     output_path = file.path(tempdir(), "yield_gap_results.xlsx"))
#' }
#'
#' @export
save_results_excel <- function(results,
                               output_path = file.path(tempdir(), "yield_gap_analysis_full.xlsx"),
                               verbose = TRUE) {

  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required for Excel export. Please install it: install.packages('writexl')")
  }

  excel_outputs <- list()

  if (!is.null(results$cpa_table))
    excel_outputs[["CPA_Table"]] <- results$cpa_table

  if (!is.null(results$predictions))
    excel_outputs[["Predictions"]] <- results$predictions

  if (!is.null(results$var_table))
    excel_outputs[["Variables_Table"]] <- results$var_table

  if (!is.null(results$minmax_results))
    excel_outputs[["MinMax_Effects"]] <- results$minmax_results

  if (!is.null(results$relative_importance) && !is.null(results$relative_importance$results))
    excel_outputs[["Relative_Importance"]] <- results$relative_importance$results

  if (!is.null(results$pareto_analysis) && !is.null(results$pareto_analysis$pareto_table))
    excel_outputs[["Pareto_Analysis"]] <- results$pareto_analysis$pareto_table

  if (!is.null(results$cross_validation)) {
    if (!is.null(results$cross_validation$by_fold))
      excel_outputs[["CrossValidation_Folds"]] <- results$cross_validation$by_fold
    if (!is.null(results$cross_validation$summary))
      excel_outputs[["CrossValidation_Summary"]] <- results$cross_validation$summary
  }

  excel_outputs[["Model_Summary"]] <- data.frame(
    Metric = c("R2", "RMSE (kg/ha)", "MAE (kg/ha)",
               "Current Mean Yield (kg/ha)", "Optimal Yield (kg/ha)",
               "Yield Gap (kg/ha)", "Gap % of Optimal"),
    Value  = c(results$metrics$R2, results$metrics$RMSE, results$metrics$MAE,
               results$yield_mean, results$yield_opt, results$yield_gap,
               results$yield_gap / results$yield_opt * 100)
  )

  if (!is.null(results$significant_vars))
    excel_outputs[["Significant_Variables"]] <- data.frame(
      Rank     = seq_along(results$significant_vars),
      Variable = results$significant_vars
    )

  if (!is.null(results$model)) {
    coef_sum <- summary(results$model)$coefficients
    excel_outputs[["Regression_Coefficients"]] <- data.frame(
      Variable    = rownames(coef_sum),
      Coefficient = coef_sum[, 1],
      Std_Error   = coef_sum[, 2],
      t_value     = coef_sum[, 3],
      p_value     = coef_sum[, 4]
    )
  }

  writexl::write_xlsx(excel_outputs, path = output_path)
  if (verbose) {
    cat("  Excel file saved to:", output_path, "\n")
    cat("  Sheets:", paste(names(excel_outputs), collapse = ", "), "\n")
  }
  invisible(output_path)
}


#' Save Field-Level Yield Gap Analysis to Excel
#'
#' Creates a detailed field-level Excel report showing per-field predictions,
#' yield gaps, factor contributions, limiting factors, and rankings.
#'
#' @param results List. Output from \code{\link{yield_gap_analysis}}.
#' @param df_final Data frame. The preprocessed dataset (before analysis).
#' @param output_path Character. Full path for the output Excel file.
#'   Default: \code{file.path(tempdir(), "field_level_analysis.xlsx")}.
#' @param verbose Logical. Print a confirmation message with the saved
#'   file path and sheet names. Default: TRUE.
#'
#' @return Invisibly returns the output path.
#'
#' @details Requires the \pkg{writexl} package. Please install it with:
#'   \code{install.packages("writexl")}
#'
#' @examples
#' \donttest{
#' data(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' results <- yield_gap_analysis(prep$data, response = "Yield", verbose = FALSE)
#' save_field_level_excel(results, prep$data,
#'                         output_path = file.path(tempdir(), "field_analysis.xlsx"))
#' }
#'
#' @export
save_field_level_excel <- function(results, df_final,
                                    output_path = file.path(tempdir(), "field_level_analysis.xlsx"),
                                    verbose = TRUE) {

  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required for Excel export. Please install it: install.packages('writexl')")
  }

  if (is.null(results)) { warning("results is NULL. Cannot create field-level Excel."); return(invisible(NULL)) }

  field_data <- df_final
  field_data$Observed_Yield  <- results$predictions$Observed
  field_data$Predicted_Yield <- results$predictions$Predicted
  field_data$Yield_Gap       <- results$predictions$Yield_Gap_Obs
  field_data$Yield_Gap_Percent <- (results$predictions$Yield_Gap_Obs / results$yield_opt) * 100

  coefs    <- coef(results$model)
  coefs    <- coefs[names(coefs) != "(Intercept)"]
  opt_vals <- results$var_table$Opt
  names(opt_vals) <- results$var_table$Variable

  contribution_matrix <- matrix(NA, nrow = nrow(field_data), ncol = length(coefs))
  colnames(contribution_matrix) <- paste0("Contribution_", names(coefs))

  for (i in seq_along(coefs)) {
    var_name <- names(coefs)[i]
    if (var_name %in% names(field_data) && var_name %in% names(opt_vals)) {
      contribution_matrix[, i] <- coefs[var_name] * (opt_vals[var_name] - field_data[[var_name]])
    }
  }

  contrib_df <- as.data.frame(contribution_matrix)
  field_data <- cbind(field_data, contrib_df)
  contrib_cols <- grep("^Contribution_", names(field_data), value = TRUE)

  # Top limiting factor (most negative contribution per field)
  neg_contrib <- field_data[, contrib_cols, drop = FALSE]
  neg_contrib[neg_contrib > 0] <- NA
  field_data$Top_Limiting_Factor <- apply(neg_contrib, 1, function(x) {
    if (all(is.na(x))) return("None")
    gsub("Contribution_", "", names(x)[which.min(x)])
  })
  field_data$Top_Limiting_Value <- apply(neg_contrib, 1, function(x) {
    if (all(is.na(x))) return(0)
    abs(min(x, na.rm = TRUE))
  })

  # Top improving factor (most positive contribution per field)
  pos_contrib <- field_data[, contrib_cols, drop = FALSE]
  pos_contrib[pos_contrib < 0] <- NA
  field_data$Top_Improving_Factor <- apply(pos_contrib, 1, function(x) {
    if (all(is.na(x))) return("None")
    gsub("Contribution_", "", names(x)[which.max(x)])
  })
  field_data$Top_Improving_Value <- apply(pos_contrib, 1, function(x) {
    if (all(is.na(x))) return(0)
    max(x, na.rm = TRUE)
  })

  field_excel_outputs <- list()

  # Sheet 1: Summary
  field_excel_outputs[["Field_Summary"]] <- data.frame(
    Statistic = c("Total Fields", "Average Yield Gap", "Min Yield Gap",
                  "Max Yield Gap", "Std Dev Yield Gap", "Average Gap %", "Fields with Gap > 0"),
    Value = c(
      nrow(field_data),
      round(mean(field_data$Yield_Gap, na.rm = TRUE), 1),
      round(min(field_data$Yield_Gap,  na.rm = TRUE), 1),
      round(max(field_data$Yield_Gap,  na.rm = TRUE), 1),
      round(stats::sd(field_data$Yield_Gap,   na.rm = TRUE), 1),
      round(mean(field_data$Yield_Gap_Percent, na.rm = TRUE), 1),
      sum(field_data$Yield_Gap > 0, na.rm = TRUE)
    )
  )

  # Sheet 2: Basic field info
  field_basic <- field_data[, c("Observed_Yield", "Predicted_Yield",
                                 "Yield_Gap", "Yield_Gap_Percent",
                                 "Top_Limiting_Factor", "Top_Limiting_Value",
                                 "Top_Improving_Factor", "Top_Improving_Value")]
  field_excel_outputs[["Field_Basic_Info"]] <- cbind(Field_ID = seq_len(nrow(field_basic)), field_basic)

  # Sheet 3: Contribution matrix
  contrib_only <- field_data[, c("Observed_Yield", contrib_cols), drop = FALSE]
  field_excel_outputs[["Field_Contributions"]] <- cbind(Field_ID = seq_len(nrow(contrib_only)), contrib_only)

  # Sheet 4: Raw preprocessed data
  raw_data_display <- field_data[, names(df_final), drop = FALSE]
  field_excel_outputs[["Field_Raw_Data"]] <- cbind(Field_ID = seq_len(nrow(raw_data_display)), raw_data_display)

  # Sheet 5: Field ranking by yield gap
  field_ranking <- field_data[order(field_data$Yield_Gap), ]
  field_excel_outputs[["Field_Ranking"]] <- data.frame(
    Rank                 = seq_len(nrow(field_ranking)),
    Field_ID             = seq_len(nrow(field_ranking)),
    Observed_Yield       = field_ranking$Observed_Yield,
    Yield_Gap            = field_ranking$Yield_Gap,
    Yield_Gap_Percent    = round(field_ranking$Yield_Gap_Percent, 1),
    Top_Limiting_Factor  = field_ranking$Top_Limiting_Factor,
    Top_Improving_Factor = field_ranking$Top_Improving_Factor
  )

  # Sheet 6: Limiting factors frequency
  lf <- as.data.frame(table(field_data$Top_Limiting_Factor))
  names(lf) <- c("Limiting_Factor", "Number_of_Fields")
  field_excel_outputs[["Limiting_Factors_Freq"]] <- lf[order(-lf$Number_of_Fields), ]

  # Sheet 7: Improving factors frequency
  imf <- as.data.frame(table(field_data$Top_Improving_Factor))
  names(imf) <- c("Improving_Factor", "Number_of_Fields")
  field_excel_outputs[["Improving_Factors_Freq"]] <- imf[order(-imf$Number_of_Fields), ]

  writexl::write_xlsx(field_excel_outputs, path = output_path)
  if (verbose) {
    cat("  Field-level Excel saved to:", output_path, "\n")
    cat("  Sheets: Field_Summary, Field_Basic_Info, Field_Contributions,\n")
    cat("          Field_Raw_Data, Field_Ranking, Limiting_Factors_Freq, Improving_Factors_Freq\n")
  }
  invisible(output_path)
}

