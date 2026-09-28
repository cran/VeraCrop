# ============================================================
# File: R/data.R
# VeraCrop: Documentation for Bundled Example Datasets
# ============================================================

#' Wheat Yield Trial Data (Clean)
#'
#' Field-level wheat yield data from 200 fields, recording yield and
#' agronomic management/environmental variables. Used throughout the
#' package documentation and vignette as the primary worked example: a
#' relatively large, clean dataset (no missing values) suitable for
#' demonstrating the full \code{\link{prep_yield_gap}} /
#' \code{\link{yield_gap_analysis}} workflow end to end.
#'
#' @format A data frame with 200 rows and 13 variables:
#' \describe{
#'   \item{Yield}{Numeric. Grain yield (kg/ha). Response variable.}
#'   \item{Irrigation}{Numeric. Irrigation water applied.}
#'   \item{Nitrogen}{Numeric. Nitrogen fertilizer applied (kg/ha).}
#'   \item{Phosphorus}{Numeric. Phosphorus fertilizer applied (kg/ha).}
#'   \item{Potassium}{Numeric. Potassium fertilizer applied (kg/ha).}
#'   \item{Soil_pH}{Numeric. Soil pH.}
#'   \item{Soil_Organic_Matter}{Numeric. Soil organic matter (percent).}
#'   \item{Plant_density}{Numeric. Plant density (plants per unit area).}
#'   \item{Weed_Infestation}{Numeric. Weed infestation score/count.}
#'   \item{Drought}{Integer (0/1). Whether the field experienced drought stress.}
#'   \item{Pest}{Integer (0/1). Whether the field experienced pest pressure.}
#'   \item{Cultivar}{Character. Wheat cultivar name (nominal, several levels).}
#'   \item{Sowing_Date}{Numeric. Sowing date, encoded as day offset.}
#' }
#'
#' @source Field trial data used during package development and testing.
#'
#' @examples
#' data(wheat1)
#' str(wheat1)
#' prep <- prep_yield_gap(wheat1, response_var = "Yield", verbose = FALSE)
#' dim(prep$data)
"wheat1"


#' Wheat Yield Trial Data (Small, High-Dimensional, Real-World Messy)
#'
#' Field-level wheat yield data from 40 fields with many recorded
#' management, socioeconomic, and pest/disease variables (29 columns).
#' Deliberately kept in its original, unedited form (including a
#' non-predictor ID column and a stray leading/trailing whitespace
#' inconsistency in \code{Cultivar}, where "Sirvan" appears both as
#' \code{"Sirvan"} and \code{"Sirvan "}) to serve as a realistic example
#' of common real-world data-quality issues and of a small, high
#' predictor-to-sample-ratio dataset - exactly the profile that exercises
#' \code{\link{trim_whitespace}}, near-zero-variance filtering, and the
#' auto-tuning logic in \code{\link{prep_yield_gap}}.
#'
#' @format A data frame with 40 rows and 29 variables:
#' \describe{
#'   \item{Field_id}{Integer. Field identifier (non-predictor; removed
#'     automatically by \code{\link{remove_non_predictors}}).}
#'   \item{Farmers_age}{Integer. Age of the farmer (years).}
#'   \item{Litracy_Level}{Character. Farmer's education level (a true
#'     ordinal variable stored as text - see Examples for how to encode
#'     its order explicitly via \code{ordinal_level_orders}).}
#'   \item{Experience_Years}{Integer. Farmer's years of experience.}
#'   \item{Cultivated_Area}{Numeric. Cultivated area (hectares).}
#'   \item{Rotation}{Character. Crop rotation sequence preceding wheat.}
#'   \item{Crop_Residue_Mgmt}{Character. How previous crop residue was
#'     managed (e.g. "Burned", "Incorporated", "Retained").}
#'   \item{Pre_Plow_Irrigation}{Character. "Yes"/"No": whether a
#'     pre-plowing irrigation was applied.}
#'   \item{Disk_Passes}{Integer. Number of disk harrow passes.}
#'   \item{Land_Leveled}{Character. "Yes"/"No": whether the land was
#'     laser-leveled.}
#'   \item{Seeder_Type}{Character. Type of seeder used (e.g. "Seed Drill",
#'     "Centrifugal").}
#'   \item{Planting_Day_From_Sep23}{Integer. Planting date, as days after
#'     September 23.}
#'   \item{Seed_Rate}{Integer. Seeding rate.}
#'   \item{Cultivar}{Character. Wheat cultivar name. Contains a real
#'     leading/trailing whitespace inconsistency ("Sirvan" vs. "Sirvan ")
#'     used to illustrate \code{\link{trim_whitespace}}.}
#'   \item{TSP_Rate}{Integer. Triple superphosphate fertilizer rate.}
#'   \item{Good_Drainage}{Character. "Yes"/"No": whether the field has
#'     good drainage.}
#'   \item{Irrigation_Count}{Integer. Number of irrigation events.}
#'   \item{Soil_texture}{Character. Soil texture class (e.g. "Clay",
#'     "Clay-sand").}
#'   \item{Weed_species}{Character. Multi-value field: weed species
#'     present, concatenated with "-" separators - treated as a
#'     high-cardinality nominal variable; see
#'     \code{\link{group_high_cardinality}}.}
#'   \item{Spraying_DAP}{Integer. Herbicide spraying date, in days after
#'     planting.}
#'   \item{Herbicide_Type}{Character. Multi-value field: herbicide
#'     active ingredient(s) applied, concatenated with "_" separators.}
#'   \item{Urea_as_adjuvant}{Character. "Yes"/"No": whether urea was used
#'     as a spray adjuvant.}
#'   \item{Nitrogen_topdressing_Number}{Integer. Number of nitrogen
#'     topdressing applications.}
#'   \item{Topdressing_N}{Integer. Total topdressing nitrogen applied.}
#'   \item{Puccinia_disease}{Character. "Yes"/"No": presence of
#'     \emph{Puccinia} (rust) disease.}
#'   \item{Days_To_Harvest}{Integer. Days from planting to harvest.}
#'   \item{Selection_Pressure}{Character. "Yes"/"No": indicator of
#'     herbicide selection pressure.}
#'   \item{Herbicide_Resistance_Suspected}{Character. "Yes"/"No": whether
#'     herbicide resistance was suspected in the field.}
#'   \item{Yield}{Integer. Grain yield (kg/ha). Response variable.}
#' }
#'
#' @source Field trial data used during package development and testing.
#'
#' @examples
#' data(wheat2)
#'
#' # Demonstrates automatic whitespace trimming: "Sirvan " and "Sirvan"
#' # are correctly merged into a single category.
#' length(unique(wheat2$Cultivar))
#' trimmed <- trim_whitespace(wheat2, verbose = FALSE)
#' length(unique(trimmed$Cultivar))
#'
#' \donttest{
#' # Full pipeline, including an explicit ordinal level order for
#' # Litracy_Level since it cannot be safely inferred automatically.
#' prep <- prep_yield_gap(
#'   wheat2,
#'   response_var = "Yield",
#'   ordinal_level_orders = list(
#'     Litracy_Level = c("Elementary School", "Middle School", "Diploma",
#'                        "Bachelor's Degree", "Master's Degree")
#'   ),
#'   verbose = FALSE
#' )
#' dim(prep$data)
#' }
"wheat2"


#' Sugarcane Yield Trial Data
#'
#' Field-level sugarcane yield data from 371 fields, recording yield and a
#' broad range of crop-cycle, fertilization, pest/disease, and
#' remote-sensing/soil variables. No missing values. A larger, cleaner
#' third example dataset (compared to \code{\link{wheat2}}),
#' useful for illustrating the package on a different crop and a dataset
#' with several genuinely ordinal and perfectly collinear (aliased)
#' variables - see Examples.
#'
#' @format A data frame with 371 rows and 30 variables:
#' \describe{
#'   \item{field_id}{Character. Field identifier (non-predictor; removed
#'     automatically by \code{\link{remove_non_predictors}}).}
#'   \item{cultivar}{Character. Sugarcane cultivar name (nominal).}
#'   \item{crop_type}{Character. Crop cycle stage: \code{"PC"} (plant
#'     cane) followed by \code{"R1"} through \code{"R9"} (successive
#'     ratoon/regrowth cycles). This is a genuinely ORDINAL variable
#'     stored as text (PC < R1 < R2 < ... < R9) - see Examples for how to
#'     encode its order explicitly via \code{ordinal_level_orders}.}
#'   \item{texture_class}{Character. Soil texture class (nominal, e.g.
#'     "Loam", "Silty clay").}
#'   \item{drainage_issue}{Character. "Yes"/"No": whether the field has a
#'     drainage issue.}
#'   \item{smut_incidence}{Character. "Yes"/"No": presence of sugarcane
#'     smut disease.}
#'   \item{other_weeds_presence}{Character. "Yes"/"No": presence of weeds
#'     other than the main tracked species.}
#'   \item{harvestable_area}{Numeric. Harvestable area (hectares).}
#'   \item{harvest_days_from_September23_2023}{Integer. Harvest date, as
#'     days after September 23, 2023.}
#'   \item{harvest_duration}{Numeric. Duration of the harvest operation
#'     (days).}
#'   \item{crop_cycle_days}{Integer. Total length of the crop cycle
#'     (days).}
#'   \item{first_topdressing_days_from_February20_2023}{Integer. First
#'     nitrogen topdressing date, as days after February 20, 2023.}
#'   \item{second_topdressing_days_from_February20_2023}{Integer. Second
#'     nitrogen topdressing date, as days after February 20, 2023.}
#'   \item{third_topdressing_days_from_February20_2023}{Integer. Third
#'     nitrogen topdressing date, as days after February 20, 2023.}
#'   \item{starter_urea_kg_ha}{Numeric. Starter urea fertilizer rate
#'     (kg/ha).}
#'   \item{first_topdressing_urea_kg_ha}{Numeric. First topdressing urea
#'     rate (kg/ha).}
#'   \item{second_topdressing_urea_kg_ha}{Numeric. Second topdressing
#'     urea rate (kg/ha).}
#'   \item{third_topdressing_urea_kg_ha}{Numeric. Third topdressing urea
#'     rate (kg/ha).}
#'   \item{total_urea_kg_ha}{Numeric. Total urea applied (kg/ha). NOTE:
#'     this is the exact sum of \code{starter_urea_kg_ha},
#'     \code{first_topdressing_urea_kg_ha},
#'     \code{second_topdressing_urea_kg_ha}, and
#'     \code{third_topdressing_urea_kg_ha} - i.e. perfectly collinear
#'     (aliased) with them when all four are included in the same model.
#'     Both \code{\link{select_vars_aic}} and
#'     \code{\link{select_vars_ftest}} handle this gracefully
#'     (see Examples), but including this column alongside all four of its
#'     components in a manually-fit \code{lm()} will produce an \code{NA}
#'     coefficient for whichever is entered last.}
#'   \item{leaf_N_at_mid_stem_elongation}{Numeric. Leaf nitrogen
#'     concentration at mid stem elongation (percent).}
#'   \item{leaf_N_at_grand_growth}{Numeric. Leaf nitrogen concentration at
#'     grand growth stage (percent).}
#'   \item{leaf_N_before_irrigation_cutoff}{Numeric. Leaf nitrogen
#'     concentration before irrigation cutoff (percent).}
#'   \item{irrigation_cutoff_days_from_September23_2023}{Integer.
#'     Irrigation cutoff date, as days after September 23, 2023.}
#'   \item{sesamia_bored_internodes_percent}{Numeric. Percentage of
#'     internodes bored by \emph{Sesamia} stem borer.}
#'   \item{cogongrass_infested_m2}{Integer. Area infested by cogongrass
#'     (square meters).}
#'   \item{irrigation_interval}{Numeric. Average interval between
#'     irrigation events (days).}
#'   \item{max_LAI}{Numeric. Maximum Leaf Area Index observed during the
#'     season.}
#'   \item{MCARI}{Numeric. Modified Chlorophyll Absorption in Reflectance
#'     Index (a remote-sensing vegetation index).}
#'   \item{soil_EC}{Numeric. Soil electrical conductivity.}
#'   \item{Yield}{Numeric. Cane yield. Response variable.}
#' }
#'
#' @source Field trial data used during package development and testing.
#'
#' @examples
#' data(sugarcane1)
#'
#' # crop_type is genuinely ordinal (PC < R1 < ... < R9); without an
#' # explicit order it would conservatively be treated as nominal.
#' \donttest{
#' prep <- prep_yield_gap(
#'   sugarcane1,
#'   response_var = "Yield",
#'   ordinal_level_orders = list(
#'     crop_type = c("PC", paste0("R", 1:9))
#'   ),
#'   verbose = FALSE
#' )
#' dim(prep$data)
#'
#' # total_urea_kg_ha is a perfect linear combination of the four
#' # individual urea application columns; both selection methods drop it
#' # automatically (with a warning) rather than erroring.
#' result <- yield_gap_analysis(prep$data, response = "Yield",
#'                               selection_method = "aic", verbose = FALSE)
#' result$yield_gap
#' }
"sugarcane1"
