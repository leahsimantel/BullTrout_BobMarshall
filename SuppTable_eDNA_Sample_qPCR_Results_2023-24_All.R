############################
#####    TABLE S1 
#####   CHAPTER 2 of MS THESIS
#####   L. Simantel
#####  eDNA qPCR results
############################

library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)

folder_path <- "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/Field_Data/2023/2023_eDNA_results"

excel_file <- file.path(
  folder_path,
  "Simantel_eDNA_Results_Nov.29.2023_postQAQC.xlsx"
)

collection_extraction <- read_excel(
  excel_file,
  sheet = "Collection and Extraction",
  skip = 2
) %>%
  clean_names() %>%
  mutate(
    site_number = str_trim(site_number),
    id_tag = str_trim(id_tag)
  )

qpcr <- read_excel(
  excel_file,
  sheet = "qPCR"
) %>%
  clean_names() %>%
  mutate(
    id_tag = str_trim(id_tag),
    assay = str_trim(assay),
    assay_clean = make_clean_names(assay, allow_dupes = TRUE)
  )

edna_2023_sample_table <- qpcr %>%
  filter(!is.na(id_tag), !is.na(assay)) %>%
  pivot_wider(
    id_cols = id_tag,
    names_from = assay_clean,
    values_from = -c(id_tag, assay, assay_clean),
    names_glue = "qpcr_{assay_clean}_{.value}",
    values_fn = list
  ) %>%
  right_join(
    collection_extraction,
    by = "id_tag"
  ) %>%
  relocate(site_number, id_tag)

#########
### read in the 2023 sample list from sampling (not from lab)
field_file <- file.path(
  folder_path,
  "2023_field_data_eDNA_results.xlsx"
)

field_2023_raw <- read_excel(field_file)

field_2023 <- field_2023_raw %>%
  clean_names() %>%
  mutate(
    sample_id = str_trim(sample_id),
    sample_id_match = str_to_upper(sample_id)
  )

edna_check <- edna_2023_sample_table %>%
  mutate(
    site_number = str_trim(site_number),
    site_number_match = str_to_upper(site_number)
  )

### Cross-reference Sample ID's:
samples_in_excel_not_edna <- field_2023 %>%
  anti_join(
    edna_check,
    by = c("sample_id_match" = "site_number_match")
  ) %>%
  select(sample_id, everything())

samples_in_edna_not_excel <- edna_check %>%
  anti_join(
    field_2023,
    by = c("site_number_match" = "sample_id_match")
  ) %>%
  select(site_number, id_tag, everything())

#### Column comparisons:
excel_cols_not_in_edna <- setdiff(
  names(field_2023),
  names(edna_check)
)
edna_cols_not_in_excel <- setdiff(
  names(edna_check),
  names(field_2023)
)

### Add in Lower SB River point to the eDNA table:
edna_2023_complete <- field_2023 %>%
  left_join(
    edna_check,
    by = c("sample_id_match" = "site_number_match"),
    suffix = c("_field", "_lab")
  )

### Cleaning up columns in final 2023 table: QC ----------------

## Consolidate date, state, & agency columns 
edna_2023_complete <- edna_2023_complete %>%
  mutate(
    state = coalesce(
      na_if(str_trim(state_5), ""),
      na_if(str_trim(state_16), "")
    ),
    .before = state_5
  ) %>%
  select(-state_5, -state_16)

edna_2023_complete <- edna_2023_complete %>%
  mutate(
    agency = str_replace(
      agency_field,
      "^USFS-\\s*ALWRI$",
      "USFS-ALWRI"
    ),
    .before = agency_field
  ) %>%
  select(-agency_field, -agency_lab)

# Keep field collection dates and remove the lab version
edna_2023_complete <- edna_2023_complete %>%
  select(-date_collected_lab) %>%
  rename(date_collected = date_collected_field)

# Keep field collaborator values and remove the lab version
edna_2023_complete <- edna_2023_complete %>%
  select(-collaborator_lab) %>%
  rename(collaborator = collaborator_field)

# Keep field coordinates and remove the lab versions
edna_2023_complete <- edna_2023_complete %>%
  select(-field_latitude_lab, -field_longitude_lab) %>%
  rename(
    field_latitude = field_latitude_field,
    field_longitude = field_longitude_field
  )

## Update two samples to reflect corrected stream names 
edna_2023_complete <- edna_2023_complete %>%
  mutate(
    feature_name_lab = if_else(
      sample_id %in% c("HUC35_058", "LSPBR_617"),
      feature_name_field,
      feature_name_lab
    )
  )

# Use field notes for only these samples
samples_use_field_notes <- c(
  "HUC16_208", "HUC16_278", "HUC33_280", "LSPBR_617",
  "HUC35_058", "HUC36_289", "HUC16_202", "HUC33_187",
  "HUC29_274", "HUC40_019", "HUC29_275"
)

edna_2023_complete <- edna_2023_complete %>%
  mutate(
    field_notes_lab = if_else(
      sample_id %in% samples_use_field_notes,
      field_notes_field,
      field_notes_lab
    )
  )

# Use lab notes for only these four samples
samples_use_lab_notes <- c(
  "HUC13_030", "HUC30_032", "HUC33_279", "HUC36_513"
)

edna_2023_complete <- edna_2023_complete %>%
  mutate(
    field_notes_field = if_else(
      sample_id %in% samples_use_lab_notes,
      field_notes_lab,
      field_notes_field
    )
  )
# ------------------------------------------------------

### Trim/consolidate/edit column names -----------------
## Fix 'analyze_for' column 
edna_2023_complete <- edna_2023_complete %>%
  mutate(
    analyze_for = coalesce(
      na_if(na_if(str_trim(analyze_for_lab), ""), "NA"),
      analyze_for_field
    ),
    .before = analyze_for_field
  ) %>%
  select(-analyze_for_field, -analyze_for_lab)

## Resort to feature_name from my field notes
edna_2023_complete <- edna_2023_complete %>%
  select(-feature_name_lab) %>%
  rename(feature_name = feature_name_field)

# Find field/lab column pairs
shared_cols <- names(edna_2023_complete) %>%
  str_subset("_field$") %>%
  str_remove("_field$")

shared_cols <- shared_cols[
  paste0(shared_cols, "_lab") %in% names(edna_2023_complete)
]

# Compare values and consolidate matching pairs
column_comparison <- tibble(
  column = character(),
  differences = integer()
)

for (col in shared_cols) {
  field_col <- paste0(col, "_field")
  lab_col   <- paste0(col, "_lab")
  
  matches <- vctrs::vec_equal(
    as.character(edna_2023_complete[[field_col]]),
    as.character(edna_2023_complete[[lab_col]]),
    na_equal = TRUE
  )
  
  column_comparison <- column_comparison %>%
    add_row(
      column = col,
      differences = sum(!matches)
    )
  
  if (all(matches)) {
    edna_2023_complete <- edna_2023_complete %>%
      select(-all_of(lab_col)) %>%
      rename_with(~ col, all_of(field_col))
  }
}

# Zero differences means the pair was consolidated
#print(column_comparison, n = Inf)
# ------------------------------------------------------

## QC for site IDs -------------------------------------
# Normalize IDs for comparison only
normalize_id <- function(x) {
  na_if(str_to_upper(str_trim(as.character(x))), "")
}

# Compare sample_id with the helper ID and lab site number
id_checks <- edna_2023_complete %>%
  transmute(
    sample_id,
    id_tag,
    sample_id_match,
    site_number,
    sample_norm = normalize_id(sample_id),
    match_norm = normalize_id(sample_id_match),
    site_norm = normalize_id(site_number)
  )

# Differences between sample_id and sample_id_match
sample_match_differences <- id_checks %>%
  filter(
    !vctrs::vec_equal(sample_norm, match_norm, na_equal = TRUE)
  ) %>%
  select(sample_id, id_tag, sample_id_match)

# Conflicting nonmissing sample_id and site_number values
site_number_conflicts <- id_checks %>%
  filter(
    !is.na(sample_norm),
    !is.na(site_norm),
    sample_norm != site_norm
  ) %>%
  select(sample_id, id_tag, site_number)

missing_ids <- id_checks %>%
  filter(
    is.na(sample_norm) |
      is.na(normalize_id(id_tag)) |
      is.na(site_norm)
  ) %>%
  select(sample_id, id_tag, site_number)

# Check both directions of the sample_id/id_tag relationship
id_pairs <- id_checks %>%
  transmute(
    sample_id = sample_norm,
    id_tag = normalize_id(id_tag)
  ) %>%
  filter(!is.na(sample_id), !is.na(id_tag)) %>%
  distinct()

# One sample_id linked to multiple id_tags
sample_to_multiple_tags <- id_pairs %>%
  add_count(sample_id, name = "n_id_tags") %>%
  filter(n_id_tags > 1)

# One id_tag linked to multiple sample_ids
tag_to_multiple_samples <- id_pairs %>%
  add_count(id_tag, name = "n_sample_ids") %>%
  filter(n_sample_ids > 1)

# Repeated sample rows, even when the ID pairing agrees
repeated_sample_rows <- id_checks %>%
  count(sample_norm, name = "n_rows") %>%
  filter(!is.na(sample_norm), n_rows > 1)

# Print all checks
id_qc <- list(
  sample_match_differences = sample_match_differences,
  site_number_conflicts = site_number_conflicts,
  missing_ids = missing_ids,
  sample_to_multiple_tags = sample_to_multiple_tags,
  tag_to_multiple_samples = tag_to_multiple_samples,
  repeated_sample_rows = repeated_sample_rows
)

id_qc

## Delete redundant columns. 
edna_2023_complete <- edna_2023_complete %>%
  select(-any_of(c(
    "sample_id_match",
    "site_number",
    "site_number_match",
    "zone"
  ))) %>%
  relocate(sample_id, id_tag)

# Identify repeated PCR metadata columns
repeated_pcr_cols <- names(edna_2023_complete) %>%
  str_subset(
    "^(species_code|std_crv|pcr_date|id_tag_spp|pcr_notes)_\\d+$"
  )

# Identify completely empty columns within those blocks
empty_pcr_cols <- repeated_pcr_cols[
  vapply(
    edna_2023_complete[repeated_pcr_cols],
    function(x) {
      values <- str_trim(as.character(x))
      all(is.na(values) | values == "")
    },
    logical(1)
  )
]

# Remove only the empty repeated PCR columns
edna_2023_complete <- edna_2023_complete %>%
  select(-all_of(empty_pcr_cols))

# Check how many columns were removed and the resulting dimensions
#length(empty_pcr_cols)
dim(edna_2023_complete)

################################################################################
################################################################################

#### QC: EDNA RESULTS, ANALYSIS INSTRUCTIONS, QPCR #############################

####  Does 'analyze_for' match the qPCR runs? ------------------------
# Assay labels accepted in analyze_for
assay_labels <- list(
  bul2 = c("BUL2", "BULL"),
  rmtf = "RMTF",
  scul = "SCUL"
)

# Split analyze_for into individual labels
analysis_labels <- str_split(
  str_to_upper(coalesce(edna_2023_complete$analyze_for, "")),
  "[,;/+&[:space:]]+"
)

# Check for labels not covered by the mapping above
known_labels <- c("DNE", unlist(assay_labels, use.names = FALSE))

unmapped_analysis_labels <- tibble(
  sample_id = edna_2023_complete$sample_id,
  analyze_for = edna_2023_complete$analyze_for,
  unmapped_labels = vapply(
    analysis_labels,
    function(x) paste(setdiff(x[nzchar(x)], known_labels), collapse = ", "),
    character(1)
  )
) %>%
  filter(
    is.na(analyze_for) |
      str_trim(analyze_for) == "" |
      unmapped_labels != ""
  )

# Identify NULL/empty cells; a cell containing NA is not NULL
is_null_cell <- function(x) {
  is.null(x) || length(x) == 0L
}

qpcr_cols <- names(edna_2023_complete) %>%
  str_subset("^qpcr_")

qpcr_null_qc <- tibble(
  sample_id = character(),
  analyze_for = character(),
  assay = character(),
  issue = character(),
  columns = character()
)

for (i in seq_len(nrow(edna_2023_complete))) {
  labels <- analysis_labels[[i]]
  
  # DNE: every qPCR cell should be NULL
  if ("DNE" %in% labels) {
    non_null_cols <- qpcr_cols[
      !vapply(
        qpcr_cols,
        function(col) is_null_cell(edna_2023_complete[[col]][[i]]),
        logical(1)
      )
    ]
    
    if (length(non_null_cols) > 0L) {
      qpcr_null_qc <- qpcr_null_qc %>%
        add_row(
          sample_id = edna_2023_complete$sample_id[i],
          analyze_for = edna_2023_complete$analyze_for[i],
          assay = "ALL",
          issue = "DNE sample has non-NULL qPCR cells",
          columns = paste(non_null_cols, collapse = ", ")
        )
    }
  }
  
  # Requested assays: no Ct cell should be NULL
  for (assay in names(assay_labels)) {
    if (!any(labels %in% assay_labels[[assay]])) next
    
    ct_cols <- names(edna_2023_complete) %>%
      str_subset(
        paste0(
          "^qpcr_", assay,
          "_(inhibition_ct|mean_ct|well_[0-9]+_ct)$"
        )
      )
    
    null_cols <- ct_cols[
      vapply(
        ct_cols,
        function(col) is_null_cell(edna_2023_complete[[col]][[i]]),
        logical(1)
      )
    ]
    
    if (length(ct_cols) == 0L || length(null_cols) > 0L) {
      qpcr_null_qc <- qpcr_null_qc %>%
        add_row(
          sample_id = edna_2023_complete$sample_id[i],
          analyze_for = edna_2023_complete$analyze_for[i],
          assay = str_to_upper(assay),
          issue = "Requested assay has missing Ct columns or NULL cells",
          columns = paste(null_cols, collapse = ", ")
        )
    }
  }
}

# Inspect exceptions
print(qpcr_null_qc, n = Inf, width = Inf)
print(unmapped_analysis_labels, n = Inf, width = Inf)

#View(qpcr_null_qc)

# Remove RMTF from analyze_for for this sample only
edna_2023_complete <- edna_2023_complete %>%
  mutate(
    analyze_for = if_else(
      sample_id == "HUC36_227",
      "BULL, SCUL",
      analyze_for
    )
  )
##################################################

#### QC: Does e_dna_result match lab amplification? ------------

## fix: change 'NONE' to 'NA' for 3 unanalyzed samples
edna_2023_complete <- edna_2023_complete %>%
  mutate(
    e_dna_result = if_else(
      sample_id %in% c("HUC09_167", "HUC35_055", "HUC35_515"),
      NA_character_,
      e_dna_result
    )
  )

# Summarize detection for each sample/assay across all runs
lab_detection <- qpcr %>%
  filter(
    !is.na(id_tag),
    assay %in% c("BUL2", "RMTF", "SCUL")
  ) %>%
  group_by(id_tag, assay) %>%
  summarise(
    counts_seen = paste(
      sort(unique(wells_amplified[!is.na(wells_amplified)])),
      collapse = ", "
    ),
    review_runs = any(is.na(wells_amplified)) |
      n_distinct(wells_amplified > 0, na.rm = TRUE) > 1,
    detected = any(wells_amplified > 0, na.rm = TRUE),
    .groups = "drop"
  )

# Build expected species lists for each lab ID
lab_results <- lab_detection %>%
  group_by(id_tag) %>%
  summarise(
    assays_recorded = paste(sort(unique(assay)), collapse = ", "),
    review_runs = any(review_runs),
    lab_expected = paste(
      sort(unique(assay[detected])),
      collapse = ", "
    ),
    .groups = "drop"
  )

# Standardize manual results: ignore order, spacing, and BULL/BUL2
# Treat NONE as no detections for comparison
normalize_result <- function(x) {
  vapply(x, function(value) {
    if (is.na(value)) return("")
    
    tokens <- str_split(
      str_to_upper(str_trim(value)),
      "[,;/+&[:space:]]+"
    )[[1]]
    
    tokens <- tokens[!tokens %in% c("", "NA", "NONE")]
    tokens[tokens == "BULL"] <- "BUL2"
    
    paste(sort(unique(tokens)), collapse = ", ")
  }, character(1))
}


# Compare manual species lists with lab-derived species lists
edna_result_qc <- edna_2023_complete %>%
  select(sample_id, id_tag, analyze_for, e_dna_result) %>%
  left_join(lab_results, by = "id_tag") %>%
  mutate(
    manual_raw = str_to_upper(
      str_trim(coalesce(e_dna_result, ""))
    ),
    manual_missing = manual_raw %in% c("", "NA"),
    manual_standardized = normalize_result(e_dna_result),
    qc_status = case_when(
      is.na(assays_recorded) & manual_raw == "NONE" ~
        "Review: NONE recorded without lab assays",
      is.na(assays_recorded) & manual_missing ~
        "No lab records; manual result missing",
      is.na(assays_recorded) ~
        "Manual result without lab records",
      review_runs ~
        "Review conflicting runs or missing counts",
      manual_missing ~
        "Manual result missing despite lab records",
      manual_standardized != lab_expected ~
        "Mismatch",
      lab_expected == "" ~
        "Match: no detections in recorded assays",
      TRUE ~ "Match"
    ),
    lab_expected = if_else(
      coalesce(review_runs, FALSE),
      NA_character_,
      lab_expected
    )
  )

# Print summary
edna_result_qc %>%
  count(qc_status, name = "n_samples") %>%
  print(n = Inf)

# Print samples needing review
edna_result_issues <- edna_result_qc %>%
  filter(
    !qc_status %in% c(
      "Match",
      "Match: no detections in recorded assays",
      "No lab records; manual result missing"
    )
  )

print(edna_result_issues, n = Inf, width = Inf)
View(edna_result_issues)

# Show assay counts and individual runs for flagged samples
qpcr %>%
  inner_join(
    edna_result_issues %>% select(sample_id, id_tag),
    by = "id_tag"
  ) %>%
  arrange(sample_id, assay, pcr_date) %>%
  select(
    sample_id, id_tag, assay, pcr_date,
    wells_amplified, wells, final_data, pcr_notes
  ) %>%
  print(n = Inf, width = Inf)

## ^these 2 samples (HUC30_032 and HUC30_034) were re-run for RMTF
## and did not amplify; their eDNA result correctly displays 'NONE'
##################################################

# Order the columns ---------------------------------------------
edna_2023_complete <- edna_2023_complete %>%
  select(
    sample_id,
    id_tag,
    analyze_for,
    standard_curve_y_n,
    e_dna_result,
    date_collected,
    collector_s,
    field_latitude,
    field_longitude,
    waterbody_type,
    feature_name,
    huc8_number,
    huc_8_name,
    state,
    agency,
    collaborator,
    field_notes,
    everything()
  )
# ----------------------------------------------------------------


















##### 2024 eDNA results
############################

#### Import lab results ----------------------------------------

folder_path_2024 <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/",
  "Masters_Project/Field_Data"
)

excel_file_2024 <- file.path(
  folder_path_2024,
  "eDNA_results_NGC",
  "Simantel_eDNAResults_updated_20Dec24.xlsx"
)

# Explicit names for the repeated species-result headers
lab_column_names_2024 <- c(
  "site_number",
  "feature_name",
  "waterbody_type",
  "collector_s",
  "state",
  "field_latitude",
  "field_longitude",
  "date_collected",
  "field_notes",
  "no_of_filters",
  "total_volume",
  "id_tag",
  "qpcr_bul2_detected",
  "qpcr_bul2_wells_amplified",
  "qpcr_bul2_mean_ct",
  "qpcr_rmtf_detected",
  "qpcr_rmtf_wells_amplified",
  "qpcr_rmtf_mean_ct",
  "qpcr_scul_detected",
  "qpcr_scul_wells_amplified",
  "qpcr_scul_mean_ct",
  "qpcr_brk_detected",
  "qpcr_brk_wells_amplified",
  "qpcr_brk_mean_ct",
  "lab_comments"
)

# Sample data begin in row 8.
# Preserve text counts such as "1/6" and "1/3, 5/6".
lab_2024 <- read_excel(
  excel_file_2024,
  sheet = 1,
  skip = 7,
  col_names = lab_column_names_2024,
  col_types = "text"
) %>%
  mutate(
    across(where(is.character), ~ na_if(str_trim(.x), ""))
  ) %>%
  filter(!is.na(site_number)) %>%
  mutate(
    site_number_match = str_to_upper(site_number)
  )


#### Import the field master: first tab ------------------------

field_file_2024 <- file.path(
  folder_path_2024,
  "2023-24_Field_Data_Results_Master_12.29.2024.xlsx"
)

field_2024_raw <- read_excel(
  field_file_2024,
  sheet = 1,
  range = cell_cols("A:AR"),
  col_types = "text"
) %>%
  clean_names()

# Keep sampled sites from 2024 only
field_2024 <- field_2024_raw %>%
  rename(
    sample_id = site_id,
    bull_wells_amplified = bul2_wells_amplified
  ) %>%
  mutate(
    sample_id = str_trim(sample_id),
    sample_id_match = str_to_upper(sample_id)
  ) %>%
  filter(
    sample_year == "2024",
    str_to_upper(str_trim(dry_sampled)) == "SAMPLED",
    !is.na(sample_id)
  )


#### Cross-reference sample IDs -------------------------------

samples_in_field_not_lab_2024 <- field_2024 %>%
  anti_join(
    lab_2024,
    by = c("sample_id_match" = "site_number_match")
  )

samples_in_lab_not_field_2024 <- lab_2024 %>%
  anti_join(
    field_2024,
    by = c("site_number_match" = "sample_id_match")
  )

# Field samples without a matching lab record
samples_in_field_not_lab_2024 %>%
  select(sample_id, analyze_for, e_dna_result, comments_for_lab) %>%
  print(n = Inf, width = Inf)

# Lab samples outside the 2024 field sample list
samples_in_lab_not_field_2024 %>%
  select(
    site_number, id_tag, date_collected,
    starts_with("qpcr_"), lab_comments
  ) %>%
  print(n = Inf, width = Inf)


#### Join field and lab tables --------------------------------

# Retain all sampled 2024 field sites.
# Stop if either input has repeated matching IDs.
edna_2024_complete <- field_2024 %>%
  left_join(
    lab_2024,
    by = c("sample_id_match" = "site_number_match"),
    suffix = c("_field", "_lab"),
    relationship = "one-to-one"
  ) %>%
  relocate(sample_id, id_tag)

# Dates and counts remain as imported text pending QC.
# Do not apply the sample-specific 2023 corrections here.


#### Inspect dimensions and column names ----------------------

dim(edna_2024_complete)

tibble(
  column_number = seq_along(edna_2024_complete),
  column_name = names(edna_2024_complete)
) %>%
  print(n = Inf, width = Inf)
# -------------------------------------------------------------

################################################################################
################################################################################
########   --------   QC: ADDRESSING DISCREPANCIES    ------------    ##########
################################################################################
################################################################################

#### 1. Three 2023 samples were analyzed later in 2024; update results --------
# Preserve the later lab results supporting these corrections
later_results_2023 <- lab_2024 %>%
  filter(
    site_number %in% c("HUC09_167", "HUC35_055", "HUC35_515")
  ) %>%
  mutate(
    result_source = "Simantel_eDNAResults_updated_20Dec24.xlsx"
  )

# Update these three samples in the 2023 table
edna_2023_complete <- edna_2023_complete %>%
  mutate(
    analyze_for = if_else(
      sample_id %in% later_results_2023$site_number,
      "BULL",
      analyze_for
    ),
    bull_wells_amplified = if_else(
      sample_id %in% later_results_2023$site_number,
      "0",
      bull_wells_amplified
    ),
    e_dna_result = if_else(
      sample_id %in% later_results_2023$site_number,
      "NONE",
      e_dna_result
    )
  )

# Verify the updates
edna_2023_complete %>%
  filter(sample_id %in% later_results_2023$site_number) %>%
  select(
    sample_id, id_tag, sample_year, analyze_for,
    bull_wells_amplified, e_dna_result
  ) %>%
  print(n = Inf, width = Inf)

#### Include supplemental results in the 2023 result QC 

# Combine original lab counts with the three later BUL2 results
# This is a result-count table, not a replicate Ct table.
lab_counts_2023_all <- bind_rows(
  qpcr %>%
    filter(
      !is.na(id_tag),
      assay %in% c("BUL2", "RMTF", "SCUL")
    ) %>%
    transmute(
      id_tag,
      assay,
      wells_amplified,
      result_source =
        "Simantel_eDNA_Results_Nov.29.2023_postQAQC.xlsx"
    ),
  
  later_results_2023 %>%
    transmute(
      id_tag,
      assay = "BUL2",
      wells_amplified = as.numeric(qpcr_bul2_wells_amplified),
      result_source
    )
)

# Check detection agreement across records
lab_detection_2023 <- lab_counts_2023_all %>%
  group_by(id_tag, assay) %>%
  summarise(
    review_runs = any(is.na(wells_amplified)) |
      n_distinct(wells_amplified > 0, na.rm = TRUE) > 1,
    detected = any(wells_amplified > 0, na.rm = TRUE),
    .groups = "drop"
  )

lab_results_2023 <- lab_detection_2023 %>%
  group_by(id_tag) %>%
  summarise(
    assays_recorded = paste(sort(unique(assay)), collapse = ", "),
    review_runs = any(review_runs),
    lab_expected = paste(
      sort(unique(assay[detected])),
      collapse = ", "
    ),
    .groups = "drop"
  )

# Recheck the updated 2023 results
# Uses normalize_result() already defined earlier in your script.
edna_result_qc_2023 <- edna_2023_complete %>%
  select(sample_id, id_tag, analyze_for, e_dna_result) %>%
  left_join(lab_results_2023, by = "id_tag") %>%
  mutate(
    manual_raw = str_to_upper(
      str_trim(coalesce(e_dna_result, ""))
    ),
    manual_missing = manual_raw %in% c("", "NA"),
    manual_standardized = normalize_result(e_dna_result),
    qc_status = case_when(
      is.na(assays_recorded) & manual_raw == "NONE" ~
        "Review: NONE recorded without lab assays",
      is.na(assays_recorded) & manual_missing ~
        "No lab records; manual result missing",
      is.na(assays_recorded) ~
        "Manual result without lab records",
      review_runs ~
        "Review conflicting runs or missing counts",
      manual_missing ~
        "Manual result missing despite lab records",
      manual_standardized != lab_expected ~
        "Mismatch",
      lab_expected == "" ~
        "Match: no detections in recorded assays",
      TRUE ~ "Match"
    ),
    lab_expected = if_else(
      coalesce(review_runs, FALSE),
      NA_character_,
      lab_expected
    )
  )

# Verify that the three corrected samples now pass
edna_result_qc_2023 %>%
  filter(sample_id %in% later_results_2023$site_number) %>%
  print(n = Inf, width = Inf)

# Updated summary for all 103 2023 samples
edna_result_qc_2023 %>%
  count(qc_status, name = "n_samples") %>%
  print(n = Inf)
# ------------------------------------------------------------------------------

#### 2. Incorporate consolidated analysis requests -----------------------------
request_file <- file.path(
  folder_path_2024,
  "2024_All_eDNA_Analysis_Requests_Simantel_11.12.2024.xlsx"
)

# Header-row positions verified in this workbook
request_layout <- tibble(
  sheet = c(
    "Request01_09.06.2024",
    "Request02_09.14.2024",
    "Request03_11.02.2024",
    "Request04_11.13.2024",
    "AllOtherSamples - DNE_11.13.24"
  ),
  header_row = c(5L, 5L, 6L, 6L, 5L)
)

# First 20 columns have the same underlying structure on each tab
request_column_names <- c(
  "analyze_for_requested",
  "standard_curve_requested",
  "sample_id_original",
  "feature_name",
  "state_collected",
  "collectors",
  "waterbody_type",
  "huc8_number",
  "huc_8_name",
  "zone",
  "latitude",
  "longitude",
  "date_collected",
  "collaborator",
  "agency",
  "agency_state",
  "field_notes",
  "no_of_filters",
  "filter_volumes_l",
  "comments"
)

# Preserve the original request records and their locations
request_records <- bind_rows(
  lapply(seq_len(nrow(request_layout)), function(i) {
    read_excel(
      request_file,
      sheet = request_layout$sheet[i],
      range = cell_limits(
        c(request_layout$header_row[i] + 1L, 1L),
        c(NA_integer_, 20L)
      ),
      col_names = request_column_names,
      col_types = "text"
    ) %>%
      mutate(
        request_sheet = request_layout$sheet[i],
        request_excel_row =
          row_number() + request_layout$header_row[i]
      )
  })
) %>%
  mutate(
    across(where(is.character), ~ na_if(str_trim(.x), "")),
    sample_id_match = str_to_upper(sample_id_original)
  ) %>%
  filter(!is.na(sample_id_match)) %>%
  mutate(
    # Confirmed historical alias: July sample became SLOLO_618
    sample_id_match = if_else(
      request_sheet == "AllOtherSamples - DNE_11.13.24" &
        sample_id_match == "SLOLO_347" &
        near(as.numeric(latitude), 46.73131) &
        near(as.numeric(longitude), -114.29544),
      "SLOLO_618",
      sample_id_match
    )
  )

# Standardize request terminology
normalize_request <- function(x) {
  vapply(x, function(value) {
    if (is.na(value) || str_trim(value) == "") {
      return(NA_character_)
    }
    
    value <- str_to_upper(str_trim(value))
    
    if (str_detect(value, "^DNE")) return("DNE")
    
    tokens <- str_split(value, "[,;/+&[:space:]]+")[[1]]
    tokens <- tokens[nzchar(tokens)]
    tokens[tokens == "BUL2"] <- "BULL"
    tokens[tokens == "BRKT"] <- "BRK"
    
    paste(sort(unique(tokens)), collapse = ", ")
  }, character(1))
}

request_records <- request_records %>%
  mutate(
    request_normalized = normalize_request(analyze_for_requested)
  )

# Combine repeated requests without counting reruns as new samples
analysis_requests <- request_records %>%
  group_by(sample_id_match) %>%
  summarise(
    has_dne = any(request_normalized == "DNE", na.rm = TRUE),
    has_analysis = any(
      !is.na(request_normalized) & request_normalized != "DNE"
    ),
    requested_species = paste(
      sort(unique(unlist(str_split(
        request_normalized[
          !is.na(request_normalized) &
            request_normalized != "DNE"
        ],
        ",\\s*"
      )))),
      collapse = ", "
    ),
    analysis_request_source = paste(
      sort(unique(paste0(
        request_sheet, " [row ", request_excel_row, "]"
      ))),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    analyze_for_requested = case_when(
      has_dne & has_analysis ~ NA_character_,
      has_dne ~ "DNE",
      has_analysis ~ requested_species,
      TRUE ~ NA_character_
    )
  )

# Stop rather than silently resolve conflicting instructions
request_conflicts <- analysis_requests %>%
  filter(has_dne & has_analysis)

print(request_conflicts, n = Inf, width = Inf)
stopifnot(nrow(request_conflicts) == 0L)


#### Update instructions in both final tables 

apply_analysis_requests <- function(data) {
  data %>%
    # Allow this section to be rerun
    select(-any_of(c(
      "analysis_request_source",
      "analysis_request_file"
    ))) %>%
    mutate(
      request_match_key = str_to_upper(str_trim(sample_id))
    ) %>%
    left_join(
      analysis_requests %>%
        select(
          request_match_key = sample_id_match,
          analyze_for_requested,
          analysis_request_source
        ),
      by = "request_match_key",
      relationship = "many-to-one"
    ) %>%
    mutate(
      # Keep existing instructions when this workbook has no entry
      analyze_for = coalesce(analyze_for_requested, analyze_for),
      analysis_request_file = if_else(
        !is.na(analysis_request_source),
        basename(request_file),
        NA_character_
      )
    ) %>%
    select(-request_match_key, -analyze_for_requested)
}

edna_2023_complete <- apply_analysis_requests(edna_2023_complete)
edna_2024_complete <- apply_analysis_requests(edna_2024_complete)


#### Verify the updates 

# Expected 2024 totals: 92 with assay requests; 55 DNE
edna_2024_complete %>%
  mutate(
    analysis_group = case_when(
      is.na(analyze_for) | str_trim(analyze_for) == "" ~
        "Missing instructions",
      analyze_for == "DNE" ~ "DNE",
      TRUE ~ "Assays requested"
    )
  ) %>%
  count(analysis_group, name = "n_samples") %>%
  print(n = Inf)

# Every sampled 2024 site should have a request-source record
edna_2024_complete %>%
  filter(is.na(analysis_request_source)) %>%
  select(sample_id, analyze_for) %>%
  print(n = Inf)

# DNE samples should not have positive OR negative result labels
# Expected: zero rows
dne_result_issues <- edna_2024_complete %>%
  filter(
    analyze_for == "DNE",
    !is.na(e_dna_result),
    !str_to_upper(str_trim(e_dna_result)) %in% c("", "NA")
  ) %>%
  select(sample_id, analyze_for, e_dna_result)

print(dne_result_issues, n = Inf)

# Verify the three later-analyzed 2023 samples remain correct
edna_2023_complete %>%
  filter(sample_id %in% later_results_2023$site_number) %>%
  select(
    sample_id, sample_year, analyze_for,
    bull_wells_amplified, e_dna_result,
    analysis_request_source
  ) %>%
  print(n = Inf, width = Inf)
# ------------------------------------------------------------------------------

#### 3. QC: 2024 manual counts versus lab counts -------------------------------
# Manual counts in long format
manual_counts_2024 <- edna_2024_complete %>%
  select(
    sample_id, id_tag, analyze_for,
    bull_wells_amplified,
    rmtf_wells_amplified,
    scul_wells_amplified,
    brk_wells_amplified
  ) %>%
  pivot_longer(
    cols = ends_with("_wells_amplified"),
    names_to = "manual_column",
    values_to = "manual_value"
  ) %>%
  mutate(
    assay = recode(
      manual_column,
      bull_wells_amplified = "BUL2",
      rmtf_wells_amplified = "RMTF",
      scul_wells_amplified = "SCUL",
      brk_wells_amplified = "BRK"
    )
  )

# Lab counts in long format
lab_counts_2024 <- edna_2024_complete %>%
  select(
    sample_id,
    matches("^qpcr_(bul2|rmtf|scul|brk)_wells_amplified$")
  ) %>%
  pivot_longer(
    cols = starts_with("qpcr_"),
    names_to = "assay",
    names_pattern = "^qpcr_(.*)_wells_amplified$",
    values_to = "lab_value"
  ) %>%
  mutate(assay = str_to_upper(assay))

# Standardize missing-value representations for comparison only
clean_count_text <- function(x) {
  value <- str_to_upper(str_trim(as.character(x)))
  value[value %in% c("", "NA", "<NULL>")] <- NA_character_
  value
}

count_qc_2024 <- manual_counts_2024 %>%
  left_join(
    lab_counts_2024,
    by = c("sample_id", "assay"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    manual_text = clean_count_text(manual_value),
    lab_text = clean_count_text(lab_value),
    
    # Manual values should be whole-number counts
    manual_valid = coalesce(
      str_detect(manual_text, "^[0-9]+$"),
      FALSE
    ),
    
    # Accept a single integer or a single numerator/denominator
    lab_single_valid = coalesce(
      str_detect(lab_text, "^[0-9]+(\\s*/\\s*[0-9]+)?$"),
      FALSE
    ),
    
    multiple_runs = coalesce(str_detect(lab_text, ","), FALSE),
    
    manual_count = if_else(
      manual_valid,
      suppressWarnings(as.numeric(manual_text)),
      NA_real_
    ),
    
    lab_count = if_else(
      lab_single_valid,
      as.numeric(str_extract(lab_text, "^[0-9]+")),
      NA_real_
    ),
    
    lab_wells_tested = if_else(
      lab_single_valid & coalesce(str_detect(lab_text, "/"), FALSE),
      as.numeric(str_extract(lab_text, "[0-9]+$")),
      NA_real_
    ),
    
    qc_status = case_when(
      is.na(manual_text) & is.na(lab_text) ~
        "No counts recorded",
      !is.na(manual_text) & !manual_valid ~
        "Review manual count format",
      multiple_runs ~
        "Review multiple lab runs",
      !is.na(lab_text) & !lab_single_valid ~
        "Review lab count format",
      !is.na(lab_wells_tested) &
        (lab_wells_tested == 0 | lab_count > lab_wells_tested) ~
        "Invalid lab fraction",
      is.na(lab_text) ~
        "Manual count without lab count",
      is.na(manual_text) ~
        "Lab count without manual count",
      manual_count != lab_count ~
        "Count mismatch",
      TRUE ~ "Match"
    )
  ) %>%
  select(
    sample_id, id_tag, analyze_for, assay,
    manual_value, lab_value,
    manual_count, lab_count, lab_wells_tested,
    qc_status
  )

# Summary by species
count_qc_2024 %>%
  count(assay, qc_status, name = "n_samples") %>%
  print(n = Inf)

# Isolate exceptions
count_issues_2024 <- count_qc_2024 %>%
  filter(!qc_status %in% c("Match", "No counts recorded")) %>%
  arrange(sample_id, assay)

print(count_issues_2024, n = Inf, width = Inf)
#View(count_issues_2024)

# Replace only the 6 flagged manual entries with their lab values
for (species in c("bull", "rmtf", "scul", "brk")) {
  
  assay_code <- switch(
    species,
    bull = "BUL2",
    rmtf = "RMTF",
    scul = "SCUL",
    brk = "BRK"
  )
  
  manual_col <- paste0(species, "_wells_amplified")
  lab_col <- paste0(
    "qpcr_", str_to_lower(assay_code), "_wells_amplified"
  )
  
  flagged_samples <- count_issues_2024 %>%
    filter(assay == assay_code) %>%
    pull(sample_id)
  
  replace_rows <- edna_2024_complete$sample_id %in% flagged_samples
  
  edna_2024_complete[[manual_col]][replace_rows] <-
    edna_2024_complete[[lab_col]][replace_rows]
}

# Verify the affected samples
edna_2024_complete %>%
  filter(sample_id %in% count_issues_2024$sample_id) %>%
  select(
    sample_id,
    bull_wells_amplified,
    rmtf_wells_amplified,
    scul_wells_amplified,
    brk_wells_amplified
  ) %>%
  print(n = Inf, width = Inf)

#### Recheck counts after adopting the lab values 

species_map <- c(
  bull = "BUL2",
  rmtf = "RMTF",
  scul = "SCUL",
  brk = "BRK"
)

# Rebuild from the current table, not the earlier QC objects
count_qc_2024 <- bind_rows(
  lapply(names(species_map), function(species) {
    assay_code <- species_map[[species]]
    
    edna_2024_complete %>%
      transmute(
        sample_id,
        id_tag,
        assay = assay_code,
        manual_value = .data[[paste0(
          species, "_wells_amplified"
        )]],
        lab_value = .data[[paste0(
          "qpcr_", str_to_lower(assay_code), "_wells_amplified"
        )]]
      )
  })
) %>%
  mutate(
    manual_text = str_remove_all(
      clean_count_text(manual_value), "\\s+"
    ),
    lab_text = str_remove_all(
      clean_count_text(lab_value), "\\s+"
    ),
    
    # Permit a whole-number manual count to match a single fraction
    numerator_match = coalesce(
      str_detect(manual_text, "^[0-9]+$") &
        str_detect(lab_text, "^[0-9]+/[1-9][0-9]*$") &
        as.numeric(str_extract(manual_text, "^[0-9]+")) ==
        as.numeric(str_extract(lab_text, "^[0-9]+")),
      FALSE
    ),
    
    qc_status = case_when(
      is.na(manual_text) & is.na(lab_text) ~
        "No counts recorded",
      is.na(manual_text) ~
        "Lab count without manual count",
      is.na(lab_text) ~
        "Manual count without lab count",
      manual_text == lab_text | numerator_match ~
        "Match",
      TRUE ~ "Review difference"
    )
  )

# Summary
count_qc_2024 %>%
  count(assay, qc_status, name = "n_samples") %>%
  print(n = Inf)

# Remaining exceptions
count_issues_2024 <- count_qc_2024 %>%
  filter(!qc_status %in% c("Match", "No counts recorded"))

print(count_issues_2024, n = Inf, width = Inf)
## -----------------------------------------------------------------------------

#### 4. QC: 2024 eDNA species lists versus lab detections ----------------------
# Assemble lab detection flags and count availability
lab_detection_2024 <- bind_rows(
  lapply(names(species_map), function(species) {
    assay_code <- species_map[[species]]
    lab_prefix <- paste0("qpcr_", str_to_lower(assay_code))
    
    edna_2024_complete %>%
      transmute(
        sample_id,
        assay = assay_code,
        detected = str_to_upper(str_trim(
          .data[[paste0(lab_prefix, "_detected")]]
        )),
        lab_count = clean_count_text(
          .data[[paste0(lab_prefix, "_wells_amplified")]]
        )
      )
  })
) %>%
  mutate(
    detected = na_if(detected, ""),
    has_record = !is.na(detected) | !is.na(lab_count),
    invalid_flag = has_record &
      (is.na(detected) | !detected %in% c("Y", "N"))
  )

# Build expected species lists from the lab flags
lab_species_2024 <- lab_detection_2024 %>%
  group_by(sample_id) %>%
  summarise(
    n_assays_recorded = sum(has_record),
    review_flags = any(invalid_flag),
    assays_recorded = paste(
      sort(assay[has_record]),
      collapse = ", "
    ),
    lab_expected = paste(
      sort(assay[!is.na(detected) & detected == "Y"]),
      collapse = ", "
    ),
    .groups = "drop"
  )

# Compare with manual results
# Uses normalize_result() defined earlier in your script.
edna_result_qc_2024 <- edna_2024_complete %>%
  select(sample_id, id_tag, analyze_for, e_dna_result) %>%
  left_join(
    lab_species_2024,
    by = "sample_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    manual_raw = str_to_upper(
      str_trim(coalesce(e_dna_result, ""))
    ),
    manual_missing = manual_raw %in% c("", "NA"),
    manual_standardized = normalize_result(e_dna_result),
    
    qc_status = case_when(
      analyze_for == "DNE" & n_assays_recorded > 0 ~
        "Review: DNE sample has lab results",
      analyze_for == "DNE" & !manual_missing ~
        "Review: DNE sample has a manual result",
      analyze_for == "DNE" & n_assays_recorded == 0 ~
        "DNE: no results expected",
      n_assays_recorded == 0 ~
        "Review: no lab results",
      review_flags ~
        "Review: missing or unrecognized lab detection flag",
      manual_missing ~
        "Manual result missing despite lab results",
      manual_standardized != lab_expected ~
        "Species-list mismatch",
      lab_expected == "" ~
        "Match: no detections in recorded assays",
      TRUE ~ "Match"
    )
  )

# Summary
edna_result_qc_2024 %>%
  count(qc_status, name = "n_samples") %>%
  print(n = Inf)

# Exceptions
edna_result_issues_2024 <- edna_result_qc_2024 %>%
  filter(
    !qc_status %in% c(
      "Match",
      "Match: no detections in recorded assays",
      "DNE: no results expected"
    )
  )

print(edna_result_issues_2024, n = Inf, width = Inf)
#View(edna_result_issues_2024)
# ------------------------------------------------------------------------------

#### 5. QC: Internal agreement of 2024 lab results ---------------
# One row per sample and assay
lab_internal_qc_2024 <- edna_2024_complete %>%
  select(
    sample_id,
    id_tag,
    matches(
      "^qpcr_(bul2|rmtf|scul|brk)_(detected|wells_amplified|mean_ct)$"
    )
  ) %>%
  pivot_longer(
    cols = starts_with("qpcr_"),
    names_to = c("assay", ".value"),
    names_pattern = "^qpcr_([^_]+)_(.*)$"
  ) %>%
  mutate(
    assay = str_to_upper(assay),
    across(
      c(detected, wells_amplified, mean_ct),
      clean_count_text
    )
  )

# Review one assay record, preserving multiple-run information
check_lab_agreement <- function(flag, counts, ct) {
  
  if (all(is.na(c(flag, counts, ct)))) {
    return("No lab results")
  }
  
  if (is.na(flag) || !flag %in% c("Y", "N")) {
    return("Review: missing or unrecognized detection flag")
  }
  
  if (is.na(counts) || is.na(ct)) {
    return("Review: missing count or mean Ct")
  }
  
  # Split run entries, retaining empty tokens for validation
  count_parts <- str_split(counts, ",")[[1]] %>%
    str_remove_all("\\s+")
  
  # Mean Ct entries use commas or slashes to separate run means
  ct_parts <- str_split(ct, "[,/]")[[1]] %>%
    str_trim()
  
  if (!all(str_detect(
    count_parts,
    "^[0-9]+(/[1-9][0-9]*)?$"
  ))) {
    return("Review: unrecognized well-count format")
  }
  
  positive_wells <- as.numeric(
    str_extract(count_parts, "^[0-9]+")
  )
  
  fraction_entries <- str_detect(count_parts, "/")
  denominators <- as.numeric(
    str_extract(count_parts[fraction_entries], "[0-9]+$")
  )
  
  if (any(positive_wells[fraction_entries] > denominators)) {
    return("Review: positive wells exceed wells tested")
  }
  
  ct_values <- suppressWarnings(as.numeric(ct_parts))
  
  if (any(!is.finite(ct_values)) || any(ct_values < 0)) {
    return("Review: unrecognized or invalid mean Ct")
  }
  
  # Do not decide which run supersedes another
  if (any(positive_wells > 0) && any(positive_wells == 0)) {
    return("Review: runs disagree about amplification")
  }
  
  if (all(positive_wells == 0)) {
    if (flag != "N") {
      return("Mismatch: detection Y with zero amplified wells")
    }
    
    if (any(ct_values > 0)) {
      return("Mismatch: positive mean Ct with zero amplified wells")
    }
    
    return("Match: no amplification")
  }
  
  if (flag != "Y") {
    return("Mismatch: detection N with amplified wells")
  }
  
  if (any(ct_values == 0)) {
    return("Mismatch: zero mean Ct with amplified wells")
  }
  
  return("Match: amplification")
}

lab_internal_qc_2024$qc_status <- vapply(
  seq_len(nrow(lab_internal_qc_2024)),
  function(i) {
    check_lab_agreement(
      lab_internal_qc_2024$detected[i],
      lab_internal_qc_2024$wells_amplified[i],
      lab_internal_qc_2024$mean_ct[i]
    )
  },
  character(1)
)

# Summary by assay
lab_internal_qc_2024 %>%
  count(assay, qc_status, name = "n_samples") %>%
  print(n = Inf)

# Exceptions
lab_internal_issues_2024 <- lab_internal_qc_2024 %>%
  filter(
    !qc_status %in% c(
      "No lab results",
      "Match: no amplification",
      "Match: amplification"
    )
  )

print(lab_internal_issues_2024, n = Inf, width = Inf)
## -----------------------------------------------------------------------------

#### 6. QC: Compare 2024 collection dates -----------------------
parse_collection_date <- function(x) {
  
  text <- str_trim(as.character(x))
  text[text %in% c("", "NA", "<null>")] <- NA_character_
  
  result <- rep(as.Date(NA), length(text))
  
  # Excel serial dates, including serials with time fractions
  serial_rows <- which(
    !is.na(text) &
      str_detect(text, "^[0-9]{5}(\\.[0-9]+)?$")
  )
  
  result[serial_rows] <- as.Date(
    floor(as.numeric(text[serial_rows])),
    origin = "1899-12-30"
  )
  
  # ISO dates, optionally followed by a time
  iso_rows <- which(
    !is.na(text) &
      str_detect(text, "^\\d{4}-\\d{2}-\\d{2}([ T].*)?$")
  )
  
  result[iso_rows] <- as.Date(
    str_sub(text[iso_rows], 1, 10),
    format = "%Y-%m-%d"
  )
  
  # Month/day/year dates, optionally followed by a time
  slash_rows <- which(
    !is.na(text) &
      str_detect(text, "^\\d{1,2}/\\d{1,2}/\\d{4}([ T].*)?$")
  )
  
  result[slash_rows] <- as.Date(
    str_remove(text[slash_rows], "[ T].*$"),
    format = "%m/%d/%Y"
  )
  
  result
}

# Preserve original representations beside parsed dates
date_qc_2024 <- edna_2024_complete %>%
  transmute(
    sample_id,
    id_tag,
    field_date_original = date_collected_field,
    lab_date_original = date_collected_lab,
    field_date = parse_collection_date(date_collected_field),
    lab_date = parse_collection_date(date_collected_lab)
  ) %>%
  mutate(
    qc_status = case_when(
      !is.na(clean_count_text(field_date_original)) &
        is.na(field_date) ~
        "Review: field date could not be parsed",
      
      !is.na(clean_count_text(lab_date_original)) &
        is.na(lab_date) ~
        "Review: lab date could not be parsed",
      
      is.na(field_date) ~
        "Review: field date missing",
      
      is.na(lab_date) ~
        "No lab collection date",
      
      field_date != lab_date ~
        "Date mismatch",
      
      TRUE ~ "Match"
    )
  )

# Summary
date_qc_2024 %>%
  count(qc_status, name = "n_samples") %>%
  print(n = Inf)

# Parsing problems or conflicting dates
date_issues_2024 <- date_qc_2024 %>%
  filter(
    !qc_status %in% c("Match", "No lab collection date")
  )

print(date_issues_2024, n = Inf, width = Inf)

# Compare the conflicting dates with MMDDYY encoded in id_tag
date_resolution_2024 <- date_issues_2024 %>%
  mutate(
    id_date_text = str_match(id_tag, "^MT_(\\d{6})_")[, 2],
    id_date = as.Date(id_date_text, format = "%m%d%y"),
    lab_matches_id = lab_date == id_date
  ) %>%
  select(
    sample_id, id_tag,
    field_date, lab_date, id_date, lab_matches_id
  )

print(date_resolution_2024, n = Inf, width = Inf)

date_resolution_2024 %>%
  count(lab_matches_id, name = "n_samples") %>%
  print(n = Inf)

# Use confirmed lab dates for the 28 mismatches;
# retain field dates for all other samples.
edna_2024_complete <- edna_2024_complete %>%
  mutate(
    date_collected = if_else(
      sample_id %in% date_issues_2024$sample_id,
      parse_collection_date(date_collected_lab),
      parse_collection_date(date_collected_field)
    ),
    .before = date_collected_field
  ) %>%
  select(-date_collected_field, -date_collected_lab)

# Verify the single date column
edna_2024_complete %>%
  select(sample_id, date_collected) %>%
  print(n = Inf, width = Inf)
## -----------------------------------------------------------------------------

#### 7. Consolidate 2024 state columns --------------------------
# Compare normalized state values
state_qc_2024 <- edna_2024_complete %>%
  transmute(
    sample_id,
    field_state_1 = na_if(str_to_upper(str_trim(state_5)), ""),
    field_state_2 = na_if(str_to_upper(str_trim(state_16)), ""),
    lab_state = na_if(str_to_upper(str_trim(state)), "")
  ) %>%
  rowwise() %>%
  mutate(
    n_states = n_distinct(
      c(field_state_1, field_state_2, lab_state),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

state_conflicts_2024 <- state_qc_2024 %>%
  filter(n_states > 1)

print(state_conflicts_2024, n = Inf, width = Inf)

# Stop if the sources disagree
stopifnot(nrow(state_conflicts_2024) == 0L)

# Prefer available field values, then the lab value
edna_2024_complete <- edna_2024_complete %>%
  mutate(
    state = coalesce(
      na_if(str_to_upper(str_trim(state_5)), ""),
      na_if(str_to_upper(str_trim(state_16)), ""),
      na_if(str_to_upper(str_trim(state)), "")
    )
  ) %>%
  select(-state_5, -state_16)

# Verify the consolidated column
edna_2024_complete %>%
  count(state, name = "n_samples") %>%
  print(n = Inf)
# ------------------------------------------------------------------------------

#### 8. QC: Compare 2024 coordinates & consolidate ----------------------------
coordinate_differences_2024 <- bind_rows(
  lapply(c("field_latitude", "field_longitude"), function(col) {
    
    edna_2024_complete %>%
      # Exclude samples without matching lab records
      filter(!is.na(id_tag)) %>%
      transmute(
        sample_id,
        id_tag,
        coordinate = col,
        field_value = as.numeric(.data[[paste0(col, "_field")]]),
        lab_value = as.numeric(.data[[paste0(col, "_lab")]])
      ) %>%
      filter(
        !vctrs::vec_equal(
          field_value, lab_value, na_equal = TRUE
        )
      )
  })
)

#print(coordinate_differences_2024, n = Inf, width = Inf)

# Keep field coordinates and remove matching lab versions
edna_2024_complete <- edna_2024_complete %>%
  select(-field_latitude_lab, -field_longitude_lab) %>%
  rename(
    field_latitude = field_latitude_field,
    field_longitude = field_longitude_field
  ) %>%
  mutate(
    across(c(field_latitude, field_longitude), as.numeric)
  )
# ------------------------------------------------------------------------------

#### 9. QC: Compare feature names and waterbody types ------------
feature_differences_2024 <- bind_rows(
  lapply(c("feature_name", "waterbody_type"), function(col) {
    
    edna_2024_complete %>%
      filter(!is.na(id_tag)) %>%
      transmute(
        sample_id,
        column = col,
        field_value = str_squish(.data[[paste0(col, "_field")]]),
        lab_value = str_squish(.data[[paste0(col, "_lab")]])
      ) %>%
      filter(
        !vctrs::vec_equal(
          field_value, lab_value, na_equal = TRUE
        )
      )
  })
)

#print(feature_differences_2024, n = Inf, width = Inf)
# Consolidate features only if their comparison passed
stopifnot(nrow(feature_differences_2024) == 0L)

edna_2024_complete <- edna_2024_complete %>%
  select(-feature_name_lab, -waterbody_type_lab) %>%
  rename(
    feature_name = feature_name_field,
    waterbody_type = waterbody_type_field
  )
#-------------------------------------------------------------------------------

#### 10. QC: Compare collectors --------------------------------
collector_differences_2024 <- edna_2024_complete %>%
  filter(!is.na(id_tag)) %>%
  transmute(
    sample_id,
    field_value = collector_s_field,
    lab_value = collector_s_lab,
    field_comparison = str_remove_all(
      str_to_upper(field_value), "\\s+"
    ),
    lab_comparison = str_remove_all(
      str_to_upper(lab_value), "\\s+"
    )
  ) %>%
  filter(
    !vctrs::vec_equal(
      field_comparison, lab_comparison, na_equal = TRUE
    )
  ) %>%
  select(sample_id, field_value, lab_value)

#print(collector_differences_2024, n = Inf, width = Inf)
# Keep field collectors, including samples without lab records
edna_2024_complete <- edna_2024_complete %>%
  select(-collector_s_lab) %>%
  rename(collector_s = collector_s_field)
# ------------------------------------------------------------------------------

#### 11. Final metadata QC and consolidation --------------------
# Preserve a copy before consolidation
edna_2024_before_consolidation <- edna_2024_complete

clean_metadata <- function(x) {
  na_if(str_squish(as.character(x)), "")
}

# Check IDs before removing redundant columns
id_check_2024 <- edna_2024_complete %>%
  transmute(
    sample_id,
    id_tag,
    sample_key = str_to_upper(str_trim(sample_id)),
    tag_key = na_if(str_to_upper(str_trim(id_tag)), "")
  )

duplicate_samples_2024 <- id_check_2024 %>%
  count(sample_key) %>%
  filter(is.na(sample_key) | sample_key == "" | n > 1)

duplicate_tags_2024 <- id_check_2024 %>%
  filter(!is.na(tag_key)) %>%
  count(tag_key) %>%
  filter(n > 1)

print(duplicate_samples_2024, n = Inf)
print(duplicate_tags_2024, n = Inf)

stopifnot(
  nrow(duplicate_samples_2024) == 0L,
  nrow(duplicate_tags_2024) == 0L
)

# Confirm lab site numbers agree with field sample IDs
if ("site_number" %in% names(edna_2024_complete)) {
  
  site_id_conflicts_2024 <- edna_2024_complete %>%
    filter(
      !is.na(site_number),
      str_to_upper(str_trim(sample_id)) !=
        str_to_upper(str_trim(site_number))
    ) %>%
    select(sample_id, id_tag, site_number)
  
  print(site_id_conflicts_2024, n = Inf)
  stopifnot(nrow(site_id_conflicts_2024) == 0L)
}


# Identify remaining field/lab column pairs
remaining_pairs <- names(edna_2024_complete) %>%
  str_subset("_field$") %>%
  str_remove("_field$")

remaining_pairs <- remaining_pairs[
  paste0(remaining_pairs, "_lab") %in%
    names(edna_2024_complete)
]

# Notes are merged separately to preserve distinct information
note_columns <- c("field_notes", "lab_comments")

# Check other metadata for conflicting nonmissing values
metadata_conflicts_2024 <- tibble(
  sample_id = character(),
  column = character(),
  field_value = character(),
  lab_value = character()
)

for (col in setdiff(remaining_pairs, note_columns)) {
  
  field_values <- clean_metadata(
    edna_2024_complete[[paste0(col, "_field")]]
  )
  
  lab_values <- clean_metadata(
    edna_2024_complete[[paste0(col, "_lab")]]
  )
  
  # Compare filter counts numerically
  if (col == "no_of_filters") {
    field_compare <- as.numeric(field_values)
    lab_compare <- as.numeric(lab_values)
  } else {
    field_compare <- field_values
    lab_compare <- lab_values
  }
  
  conflicts <- !is.na(field_values) &
    !is.na(lab_values) &
    !vctrs::vec_equal(
      field_compare, lab_compare, na_equal = TRUE
    )
  
  metadata_conflicts_2024 <- bind_rows(
    metadata_conflicts_2024,
    tibble(
      sample_id = edna_2024_complete$sample_id[conflicts],
      column = col,
      field_value = field_values[conflicts],
      lab_value = lab_values[conflicts]
    )
  )
}

print(metadata_conflicts_2024, n = Inf, width = Inf)

# Do not silently choose between conflicting metadata
stopifnot(nrow(metadata_conflicts_2024) == 0L)

# Preserve both sources when notes contain distinct information
combine_notes <- function(field, lab) {
  
  field <- clean_metadata(field)
  lab <- clean_metadata(lab)
  
  vapply(seq_along(field), function(i) {
    
    if (is.na(field[i]) && is.na(lab[i])) {
      return(NA_character_)
    }
    
    if (is.na(field[i])) return(lab[i])
    if (is.na(lab[i])) return(field[i])
    if (field[i] == lab[i]) return(field[i])
    
    paste0(
      "Field record: ", field[i],
      "\nLab record: ", lab[i]
    )
  }, character(1))
}

# Consolidate the remaining pairs
for (col in remaining_pairs) {
  
  field_col <- paste0(col, "_field")
  lab_col <- paste0(col, "_lab")
  
  if (col %in% note_columns) {
    consolidated <- combine_notes(
      edna_2024_complete[[field_col]],
      edna_2024_complete[[lab_col]]
    )
  } else {
    consolidated <- coalesce(
      clean_metadata(edna_2024_complete[[field_col]]),
      clean_metadata(edna_2024_complete[[lab_col]])
    )
  }
  
  edna_2024_complete[[field_col]] <- consolidated
  
  edna_2024_complete <- edna_2024_complete %>%
    select(-all_of(lab_col)) %>%
    rename_with(~ col, all_of(field_col))
}

# Standardize numeric metadata
numeric_metadata <- intersect(
  c(
    "field_latitude", "field_longitude",
    "snapped_latitude", "snapped_longitude",
    "no_of_filters", "total_volume",
    "huc8_number", "sample_year"
  ),
  names(edna_2024_complete)
)

edna_2024_complete <- edna_2024_complete %>%
  mutate(
    across(all_of(numeric_metadata), as.numeric),
    agency = str_replace(
      str_trim(agency),
      "^USFS-\\s*ALWRI$",
      "USFS-ALWRI"
    )
  ) %>%
  select(-any_of(c(
    "sample_id_match",
    "site_number",
    "site_number_match",
    "zone"
  )))

# Apply the same leading-column order to both final tables
final_column_order <- c(
  "sample_id",
  "id_tag",
  "analyze_for",
  "standard_curve_y_n",
  "e_dna_result",
  "date_collected",
  "collector_s",
  "field_latitude",
  "field_longitude",
  "waterbody_type",
  "feature_name",
  "huc8_number",
  "huc_8_name",
  "state",
  "agency",
  "collaborator",
  "field_notes"
)

edna_2023_complete <- edna_2023_complete %>%
  select(all_of(final_column_order), everything())

edna_2024_complete <- edna_2024_complete %>%
  select(all_of(final_column_order), everything())

# Final metadata checks
stopifnot(
  nrow(edna_2023_complete) == 103L,
  nrow(edna_2024_complete) == 147L,
  inherits(edna_2024_complete$date_collected, "Date"),
  !anyNA(edna_2024_complete$date_collected)
)

# Expected: no leftover paired metadata columns
names(edna_2024_complete) %>%
  str_subset("_(field|lab)$") %>%
  print()

dim(edna_2023_complete)
dim(edna_2024_complete)
# ------------------------------------------------------------------------------

#### 12. Final QC: 2024 eDNA results and amplification ------------
# Required helper functions were defined earlier in the script
stopifnot(
  is.function(clean_count_text),
  is.function(normalize_result),
  is.function(check_lab_agreement)
)

species_map <- c(
  bull = "BUL2",
  rmtf = "RMTF",
  scul = "SCUL",
  brk = "BRK"
)

# Rebuild every comparison from the final consolidated table
assay_qc_2024 <- bind_rows(
  lapply(names(species_map), function(species) {
    
    assay_code <- species_map[[species]]
    prefix <- paste0("qpcr_", str_to_lower(assay_code))
    
    request_pattern <- switch(
      assay_code,
      BUL2 = "\\b(BULL|BUL2)\\b",
      BRK = "\\b(BRK|BRKT)\\b",
      paste0("\\b", assay_code, "\\b")
    )
    
    edna_2024_complete %>%
      transmute(
        sample_id,
        id_tag,
        analyze_for,
        assay = assay_code,
        requested = str_detect(
          str_to_upper(coalesce(analyze_for, "")),
          request_pattern
        ),
        manual_value = .data[[paste0(
          species, "_wells_amplified"
        )]],
        lab_value = .data[[paste0(prefix, "_wells_amplified")]],
        detected = .data[[paste0(prefix, "_detected")]],
        mean_ct = .data[[paste0(prefix, "_mean_ct")]]
      )
  })
) %>%
  mutate(
    manual_text = str_remove_all(
      clean_count_text(manual_value), "\\s+"
    ),
    lab_text = str_remove_all(
      clean_count_text(lab_value), "\\s+"
    ),
    detected = clean_count_text(detected),
    mean_ct = clean_count_text(mean_ct),
    
    has_record = !is.na(lab_text) |
      !is.na(detected) |
      !is.na(mean_ct),
    
    coverage_status = case_when(
      requested & !has_record ~ "Requested assay missing",
      !requested & has_record ~ "Lab result outside instructions",
      TRUE ~ "Match"
    ),
    
    # Whole-number counts may match a single lab fraction.
    # Multiple-run entries must retain the same complete text.
    numerator_match = coalesce(
      str_detect(manual_text, "^[0-9]+$") &
        str_detect(lab_text, "^[0-9]+/[1-9][0-9]*$") &
        as.numeric(str_extract(manual_text, "^[0-9]+")) ==
        as.numeric(str_extract(lab_text, "^[0-9]+")),
      FALSE
    ),
    
    count_status = case_when(
      is.na(manual_text) & is.na(lab_text) ~
        "No counts recorded",
      is.na(manual_text) ~
        "Manual count missing",
      is.na(lab_text) ~
        "Lab count missing",
      manual_text == lab_text | numerator_match ~
        "Match",
      TRUE ~ "Count mismatch"
    )
  )

# check_lab_agreement() must contain the updated Ct splitter:
# ct_parts <- str_split(ct, "[,/]")[[1]] %>% str_trim()
assay_qc_2024$lab_status <- vapply(
  seq_len(nrow(assay_qc_2024)),
  function(i) {
    check_lab_agreement(
      assay_qc_2024$detected[i],
      assay_qc_2024$lab_value[i],
      assay_qc_2024$mean_ct[i]
    )
  },
  character(1)
)

# Assay-level exceptions
assay_issues_2024 <- assay_qc_2024 %>%
  filter(
    coverage_status != "Match" |
      !count_status %in% c("Match", "No counts recorded") |
      !lab_status %in% c(
        "No lab results",
        "Match: amplification",
        "Match: no amplification"
      )
  )


# Derive expected species lists from lab detection flags
lab_species_final_2024 <- assay_qc_2024 %>%
  group_by(sample_id) %>%
  summarise(
    n_assays_recorded = sum(has_record),
    lab_review = any(
      !lab_status %in% c(
        "No lab results",
        "Match: amplification",
        "Match: no amplification"
      )
    ),
    lab_expected = paste(
      sort(assay[!is.na(detected) & detected == "Y"]),
      collapse = ", "
    ),
    .groups = "drop"
  )

edna_result_qc_2024 <- edna_2024_complete %>%
  select(sample_id, id_tag, analyze_for, e_dna_result) %>%
  left_join(
    lab_species_final_2024,
    by = "sample_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    manual_raw = str_to_upper(
      str_trim(coalesce(e_dna_result, ""))
    ),
    manual_missing = manual_raw %in% c("", "NA"),
    manual_standardized = normalize_result(e_dna_result),
    
    qc_status = case_when(
      analyze_for == "DNE" & n_assays_recorded > 0 ~
        "Review: DNE sample has lab results",
      analyze_for == "DNE" & !manual_missing ~
        "Review: DNE sample has a manual result",
      analyze_for == "DNE" & n_assays_recorded == 0 ~
        "DNE: no results expected",
      n_assays_recorded == 0 ~
        "Review: no lab results",
      lab_review ~
        "Review lab inconsistencies first",
      manual_missing ~
        "Manual result missing",
      manual_standardized != lab_expected ~
        "Species-list mismatch",
      lab_expected == "" ~
        "Match: no detections in recorded assays",
      TRUE ~ "Match"
    )
  )

edna_result_issues_2024 <- edna_result_qc_2024 %>%
  filter(
    !qc_status %in% c(
      "Match",
      "Match: no detections in recorded assays",
      "DNE: no results expected"
    )
  )

#### Print the final accuracy checks ---------------------------

assay_qc_2024 %>%
  count(assay, count_status, name = "n_samples") %>%
  print(n = Inf)

edna_result_qc_2024 %>%
  count(qc_status, name = "n_samples") %>%
  print(n = Inf)

tibble(
  check = c(
    "Requested assays versus lab availability",
    "Manual versus lab amplification counts",
    "Internal lab summary consistency",
    "Manual eDNA species lists versus lab flags"
  ),
  exceptions = c(
    sum(assay_qc_2024$coverage_status != "Match"),
    sum(!assay_qc_2024$count_status %in%
          c("Match", "No counts recorded")),
    sum(!assay_qc_2024$lab_status %in% c(
      "No lab results",
      "Match: amplification",
      "Match: no amplification"
    )),
    nrow(edna_result_issues_2024)
  )
) %>%
  print(n = Inf)

print(assay_issues_2024, n = Inf, width = Inf)
print(edna_result_issues_2024, n = Inf, width = Inf)
# ------------------------------------------------------------------------------

#### 13. Create matching reporting tables -----------------------
report_columns <- c(
  final_column_order,
  "sample_year",
  "bull_wells_amplified",
  "rmtf_wells_amplified",
  "scul_wells_amplified",
  "brk_wells_amplified"
)

# Standardize bull trout species codes; preserve assay labels
standardize_bull_code <- function(data) {
  data %>%
    mutate(
      across(
        any_of(c("e_dna_result", "analyze_for")),
        ~ str_replace_all(.x, "\\bBUL2\\b", "BULL")
      )
    )
}

edna_2023_complete <- standardize_bull_code(edna_2023_complete)
edna_2024_complete <- standardize_bull_code(edna_2024_complete)

edna_2023_report <- edna_2023_complete %>%
  select(all_of(report_columns))

edna_2024_report <- edna_2024_complete %>%
  select(all_of(report_columns))

# Confirm identical column names and order
stopifnot(
  identical(
    names(edna_2023_report),
    names(edna_2024_report)
  )
)

dim(edna_2023_report)
dim(edna_2024_report)

## Join 2023 and 2024 data
edna_2023_2024_report <- bind_rows(
  edna_2023_report %>%
    mutate(
      sample_year = 2023L,
      date_collected = as.Date(date_collected)
    ),
  
  edna_2024_report %>%
    mutate(
      sample_year = 2024L,
      date_collected = as.Date(date_collected)
    )
) %>%
  arrange(sample_year, sample_id)

# Explicitly mark confirmed DNE results as missing
result_columns <- c(
  "e_dna_result",
  "bull_wells_amplified",
  "rmtf_wells_amplified",
  "scul_wells_amplified",
  "brk_wells_amplified"
)

set_dne_results_na <- function(data) {
  data %>%
    mutate(
      across(
        any_of(result_columns),
        ~ if_else(
          coalesce(str_to_upper(str_trim(analyze_for)) == "DNE", FALSE),
          NA_character_,
          as.character(.x)
        )
      )
    )
}

edna_2023_complete <- set_dne_results_na(edna_2023_complete)
edna_2024_complete <- set_dne_results_na(edna_2024_complete)

edna_2023_report <- set_dne_results_na(edna_2023_report)
edna_2024_report <- set_dne_results_na(edna_2024_report)

edna_2023_2024_report <- set_dne_results_na(
  edna_2023_2024_report
)

# Verify: every result column should return TRUE
edna_2023_2024_report %>%
  filter(analyze_for == "DNE") %>%
  group_by(sample_year) %>%
  summarise(
    n_dne = n(),
    across(all_of(result_columns), ~ all(is.na(.x))),
    .groups = "drop"
  ) %>%
  print(width = Inf)

# Verify: 103 samples from 2023 and 147 from 2024
edna_2023_2024_report %>%
  count(sample_year, name = "n_samples")

# Expected: 250 rows × 22 columns
dim(edna_2023_2024_report)

#View(edna_2023_2024_report)
## -----------------------------------------------------------------------------



################################################################################
################################################################################
######   --------   CONSOLIDATE / CREATE FINAL WORKBOOK    ----------    #######
################################################################################
################################################################################


#### Export the final eDNA workbook -------------------------
# Install export dependencies if needed
for (pkg in c("openxlsx", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

output_file <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/Field_Data/eDNA_results_NGC/FINAL_RESULTS_ALL/",
  "eDNA_results_all_postQC.xlsx"
)

#### Prepare the reporting tabs --------------------------------
report_all <- edna_2023_2024_report %>%
  arrange(sample_year, sample_id)

report_bulltrout <- report_all %>%
  filter(
    str_detect(
      str_to_upper(coalesce(analyze_for, "")),
      "\\b(BULL|BUL2)\\b"
    )
  )

stopifnot(
  nrow(report_all) == 250L,
  nrow(report_bulltrout) == 175L,
  !anyDuplicated(report_all[c("sample_year", "sample_id")])
)


#### Functions for preserving complete source information --------

# Convert list cells to JSON arrays for Excel.
# NULL cells remain blank; missing values within arrays are retained.
excel_safe <- function(data) {
  
  data[] <- lapply(data, function(column) {
    
    if (is.list(column)) {
      return(vapply(column, function(value) {
        
        if (is.null(value) || length(value) == 0L) {
          return(NA_character_)
        }
        
        as.character(jsonlite::toJSON(
          value,
          auto_unbox = FALSE,
          na = "null",
          null = "null",
          Date = "ISO8601",
          POSIXt = "ISO8601",
          digits = NA
        ))
      }, character(1)))
    }
    
    if (inherits(column, "POSIXt")) {
      return(format(column, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    }
    
    column
  })
  
  data
}

# Attach all source columns, preserving repeated source records.
# Prefixes distinguish source values from the final corrected values.
attach_source <- function(
    final_data, source_data, source_key, final_key, prefix) {
  
  source_data <- source_data %>%
    mutate(
      export_match_key = na_if(
        str_to_upper(str_trim(as.character(.data[[source_key]]))),
        ""
      )
    ) %>%
    filter(!is.na(export_match_key)) %>%
    group_by(export_match_key) %>%
    summarise(
      across(everything(), ~ list(.x)),
      .groups = "drop"
    ) %>%
    rename_with(
      ~ paste0(prefix, .x),
      -all_of("export_match_key")
    ) %>%
    excel_safe()
  
  final_data %>%
    mutate(
      export_match_key = na_if(
        str_to_upper(str_trim(as.character(.data[[final_key]]))),
        ""
      )
    ) %>%
    left_join(
      source_data,
      by = "export_match_key",
      relationship = "many-to-one",
      na_matches = "never"
    ) %>%
    select(-export_match_key)
}


#### Assemble all available information for 2023 ----------------
full_2023 <- edna_2023_complete %>%
  mutate(
    sample_year = 2023L,
    date_collected = as.Date(date_collected)
  ) %>%
  excel_safe()

full_2023 <- attach_source(
  full_2023, field_2023,
  "sample_id", "sample_id", "source_2023_field_"
)

full_2023 <- attach_source(
  full_2023, collection_extraction,
  "id_tag", "id_tag", "source_2023_extraction_"
)

full_2023 <- attach_source(
  full_2023, qpcr,
  "id_tag", "id_tag", "source_2023_qpcr_"
)

# Include the later results for the three 2023 samples
full_2023 <- attach_source(
  full_2023, later_results_2023,
  "id_tag", "id_tag", "source_later_lab_"
)


#### Assemble all available information for 2024 ----------------

full_2024 <- edna_2024_complete %>%
  mutate(
    sample_year = 2024L,
    date_collected = as.Date(date_collected)
  ) %>%
  excel_safe()

full_2024 <- attach_source(
  full_2024, field_2024,
  "sample_id", "sample_id", "source_2024_field_"
)

full_2024 <- attach_source(
  full_2024, lab_2024,
  "id_tag", "id_tag", "source_2024_lab_"
)

# Preserve the date comparisons and original date representations
full_2024 <- attach_source(
  full_2024, date_qc_2024,
  "sample_id", "sample_id", "source_date_qc_"
)


#### Include consolidated request records for both years --------
full_2023 <- attach_source(
  full_2023, request_records,
  "sample_id_match", "sample_id", "source_request_"
)

full_2024 <- attach_source(
  full_2024, request_records,
  "sample_id_match", "sample_id", "source_request_"
)

#### Align types and combine the complete tables ----------------
# Preserve values as text only where the two years use
# incompatible types for a shared column.
shared_columns <- intersect(names(full_2023), names(full_2024))

for (col in shared_columns) {
  
  compatible <- tryCatch(
    {
      vctrs::vec_ptype_common(
        full_2023[[col]],
        full_2024[[col]]
      )
      TRUE
    },
    error = function(e) FALSE
  )
  
  if (!compatible) {
    full_2023[[col]] <- as.character(full_2023[[col]])
    full_2024[[col]] <- as.character(full_2024[[col]])
  }
}

# Columns unavailable in one year are automatically filled with NA ---------
all_qpcr <- bind_rows(full_2023, full_2024) %>%
  arrange(sample_year, sample_id) %>%
  select(all_of(report_columns), everything())

stopifnot(
  nrow(all_qpcr) == 250L,
  !anyDuplicated(all_qpcr[c("sample_year", "sample_id")]),
  all(names(full_2023) %in% names(all_qpcr)),
  all(names(full_2024) %in% names(all_qpcr))
)

#### Check Excel limits before writing -------------------------
export_tables <- list(
  eDNA_data_bulltrout = report_bulltrout,
  eDNA_data_all = report_all,
  eDNA_data_all_qpcr = all_qpcr
)

# Stop rather than truncate cells exceeding Excel's character limit
for (sheet_name in names(export_tables)) {
  
  data <- export_tables[[sheet_name]]
  
  stopifnot(
    ncol(data) <= 16384L,
    nrow(data) + 1L <= 1048576L
  )
  
  for (col in names(data)) {
    if (is.character(data[[col]]) &&
        any(nchar(data[[col]]) > 32767L, na.rm = TRUE)) {
      stop(
        "Excel cell-length limit exceeded: ",
        sheet_name, " / ", col,
        ". Export stopped to prevent data loss."
      )
    }
  }
}


#### Write the three-tab workbook ------------------------------
wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF",
  fgFill = "gray",
  textDecoration = "bold",
  wrapText = TRUE
)

for (sheet_name in names(export_tables)) {
  
  data <- export_tables[[sheet_name]]
  
  openxlsx::addWorksheet(wb, sheet_name)
  
  openxlsx::writeDataTable(
    wb,
    sheet = sheet_name,
    x = data,
    tableStyle = "TableStyleMedium2",
    headerStyle = header_style,
    keepNA = FALSE
  )
  
  openxlsx::freezePane(
    wb,
    sheet = sheet_name,
    firstActiveRow = 2,
    firstActiveCol = 2
  )
  
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = seq_len(ncol(data)),
    widths = 18
  )
  
  openxlsx::setColWidths(
    wb,
    sheet = sheet_name,
    cols = which(names(data) %in% c(
      "field_notes", "lab_comments", "analysis_request_source"
    )),
    widths = 55
  )
  
  openxlsx::setRowHeights(
    wb,
    sheet = sheet_name,
    rows = 1,
    heights = 32
  )
}

# Avoid overwriting an existing export
if (file.exists(output_file)) {
  output_file <- sub(
    "\\.xlsx$",
    paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    output_file
  )
}

openxlsx::saveWorkbook(
  wb,
  file = output_file,
  overwrite = FALSE
)

# Verify the saved sheet names and row counts
stopifnot(
  identical(
    openxlsx::getSheetNames(output_file),
    names(export_tables)
  )
)

for (sheet_name in names(export_tables)) {
  saved_data <- openxlsx::read.xlsx(
    output_file,
    sheet = sheet_name,
    check.names = FALSE,
    skipEmptyRows = FALSE
  )
  
  stopifnot(
    nrow(saved_data) == nrow(export_tables[[sheet_name]]),
    ncol(saved_data) == ncol(export_tables[[sheet_name]])
  )
}

# Print the export location and dimensions
print(output_file)

tibble(
  sheet = names(export_tables),
  rows = vapply(export_tables, nrow, integer(1)),
  columns = vapply(export_tables, ncol, integer(1))
) %>%
  print(n = Inf)
################################################################################






