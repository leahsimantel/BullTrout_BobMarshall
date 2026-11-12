################################
##### Ch02 Data Exploration
##### June 27, 2026
################################

### Libraries
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

folder_path <- "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/Ch02_data_drop/1km_reach_allPixels_attributes"

# QA: Initial data import and structural checks -------------------------------
csv_files <- list.files(
  folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

cat("Number of CSV files:", length(csv_files), "\n")

data_list <- csv_files |>
  set_names(basename(csv_files)) |>
  map(~ read_csv(.x, show_col_types = FALSE))

file_summary <- imap_dfr(
  data_list,
  ~ tibble(
    source_file = .y,
    rows = nrow(.x),
    columns = ncol(.x)
  )
)

print(file_summary, n = Inf)

cat("Total rows:", sum(file_summary$rows), "\n")

empty_or_unusual_files <- file_summary |>
  filter(rows == 0 | columns != max(columns))

print(empty_or_unusual_files, n = Inf)

column_schema <- imap_dfr(
  data_list,
  ~ tibble(
    source_file = .y,
    column_name = names(.x)
  )
)

column_presence <- column_schema |>
  count(column_name, name = "files_present") |>
  mutate(
    expected_files = length(data_list),
    missing_from_files = expected_files - files_present
  ) |>
  arrange(desc(missing_from_files), column_name)

missing_columns_by_file <- crossing(
  source_file = names(data_list),
  column_name = unique(column_schema$column_name)
) |>
  anti_join(
    column_schema,
    by = c("source_file", "column_name")
  )

print(
  column_presence |> filter(missing_from_files > 0),
  n = Inf
)

print(missing_columns_by_file, n = Inf)

type_summary <- imap_dfr(
  data_list,
  ~ tibble(
    source_file = .y,
    column_name = names(.x),
    column_type = map_chr(.x, ~ class(.x)[1])
  )
)

type_conflicts <- type_summary |>
  distinct(column_name, column_type) |>
  count(column_name, name = "number_of_types") |>
  filter(number_of_types > 1) |>
  left_join(type_summary, by = "column_name") |>
  arrange(column_name, source_file)

print(type_conflicts, n = Inf)

all_pixel_attributes_raw <- bind_rows(data_list) |>
  select(-any_of("system:index"))

stopifnot(
  nrow(all_pixel_attributes_raw) == sum(file_summary$rows)
)

cat(
  "Combined data dimensions:",
  nrow(all_pixel_attributes_raw), "rows x",
  ncol(all_pixel_attributes_raw), "columns\n"
)

#View(all_pixel_attributes_raw)
# End initial QA --------------------------------------------------------------

# QA: Missing values, duplicates, and identifier consistency ------------------
qa_data <- all_pixel_attributes_raw

missing_summary <- qa_data |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "column_name",
    values_to = "missing_values"
  ) |>
  mutate(
    percent_missing = round(
      100 * missing_values / nrow(qa_data),
      2
    )
  ) |>
  arrange(desc(percent_missing))

print(missing_summary, n = Inf)

placeholder_summary <- qa_data |>
  select(where(is.numeric)) |>
  summarise(
    across(
      everything(),
      ~ sum(.x %in% c(-9999, -999, 9999), na.rm = TRUE)
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "column_name",
    values_to = "placeholder_values"
  ) |>
  filter(placeholder_values > 0) |>
  arrange(desc(placeholder_values))

print(placeholder_summary, n = Inf)

if ("pixel_ID" %in% names(qa_data)) {
  
  duplicate_pixel_IDs <- qa_data |>
    count(pixel_ID, name = "number_of_rows") |>
    filter(!is.na(pixel_ID), number_of_rows > 1) |>
    arrange(desc(number_of_rows))
  
  cat(
    "Number of duplicated pixel IDs:",
    nrow(duplicate_pixel_IDs),
    "\n"
  )
  
  print(duplicate_pixel_IDs, n = Inf)
}

if (all(c("pixel_ID", "COMID") %in% names(qa_data))) {
  
  pixel_COMID_conflicts <- qa_data |>
    filter(!is.na(pixel_ID), !is.na(COMID)) |>
    distinct(pixel_ID, COMID) |>
    count(pixel_ID, name = "number_of_COMIDs") |>
    filter(number_of_COMIDs > 1) |>
    arrange(desc(number_of_COMIDs))
  
  cat(
    "Pixels associated with multiple COMIDs:",
    nrow(pixel_COMID_conflicts),
    "\n"
  )
  
  print(pixel_COMID_conflicts, n = Inf)
}

if ("COMID" %in% names(qa_data)) {
  
  pixels_per_COMID <- qa_data |>
    group_by(COMID) |>
    summarise(
      rows = n(),
      unique_pixels = if ("pixel_ID" %in% names(qa_data)) {
        n_distinct(pixel_ID, na.rm = TRUE)
      } else {
        NA_integer_
      },
      .groups = "drop"
    ) |>
    arrange(unique_pixels)
  
  print(pixels_per_COMID, n = Inf)
  
  summary(pixels_per_COMID$unique_pixels)
}

numeric_range_summary <- qa_data |>
  select(where(is.numeric)) |>
  summarise(
    across(
      everything(),
      list(
        minimum = ~ min(.x, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE)
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("column_name", ".value"),
    names_pattern = "^(.*)_(minimum|maximum|mean)$"
  ) |>
  arrange(column_name)

print(numeric_range_summary, n = Inf)

# End missing-value and identifier QA -----------------------------------------

# QA: Identify differences among duplicated pixel records ---------------------
duplicate_rows <- qa_data |>
  group_by(pixel_ID) |>
  filter(n() > 1) |>
  ungroup()

duplicate_difference_flags <- duplicate_rows |>
  group_by(pixel_ID) |>
  summarise(
    across(
      everything(),
      ~ n_distinct(.x, na.rm = FALSE) > 1
    ),
    .groups = "drop"
  )

duplicate_difference_summary <- duplicate_difference_flags |>
  pivot_longer(
    -pixel_ID,
    names_to = "column_name",
    values_to = "differs"
  ) |>
  filter(differs) |>
  count(column_name, name = "duplicated_pixels_affected") |>
  arrange(desc(duplicated_pixels_affected))

print(duplicate_difference_summary, n = Inf)

differences_per_pixel <- duplicate_difference_flags |>
  pivot_longer(
    -pixel_ID,
    names_to = "column_name",
    values_to = "differs"
  ) |>
  filter(differs) |>
  group_by(pixel_ID) |>
  summarise(
    number_of_differing_columns = n(),
    differing_columns = paste(column_name, collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(desc(number_of_differing_columns))

print(differences_per_pixel, n = Inf)

duplicate_key_values <- duplicate_rows |>
  select(
    any_of(c(
      "pixel_ID",
      "sample_ID",
      "COMID",
      "huc12",
      "fire_event",
      "ignition_year",
      "sev_class",
      "sev_num",
      "num_times_burned"
    ))
  ) |>
  arrange(pixel_ID)

print(duplicate_key_values, n = 40, width = Inf)

#placeholder_patterns |>
#  print(n = Inf, width = Inf)

#invalid_stream_summary <- invalid_stream_rows |>
#  summarise(
#    rows = n(),
#    unique_pixels = n_distinct(pixel_ID),
#    unique_sample_IDs = n_distinct(sample_ID),
#    unique_huc12s = n_distinct(huc12)
#  )

#print(invalid_stream_summary)
# End duplicate-record comparison ---------------------------------------------

# QA: Examine overlapping samples and segment-length conflicts ----------------
duplicate_sample_pairs <- duplicate_rows |>
  select(pixel_ID, sample_ID) |>
  distinct() |>
  group_by(pixel_ID) |>
  summarise(
    sample_ID_1 = first(sort(sample_ID)),
    sample_ID_2 = last(sort(sample_ID)),
    .groups = "drop"
  ) |>
  count(
    sample_ID_1,
    sample_ID_2,
    name = "shared_pixels"
  ) |>
  arrange(desc(shared_pixels))

print(duplicate_sample_pairs, n = Inf)

segment_length_conflicts <- duplicate_rows |>
  semi_join(
    differences_per_pixel |>
      filter(grepl("nhdsegment_length", differing_columns)),
    by = "pixel_ID"
  ) |>
  select(
    pixel_ID,
    sample_ID,
    COMID,
    huc12,
    nhdsegment_length
  ) |>
  arrange(pixel_ID, sample_ID)

print(segment_length_conflicts, n = Inf)

segment_length_conflicts |>
  group_by(pixel_ID, COMID) |>
  summarise(
    sample_IDs = paste(sample_ID, collapse = " | "),
    segment_lengths = paste(
      nhdsegment_length,
      collapse = " | "
    ),
    difference = max(nhdsegment_length) -
      min(nhdsegment_length),
    .groups = "drop"
  ) |>
  print(n = Inf, width = Inf)

cat(
  "Unique sample-pixel combinations:",
  n_distinct(
    paste(qa_data$sample_ID, qa_data$pixel_ID)
  ),
  "\n"
)

cat(
  "Total rows:",
  nrow(qa_data),
  "\n"
)

# End overlapping-sample QA ---------------------------------------------------

# QA: Examine records without stream attributes -------------------------------
invalid_stream_by_sample <- qa_data |>
  group_by(sample_ID, huc12) |>
  summarise(
    total_rows = n(),
    rows_without_COMID = sum(COMID == -9999),
    percent_without_COMID = round(
      100 * rows_without_COMID / total_rows,
      1
    ),
    .groups = "drop"
  ) |>
  filter(rows_without_COMID > 0) |>
  arrange(desc(percent_without_COMID))

print(invalid_stream_by_sample, n = Inf)

invalid_stream_by_sample |>
  filter(percent_without_COMID == 100) |>
  print(n = Inf)

invalid_stream_rows |>
  select(
    sample_ID,
    pixel_ID,
    huc12,
    pixel_lat,
    pixel_long,
    COMID,
    FCODE,
    segment_slope,
    upstream_da_sqkm
  ) |>
  arrange(sample_ID, pixel_ID) |>
  View()
# End unmatched-stream QA -----------------------------------------------------

# QA map: Missing stream attributes using coordinates -------------------------
library(dplyr)
library(ggplot2)
library(ggrepel)

map_data <- qa_data |>
  mutate(
    missing_stream_attributes = COMID == -9999
  ) |>
  filter(!is.na(pixel_long), !is.na(pixel_lat))

missing_sample_labels <- map_data |>
  filter(missing_stream_attributes) |>
  group_by(sample_ID) |>
  summarise(
    pixel_long = mean(pixel_long),
    pixel_lat = mean(pixel_lat),
    missing_pixels = n(),
    .groups = "drop"
  )

p <- ggplot(
  map_data,
  aes(x = pixel_long, y = pixel_lat)
) +
  geom_point(
    color = "grey75",
    size = 0.8,
    alpha = 0.5
  ) +
  geom_point(
    data = filter(map_data, missing_stream_attributes),
    color = "red",
    size = 1.8
  ) +
  geom_label_repel(
    data = missing_sample_labels,
    aes(
      label = paste0(
        sample_ID,
        "\n",
        missing_pixels,
        " missing"
      )
    ),
    size = 3,
    min.segment.length = 0,
    seed = 25
  ) +
  coord_equal() +
  labs(
    title = "Samples with Missing Stream Attributes",
    subtitle = "Red points have COMID = -9999",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

print(p)

# End missing-stream map ------------------------------------------------------

# QA: Variable ranges and categorical values ----------------------------------

qa_ranges <- qa_data |>
  mutate(
    across(where(is.numeric), ~ na_if(.x, -9999))
  )

range_checks <- bind_rows(
  qa_ranges |>
    summarise(
      variable_group = "NDVI",
      invalid_values = sum(
        if_any(starts_with("NDVI_"), ~ .x < -1 | .x > 1),
        na.rm = TRUE
      )
    ),
  
  qa_ranges |>
    summarise(
      variable_group = "SWE",
      invalid_values = sum(
        if_any(starts_with("SWE_"), ~ .x < 0),
        na.rm = TRUE
      )
    ),
  
  qa_ranges |>
    summarise(
      variable_group = "CWD",
      invalid_values = sum(
        if_any(starts_with("CWD_"), ~ .x < 0),
        na.rm = TRUE
      )
    ),
  
  qa_ranges |>
    summarise(
      variable_group = "PRISM precipitation",
      invalid_values = sum(
        if_any(
          matches("^ppt"),
          ~ .x < 0
        ),
        na.rm = TRUE
      )
    ),
  
  qa_ranges |>
    summarise(
      variable_group = "PRISM temperature",
      invalid_values = sum(
        if_any(
          starts_with("tmean"),
          ~ .x < -50 | .x > 50
        ),
        na.rm = TRUE
      )
    ),
  
  qa_ranges |>
    summarise(
      variable_group = "Latitude",
      invalid_values = sum(
        pixel_lat < -90 | pixel_lat > 90,
        na.rm = TRUE
      )
    ),
  
  qa_ranges |>
    summarise(
      variable_group = "Longitude",
      invalid_values = sum(
        pixel_long < -180 | pixel_long > 180,
        na.rm = TRUE
      )
    ),
  
  qa_ranges |>
    summarise(
      variable_group = "Segment slope",
      invalid_values = sum(
        segment_slope < 0,
        na.rm = TRUE
      )
    ),
  
  qa_ranges |>
    summarise(
      variable_group = "Upstream drainage area",
      invalid_values = sum(
        upstream_da_sqkm < 0,
        na.rm = TRUE
      )
    ),
  
  qa_ranges |>
    summarise(
      variable_group = "Severity value",
      invalid_values = sum(
        sev_num < 0 | sev_num > 4,
        na.rm = TRUE
      )
    )
)

print(range_checks, n = Inf)

categorical_summary <- bind_rows(
  qa_ranges |>
    count(sev_class, name = "rows") |>
    mutate(variable = "sev_class",
           value = as.character(sev_class)),
  
  qa_ranges |>
    count(fire_event, name = "rows") |>
    mutate(variable = "fire_event",
           value = as.character(fire_event)),
  
  qa_ranges |>
    count(num_times_burned, name = "rows") |>
    mutate(variable = "num_times_burned",
           value = as.character(num_times_burned)),
  
  qa_ranges |>
    count(FCODE, name = "rows") |>
    mutate(variable = "FCODE",
           value = as.character(FCODE))
) |>
  select(variable, value, rows) |>
  arrange(variable, desc(rows))

print(categorical_summary, n = Inf)

severity_conflicts <- qa_ranges |>
  filter(
    (!is.na(sev_num) & sev_class %in% c("None", NA_character_)) |
      (is.na(sev_num) & !sev_class %in% c("None", NA_character_))
  )

cat(
  "Rows with inconsistent severity values:",
  nrow(severity_conflicts),
  "\n"
)

# End range and categorical QA ------------------------------------------------

# QA: Annual column coverage and time-series completeness ---------------------

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

expected_years <- 1984:2024

expected_columns <- list(
  NDVI = paste0("NDVI_", expected_years),
  CWD = paste0("CWD_TC_", expected_years),
  SWE = paste0("SWE_", expected_years, "04_TC"),
  precipitation = paste0("ppttot_JJA_", expected_years, "_PR"),
  temperature = paste0("tmeanavg_JJA_", expected_years, "_PR")
)

annual_column_check <- imap_dfr(
  expected_columns,
  ~ tibble(
    variable_group = .y,
    expected_column = .x,
    present = .x %in% names(qa_ranges)
  )
)

missing_annual_columns <- annual_column_check |>
  filter(!present)

print(missing_annual_columns, n = Inf)

annual_coverage_summary <- annual_column_check |>
  group_by(variable_group) |>
  summarise(
    expected_columns = n(),
    columns_present = sum(present),
    columns_missing = sum(!present),
    .groups = "drop"
  )

print(annual_coverage_summary, n = Inf)

time_series_missingness <- imap_dfr(
  expected_columns,
  function(columns, group_name) {
    
    available_columns <- intersect(
      columns,
      names(qa_ranges)
    )
    
    tibble(
      variable_group = group_name,
      total_values = nrow(qa_ranges) *
        length(available_columns),
      missing_values = sum(
        is.na(qa_ranges[available_columns])
      ),
      percent_missing = round(
        100 * missing_values / total_values,
        2
      )
    )
  }
)

print(time_series_missingness, n = Inf)

ndvi_columns <- intersect(
  expected_columns$NDVI,
  names(qa_ranges)
)

ndvi_missing_by_pixel <- qa_ranges |>
  transmute(
    sample_ID,
    pixel_ID,
    missing_NDVI_years = rowSums(
      is.na(pick(all_of(ndvi_columns)))
    )
  )

summary(ndvi_missing_by_pixel$missing_NDVI_years)

ndvi_missing_by_sample <- ndvi_missing_by_pixel |>
  group_by(sample_ID) |>
  summarise(
    total_pixels = n(),
    pixels_with_missing_NDVI =
      sum(missing_NDVI_years > 0),
    mean_missing_years = round(
      mean(missing_NDVI_years),
      2
    ),
    maximum_missing_years =
      max(missing_NDVI_years),
    .groups = "drop"
  ) |>
  arrange(
    desc(maximum_missing_years),
    desc(mean_missing_years)
  )

print(ndvi_missing_by_sample, n = Inf)

# End annual time-series QA ---------------------------------------------------

# Fix PRISM 30-year averages: 1986–2015 ---------------------------------------

baseline_years <- 1986:2015

ppt_baseline_cols <- paste0(
  "ppttot_JJA_",
  baseline_years,
  "_PR"
)

tmean_baseline_cols <- paste0(
  "tmeanavg_JJA_",
  baseline_years,
  "_PR"
)

qa_ranges <- qa_ranges |>
  mutate(
    ppt_30yrAvg_JJA_PR = rowMeans(
      pick(all_of(ppt_baseline_cols)),
      na.rm = FALSE
    ),
    
    tmean_30yrAvg_JJA_PR = rowMeans(
      pick(all_of(tmean_baseline_cols)),
      na.rm = FALSE
    )
  )

# Verify the replacement
qa_ranges |>
  summarise(
    ppt_min = min(ppt_30yrAvg_JJA_PR),
    ppt_max = max(ppt_30yrAvg_JJA_PR),
    tmean_min = min(tmean_30yrAvg_JJA_PR),
    tmean_max = max(tmean_30yrAvg_JJA_PR)
  ) |>
  print()

# End PRISM 30-year-average correction ----------------------------------------

# QA: Climate distributions and potential outliers ----------------------------
climate_long <- bind_rows(
  qa_ranges |>
    select(sample_ID, pixel_ID, all_of(expected_columns$SWE)) |>
    pivot_longer(
      -c(sample_ID, pixel_ID),
      names_to = "column_name",
      values_to = "value"
    ) |>
    mutate(
      variable = "SWE",
      year = as.integer(
        str_extract(column_name, "\\d{4}")
      )
    ),
  
  qa_ranges |>
    select(sample_ID, pixel_ID, all_of(expected_columns$CWD)) |>
    pivot_longer(
      -c(sample_ID, pixel_ID),
      names_to = "column_name",
      values_to = "value"
    ) |>
    mutate(
      variable = "CWD",
      year = as.integer(
        str_extract(column_name, "\\d{4}")
      )
    ),
  
  qa_ranges |>
    select(
      sample_ID,
      pixel_ID,
      all_of(expected_columns$precipitation)
    ) |>
    pivot_longer(
      -c(sample_ID, pixel_ID),
      names_to = "column_name",
      values_to = "value"
    ) |>
    mutate(
      variable = "Precipitation",
      year = as.integer(
        str_extract(column_name, "\\d{4}")
      )
    ),
  
  qa_ranges |>
    select(
      sample_ID,
      pixel_ID,
      all_of(expected_columns$temperature)
    ) |>
    pivot_longer(
      -c(sample_ID, pixel_ID),
      names_to = "column_name",
      values_to = "value"
    ) |>
    mutate(
      variable = "Temperature",
      year = as.integer(
        str_extract(column_name, "\\d{4}")
      )
    )
)

annual_climate_summary <- climate_long |>
  group_by(variable, year) |>
  summarise(
    minimum = min(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE),
    standard_deviation = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

print(annual_climate_summary, n = Inf)

climate_outliers <- climate_long |>
  group_by(variable, year) |>
  mutate(
    lower_quartile = quantile(
      value,
      0.25,
      na.rm = TRUE
    ),
    upper_quartile = quantile(
      value,
      0.75,
      na.rm = TRUE
    ),
    IQR_value = IQR(value, na.rm = TRUE),
    lower_limit = lower_quartile -
      3 * IQR_value,
    upper_limit = upper_quartile +
      3 * IQR_value
  ) |>
  ungroup() |>
  filter(
    value < lower_limit |
      value > upper_limit
  ) |>
  arrange(variable, year, value)

outlier_summary <- climate_outliers |>
  count(variable, year, name = "flagged_values") |>
  arrange(variable, desc(flagged_values))

print(outlier_summary, n = Inf)

cat(
  "Total potential climate outliers:",
  nrow(climate_outliers),
  "\n"
)

# End climate outlier QA ------------------------------------------------------

# Create final analysis-ready dataset -----------------------------------------
all_pixel_attributes <- qa_ranges |>
  mutate(
    COMID = as.character(COMID),
    FCODE = as.character(FCODE),
    pixel_ID = as.character(pixel_ID),
    sample_ID = as.character(sample_ID),
    
    across(
      contains("SWE"),
      ~ round(.x, 1)
    ),
    
    across(
      ends_with("_PR"),
      ~ round(.x, 1)
    )
  ) |>
  rename(
    nhdsegment_slope = segment_slope
  ) |>
  select(
    -any_of(c(
      "system:index",
      "stream_match_distance_m"
    ))
  )

stopifnot(
  nrow(all_pixel_attributes) == 6184,
  n_distinct(
    all_pixel_attributes$sample_ID,
    all_pixel_attributes$pixel_ID
  ) == 6184
)

cat(
  "Final dataset:",
  nrow(all_pixel_attributes), "rows x",
  ncol(all_pixel_attributes), "columns\n"
)

View(all_pixel_attributes)

# End final dataset creation --------------------------------------------------

################################################################################
################################################################################
################################################################################
################################################################################

# Read Bull Trout results summary ---------------------------------------------

library(readxl)

excel_path <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/",
  "Field_Data/BT_Results_BMWA_Clean_Rcopy.xlsx"
)

BT_Results_Summary <- read_excel(
  path = excel_path,
  sheet = "BT_Results_Summary"
)

glimpse(BT_Results_Summary)
#View(BT_Results_Summary)

# End Bull Trout results import -----------------------------------------------

# Create Bull Trout pixel-level analysis dataset ------------------------------
# Confirm each Site_ID occurs only once in the Excel data
stopifnot(
  !anyDuplicated(BT_Results_Summary$Site_ID)
)

BT_pixel_data <- all_pixel_attributes |>
  inner_join(
    BT_Results_Summary,
    by = c("sample_ID" = "Site_ID")
  )

cat(
  "Sites retained:",
  n_distinct(BT_pixel_data$sample_ID),
  "\n"
)

cat(
  "Individual pixel rows retained:",
  nrow(BT_pixel_data),
  "\n"
)

cat(
  "Columns in new dataset:",
  ncol(BT_pixel_data),
  "\n"
)

# Check for Excel sites that did not match the main dataset
unmatched_BT_sites <- BT_Results_Summary |>
  anti_join(
    all_pixel_attributes |>
      distinct(sample_ID),
    by = c("Site_ID" = "sample_ID")
  )

print(unmatched_BT_sites, n = Inf)
# End Bull Trout pixel dataset ------------------------------------------------

# Add pixel-level climate z-scores: 2022–2024 -------------------------------

baseline_years <- 1986:2015
target_years <- 2022:2024

row_population_sd <- function(data) {
  values <- as.matrix(data)
  row_means <- rowMeans(values, na.rm = FALSE)
  sqrt(rowMeans((values - row_means)^2, na.rm = FALSE))
}

cwd_sd_30yr <- row_population_sd(
  BT_pixel_data |>
    select(all_of(
      paste0("CWD_TC_", baseline_years)
    ))
)

ppt_sd_30yr <- row_population_sd(
  BT_pixel_data |>
    select(all_of(
      paste0(
        "ppttot_JJA_",
        baseline_years,
        "_PR"
      )
    ))
)

swe_sd_30yr <- row_population_sd(
  BT_pixel_data |>
    select(all_of(
      paste0(
        "SWE_",
        baseline_years,
        "04_TC"
      )
    ))
)

tmean_sd_30yr <- row_population_sd(
  BT_pixel_data |>
    select(all_of(
      paste0(
        "tmeanavg_JJA_",
        baseline_years,
        "_PR"
      )
    ))
)

# Prevent division by zero
cwd_sd_30yr[cwd_sd_30yr == 0] <- NA_real_
ppt_sd_30yr[ppt_sd_30yr == 0] <- NA_real_
swe_sd_30yr[swe_sd_30yr == 0] <- NA_real_
tmean_sd_30yr[tmean_sd_30yr == 0] <- NA_real_

BT_pixel_data <- BT_pixel_data |>
  mutate(
    cwdz_JJA_2022_TC =
      (CWD_TC_2022 - cwd_30yrAvg_JJA_TC) /
      cwd_sd_30yr,
    cwdz_JJA_2023_TC =
      (CWD_TC_2023 - cwd_30yrAvg_JJA_TC) /
      cwd_sd_30yr,
    cwdz_JJA_2024_TC =
      (CWD_TC_2024 - cwd_30yrAvg_JJA_TC) /
      cwd_sd_30yr,
    
    pptz_JJA_2022_PR =
      (ppttot_JJA_2022_PR - ppt_30yrAvg_JJA_PR) /
      ppt_sd_30yr,
    pptz_JJA_2023_PR =
      (ppttot_JJA_2023_PR - ppt_30yrAvg_JJA_PR) /
      ppt_sd_30yr,
    pptz_JJA_2024_PR =
      (ppttot_JJA_2024_PR - ppt_30yrAvg_JJA_PR) /
      ppt_sd_30yr,
    
    swez_Apr_2022_TC =
      (SWE_202204_TC - swe_30yrAvg_Apr_TC) /
      swe_sd_30yr,
    swez_Apr_2023_TC =
      (SWE_202304_TC - swe_30yrAvg_Apr_TC) /
      swe_sd_30yr,
    swez_Apr_2024_TC =
      (SWE_202404_TC - swe_30yrAvg_Apr_TC) /
      swe_sd_30yr,
    
    tmeanz_JJA_2022_PR =
      (tmeanavg_JJA_2022_PR -
         tmean_30yrAvg_JJA_PR) /
      tmean_sd_30yr,
    tmeanz_JJA_2023_PR =
      (tmeanavg_JJA_2023_PR -
         tmean_30yrAvg_JJA_PR) /
      tmean_sd_30yr,
    tmeanz_JJA_2024_PR =
      (tmeanavg_JJA_2024_PR -
         tmean_30yrAvg_JJA_PR) /
      tmean_sd_30yr
  )

zscore_columns <- names(BT_pixel_data) |>
  grep(
    pattern = "^(cwdz|pptz|swez|tmeanz)_",
    value = TRUE
  )

BT_pixel_data |>
  select(
    sample_ID,
    pixel_ID,
    all_of(zscore_columns)
  ) |>
  glimpse()

# End pixel-level climate z-scores -------------------------------------------

# Create reach-level Bull Trout climate dataset -------------------------------

BT_site_columns <- setdiff(
  names(BT_Results_Summary),
  "Site_ID"
)

BT_reach_data <- BT_pixel_data |>
  group_by(sample_ID) |>
  summarise(
    n_pixels = n(),
    
    reach_latitude = mean(
      pixel_lat,
      na.rm = TRUE
    ),
    
    reach_longitude = mean(
      pixel_long,
      na.rm = TRUE
    ),
    
    across(
      all_of(zscore_columns),
      ~ mean(.x, na.rm = TRUE)
    ),
    
    across(
      all_of(BT_site_columns),
      ~ first(.x)
    ),
    
    .groups = "drop"
  )

stopifnot(
  nrow(BT_reach_data) == 153,
  !anyDuplicated(BT_reach_data$sample_ID)
)

cat(
  "Reach-level dataset:",
  nrow(BT_reach_data), "sites x",
  ncol(BT_reach_data), "columns\n"
)
# End reach-level dataset ------------------------------------------------------

# Read and standardize sample dates and observed flow --------------------------
library(readxl)
library(dplyr)
library(lubridate)

field_data_path <- "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/Field_Data"

parse_sample_date <- function(x) {
  
  x <- trimws(as.character(x))
  excel_serial <- suppressWarnings(as.numeric(x))
  
  result <- as.Date(
    rep(NA_character_, length(x))
  )
  
  is_excel_serial <- !is.na(excel_serial) &
    excel_serial > 20000
  
  result[is_excel_serial] <- as.Date(
    excel_serial[is_excel_serial],
    origin = "1899-12-30"
  )
  
  result[!is_excel_serial] <- as.Date(
    parse_date_time(
      x[!is_excel_serial],
      orders = c("mdy", "ymd", "dmy"),
      quiet = TRUE
    )
  )
  
  result
}

sample_field_info <- suppressMessages(
  read_excel(
    path = file.path(
      field_data_path,
      "2023-24_Field_Data_Results_Master_12.29.2024.xlsx"
    ),
    sheet = "SFF_Sampled_23-24_2023btModel_C"
  )
) |>
  transmute(
    sample_ID = as.character(Site_ID),
    Site_ID = as.character(Site_ID),
    sample_date_original = as.character(`Date Collected`),
    sample_date = parse_sample_date(`Date Collected`),
    obs_flow = suppressWarnings(as.numeric(`obs_flow(0=tooLow)`))
  )

# Display values that could not be interpreted as dates
sample_field_info |>
  filter(
    is.na(sample_date),
    !is.na(sample_date_original),
    sample_date_original != ""
  ) |>
  distinct(sample_date_original) |>
  print(n = Inf)

class(sample_field_info$sample_date)
summary(sample_field_info$sample_date)
# End field-information import ------------------------------------------------

# QA: Find invalid sample years -----------------------------------------------

invalid_sample_dates <- sample_field_info |>
  filter(
    !is.na(sample_date),
    !year(sample_date) %in% c(2023, 2024)
  ) |>
  select(
    Site_ID,
    sample_date_original,
    sample_date
  )

print(invalid_sample_dates, n = Inf)

sample_field_info <- sample_field_info |>
  mutate(
    sample_date = case_when(
      year(sample_date) == 3023 ~
        sample_date %m-% years(1000),
      TRUE ~ sample_date
    )
  )

# Confirm all populated dates are in 2023 or 2024
sample_field_info |>
  count(sample_year = year(sample_date), .drop = FALSE) |>
  print(n = Inf)

stopifnot(
  all(
    is.na(sample_field_info$sample_date) |
      year(sample_field_info$sample_date) %in% c(2023, 2024)
  )
)
# End sample-date QA -----------------------------------------------------------


################################################################################
################################################################################
################################################################################
################################################################################

### SAMPLE CULLING
##  Mapping, box plots, and histograms for covariates -----------------------------------
library(dplyr)
library(ggplot2)
library(sf)

# Stored classifications and plot colors
status_colors <- c(
  "Present, Correct"   = "#2CA25F",
  "Absent, Correct"    = "#9E9E9E",
  "Present, Incorrect" = "#FFD92F",
  "Absent, Incorrect"  = "#DE2D26"
)

# Labels displayed in figures
status_labels <- c(
  "Present, Correct"   = "Present, Suitable Habitat",
  "Absent, Correct"    = "Absent, Unsuitable Habitat",
  "Present, Incorrect" = "Present, Unsuitable Habitat",
  "Absent, Incorrect"  = "Absent, Suitable Habitat"
)

# Reusable ggplot scales
status_fill_scale <- scale_fill_manual(
  values = status_colors,
  labels = status_labels,
  drop = FALSE
)

status_color_scale <- scale_color_manual(
  values = status_colors,
  labels = status_labels,
  drop = FALSE
)

# Map inputs -----------------------------------------------------------------
huc12_gdb <- paste0(
  "C:/Users/leahs/OneDrive/Documents/ArcGIS/Projects/",
  "BT_model_comparisons/BT_model_comparisons.gdb"
)

huc12_layer <- "HUC12_BMWA"

streams_path <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/",
  "ArcGIS/BMWC_Fire_Data/NHDPlus_V2_Streamlines/",
  "NHDFlowline_Network_clipped_NoLakes.shp"
)

bt_baseline_path <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/",
  "ArcGIS/BMWC_Fire_Data/bull_trout_model/Revised_2023/",
  "BullTroutPatchScenarios_BaselineClimate_",
  "Isaak_et_al_2022_AppendixS6.shp"
)

stopifnot(
  file.exists(huc12_gdb),
  file.exists(streams_path),
  file.exists(bt_baseline_path)
)

huc12_map_sf <- st_read(
  dsn = huc12_gdb,
  layer = huc12_layer,
  quiet = TRUE
) |>
  st_zm(
    drop = TRUE,
    what = "ZM"
  ) |>
  st_make_valid()

streams_map_sf <- st_read(
  streams_path,
  quiet = TRUE
) |>
  st_zm(
    drop = TRUE,
    what = "ZM"
  )

bt_baseline_map_sf <- st_read(
  bt_baseline_path,
  quiet = TRUE
) |>
  st_zm(
    drop = TRUE,
    what = "ZM"
  ) |>
  st_make_valid()

stopifnot(!is.na(st_crs(huc12_map_sf)))

map_crs <- st_crs(huc12_map_sf)

streams_map_sf <- streams_map_sf |>
  st_transform(map_crs)

bt_baseline_map_sf <- bt_baseline_map_sf |>
  st_transform(map_crs)


# Sample culling using list of culled samples previously made in GEE  ---------
library(readxl)
library(dplyr)
library(lubridate)
library(sf)

stopifnot(
  exists("BT_reach_data"),
  exists("sample_field_info"),
  exists("huc12_map_sf"),
  all(c("sample_ID", "reach_longitude", "reach_latitude", "stat_cat") %in%
        names(BT_reach_data)),
  "name" %in% names(huc12_map_sf)
)

## Retain samples from selected HUC12s:
retained_huc12_names <- c(
  "Gorge Creek",
  "Upper White River",
  "Middle White River"
)

retained_huc12s <- huc12_map_sf |>
  filter(name %in% retained_huc12_names) |>
  st_make_valid()

missing_retained_huc12s <- setdiff(
  retained_huc12_names,
  retained_huc12s$name
)

stopifnot(
  nrow(retained_huc12s) > 0,
  length(missing_retained_huc12s) == 0
)

retained_huc12_samples <- BT_reach_data |>
  filter(
    is.finite(reach_longitude),
    is.finite(reach_latitude)
  ) |>
  st_as_sf(
    coords = c("reach_longitude", "reach_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  st_transform(st_crs(retained_huc12s)) |>
  filter(lengths(st_intersects(geometry, retained_huc12s)) > 0) |>
  st_drop_geometry() |>
  transmute(
    sample_ID = as.character(sample_ID)
  ) |>
  distinct(sample_ID)

# Excel list: Cull samples based on 2022 model
field_data_path <- "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/Field_Data"

bt_results_path <- file.path(
  field_data_path,
  "BT_Results_BMWA_Clean.xlsx"
)

bt_culled_sample_list_original <- read_excel(
  path = bt_results_path,
  sheet = "ArcGIS_Attribute_Table_R"
) |>
  transmute(
    sample_ID = as.character(Site_ID),
    stat_cat = as.character(stat_cat)
  ) |>
  filter(
    !is.na(sample_ID),
    sample_ID != "",
    !is.na(stat_cat),
    stat_cat != ""
  ) |>
  distinct(sample_ID, .keep_all = TRUE)

bt_culled_sample_list <- bt_culled_sample_list_original |>
  bind_rows(
    BT_reach_data |>
      semi_join(retained_huc12_samples, by = "sample_ID") |>
      transmute(
        sample_ID = as.character(sample_ID),
        stat_cat = as.character(stat_cat)
      )
  ) |>
  filter(
    !is.na(sample_ID),
    sample_ID != "",
    !is.na(stat_cat),
    stat_cat != ""
  ) |>
  distinct(sample_ID, .keep_all = TRUE)

cat(
  "Original Excel culled samples:",
  n_distinct(bt_culled_sample_list_original$sample_ID),
  "\nAdditional HUC12 samples retained:",
  n_distinct(retained_huc12_samples$sample_ID),
  "\nFinal culled sample list:",
  n_distinct(bt_culled_sample_list$sample_ID),
  "\n"
)

# Create rich culled reach-level table
BT_reach_field_culled <- BT_reach_data |>
  semi_join(
    bt_culled_sample_list,
    by = "sample_ID"
  ) |>
  select(
    -any_of(c(
      "stat_cat",
      "sample_date",
      "sample_year",
      "obs_flow"
    ))
  ) |>
  left_join(
    bt_culled_sample_list,
    by = "sample_ID"
  ) |>
  left_join(
    sample_field_info |>
      transmute(
        sample_ID = as.character(sample_ID),
        sample_date,
        sample_year = year(sample_date),
        obs_flow
      ) |>
      distinct(sample_ID, .keep_all = TRUE),
    by = "sample_ID"
  )

# Spatial point dataset used by maps
BT_reach_field_culled_sf <- BT_reach_field_culled |>
  filter(
    is.finite(reach_longitude),
    is.finite(reach_latitude)
  ) |>
  st_as_sf(
    coords = c("reach_longitude", "reach_latitude"),
    crs = 4326,
    remove = FALSE
  )

# QA
stopifnot(
  nrow(BT_reach_field_culled) > 0,
  !anyDuplicated(BT_reach_field_culled$sample_ID),
  nrow(BT_reach_field_culled_sf) ==
    nrow(BT_reach_field_culled),
  all(c(
    "sample_ID",
    "stat_cat",
    "sample_date",
    "sample_year",
    "obs_flow"
  ) %in% names(BT_reach_field_culled))
)

cat(
  "Original reach-level samples:",
  n_distinct(BT_reach_data$sample_ID),
  "\nCulled samples:",
  n_distinct(BT_reach_field_culled$sample_ID),
  "\nExcluded samples:",
  n_distinct(BT_reach_data$sample_ID) -
    n_distinct(BT_reach_field_culled$sample_ID),
  "\n"
)

BT_reach_field_culled |>
  count(
    sample_year,
    stat_cat,
    .drop = FALSE
  ) |>
  print(n = Inf)

BT_reach_field_culled |>
  summarise(
    n_missing_sample_date = sum(is.na(sample_date)),
    n_missing_sample_year = sum(is.na(sample_year)),
    n_missing_obs_flow = sum(is.na(obs_flow))
  ) |>
  print()
# End sample culling ----------------------------------------------------------

## Checking out sample fire info... -------------------------------------------
sample_fire_summary <- BT_pixel_data |>
  group_by(sample_ID) |>
  summarise(
    fire_n_pixels = n(),
    fire_n_burned_pixels = sum(num_times_burned > 0, na.rm = TRUE),
    fire_prop_burned = fire_n_burned_pixels / fire_n_pixels,
    fire_ever_burned = fire_n_burned_pixels > 0,
    fire_max_sev_num = ifelse(
      all(is.na(sev_num)),
      NA_real_,
      max(sev_num, na.rm = TRUE)
    ),
    fire_mean_sev_num = mean(sev_num, na.rm = TRUE),
    fire_most_recent_year = ifelse(
      all(is.na(ignition_year)),
      NA_real_,
      max(ignition_year, na.rm = TRUE)
    ),
    fire_num_events = n_distinct(
      fire_event[!is.na(fire_event) & fire_event != "None"]
    ),
    .groups = "drop"
  )

BT_reach_field_culled <- BT_reach_field_culled |>
  left_join(sample_fire_summary, by = "sample_ID")
## ================================================

# Clip Bull Trout model stream lines to the HUC12 boundaries -----------------
huc12_clip_geometry <- huc12_map_sf |>
  st_make_valid() |>
  st_union()

bt_baseline_map <- suppressWarnings(
  st_intersection(
    bt_baseline_map_sf,
    huc12_clip_geometry
  )
)

bt_baseline_map <- bt_baseline_map[
  st_geometry_type(bt_baseline_map) %in%
    c("LINESTRING", "MULTILINESTRING"),
]

# Create map points exclusively from the culled sample set
culled_sample_points_sf <- BT_reach_field_culled_sf |>
  mutate(
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    )
  ) |>
  filter(!is.na(stat_cat)) |>
  st_transform(map_crs)

# Keep culled points within the HUC12 map region
culled_sample_points_map <- culled_sample_points_sf[
  lengths(
    st_intersects(
      culled_sample_points_sf,
      huc12_map_sf
    )
  ) > 0,
]

# Crop contextual layers to the HUC12 extent
streams_map <- suppressWarnings(
  st_intersection(
    streams_map_sf |>
      st_make_valid(),
    huc12_clip_geometry
  )
)

streams_map <- streams_map[
  st_geometry_type(streams_map) %in%
    c("LINESTRING", "MULTILINESTRING"),
]

cat(
  "Culled samples mapped:",
  nrow(culled_sample_points_map),
  "\n"
)

## Analysis CRS / Bob Marshall HUC12 geometry ---------------------------------
analysis_crs <- 5070  # NAD83 / Conus Albers, meters

huc12_bob <- huc12_map_sf |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(analysis_crs) |>
  st_make_valid()

huc12_bob_union <- huc12_bob |>
  st_union() |>
  st_make_valid()

huc12_bob_sf <- st_sf(
  geometry = st_sfc(huc12_bob_union, crs = analysis_crs)
)

## Read in barrier shapefile and clip:  ---------------------------------------
barriers_path <- file.path(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project",
  "ArcGIS/BMWC_Fire_Data/Barriers/Barriers_all_07252026.shp"
)

stopifnot(file.exists(barriers_path))

barriers_sf <- st_read(barriers_path, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(analysis_crs) |>
  st_make_valid()

## clip to the Bob.
barriers_sf <- barriers_sf |>
  filter(lengths(st_intersects(geometry, huc12_bob_sf)) > 0)
# -----------------------------------------------------------------------------

################################################################################
############# Culled sample status summary  [CONFUSION MATRIX] #################

# Audit: confirm all culled samples are included/correct ----------------------
sample_model_overlap_distance_m <- 1000  #m
downstream_patch_distance_m <- 1000

retained_huc12_names <- c(
  "Middle White River",
  "Upper White River",
  "Lower Big Salmon Creek",
  "Gorge Creek"
)

retained_huc12s <- huc12_map_sf |>
  filter(name %in% retained_huc12_names) |>
  st_make_valid()

missing_retained_huc12s <- setdiff(
  retained_huc12_names,
  retained_huc12s$name
)

stopifnot(length(missing_retained_huc12s) == 0)

get_lowest_patch_endpoints <- function(model_lines_sf) {
  line_parts <- model_lines_sf |>
    st_cast("LINESTRING", warn = FALSE) |>
    mutate(patch_row_id = row_number())
  
  coords <- st_coordinates(line_parts) |>
    as_tibble() |>
    group_by(L1) |>
    slice_min(Y, n = 1, with_ties = FALSE) |>
    ungroup()
  
  st_as_sf(
    coords,
    coords = c("X", "Y"),
    crs = st_crs(line_parts)
  ) |>
    transmute(
      patch_row_id = L1
    )
}

bt_baseline_downstream_ends <- get_lowest_patch_endpoints(bt_baseline_map)

baseline_sample_audit <- culled_sample_points_map |>
  distinct(sample_ID, .keep_all = TRUE) |>
  mutate(
    overlaps_1980_model = lengths(st_is_within_distance(
      geometry,
      bt_baseline_map,
      dist = sample_model_overlap_distance_m
    )) > 0,
    
    within_1km_downstream_1980 = lengths(st_is_within_distance(
      geometry,
      bt_baseline_downstream_ends,
      dist = downstream_patch_distance_m
    )) > 0,
    
    in_retained_huc12 = lengths(st_intersects(
      geometry,
      retained_huc12s
    )) > 0,
    
    retained_by_1980_or_huc12_rule =
      overlaps_1980_model |
      within_1km_downstream_1980 |
      in_retained_huc12
  )

baseline_sample_audit |>
  st_drop_geometry() |>
  count(
    overlaps_1980_model,
    within_1km_downstream_1980,
    in_retained_huc12,
    retained_by_1980_or_huc12_rule
  ) |>
  print(n = Inf)

baseline_sample_audit |>
  st_drop_geometry() |>
  summarise(
    total_samples = n_distinct(sample_ID),
    retained_by_rule = sum(retained_by_1980_or_huc12_rule == TRUE, na.rm = TRUE),
    not_retained_by_rule = sum(retained_by_1980_or_huc12_rule == FALSE, na.rm = TRUE)
  ) |>
  print()

baseline_samples_not_retained <- baseline_sample_audit |>
  filter(retained_by_1980_or_huc12_rule == FALSE) |>
  st_drop_geometry() |>
  select(
    sample_ID,
    reach_longitude,
    reach_latitude,
    overlaps_1980_model,
    within_1km_downstream_1980,
    in_retained_huc12
  )

print(baseline_samples_not_retained, n = Inf)

manual_expected_present_1980_samples <- c(
  "HUC06_399",
  "HUC13_610",
  "HUC41_405"
)
# -----------------------------------------------------------------------------

# Recalculate 2040 sample status and confusion matrix --------------------------
# Expected presence is based ONLY on spatial overlap with 2040 modeled habitat!
# Barriers are ignored for this matrix/map.

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)

# Read and prepare 2040 projected suitable habitat -----------------------------
mod23_path <- file.path(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project",
  "ArcGIS/BMWC_Fire_Data/bull_trout_model/Revised_2023",
  "BullTroutPatchScenarios_ModerateClimate_Isaak_et_al_2022_AppendixS6.shp"
)

stopifnot(file.exists(mod23_path))

huc12_clip_geometry <- huc12_map_sf |>
  st_make_valid() |>
  st_union()

bt_2040_map <- st_read(mod23_path, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_make_valid() |>
  st_transform(st_crs(huc12_map_sf))

bt_2040_map <- suppressWarnings(
  st_intersection(bt_2040_map, huc12_clip_geometry)
) |>
  filter(!st_is_empty(geometry)) |>
  st_collection_extract("LINESTRING", warn = FALSE) |>
  filter(!st_is_empty(geometry))

bt_2040_map_aligned <- bt_2040_map |>
  st_transform(st_crs(culled_sample_points_map))


# Recalculate 2040 expected presence and sample status -------------------------
culled_sample_points_map <- culled_sample_points_map |>
  distinct(sample_ID, .keep_all = TRUE) |>
  mutate(
    field_detection = case_when(
      bt_status_binary %in% c(1, "1", "Present", "present", "Detected", "detected") ~ "Present",
      bt_status_binary %in% c(0, "0", "Absent", "absent", "Not detected", "not detected") ~ "Absent",
      TRUE ~ NA_character_
    ),
    overlaps_2040_model_spatial = lengths(
      st_is_within_distance(
        geometry,
        bt_2040_map_aligned,
        dist = sample_model_overlap_distance_m
      )
    ) > 0,
    
    manual_exclude_2040_expected_present = sample_ID %in% c("HUC09_167"),  # Fix HUC09_167 (doesn't overlap 2040)
    expected_present_2040 = overlaps_2040_model_spatial & !manual_exclude_2040_expected_present,
    stat_cat_2040 = case_when(
      field_detection == "Present" & expected_present_2040 ~ "Present, Correct",
      field_detection == "Absent" & expected_present_2040 ~ "Absent, Incorrect",
      field_detection == "Present" & !expected_present_2040 ~ "Present, Incorrect",
      field_detection == "Absent" & !expected_present_2040 ~ "Absent, Correct",
      TRUE ~ NA_character_
    ),
    stat_cat_2040 = factor(stat_cat_2040, levels = names(status_colors))
  )

stopifnot(n_distinct(culled_sample_points_map$sample_ID) == 127)

# QA summaries printed to console ---------------------------------------------
culled_sample_points_map |>
  st_drop_geometry() |>
  summarise(
    n_samples = n_distinct(sample_ID),
    n_expected_present_2040 = sum(expected_present_2040, na.rm = TRUE),
    n_expected_absent_2040 = sum(!expected_present_2040, na.rm = TRUE),
    n_missing_field_detection = sum(is.na(field_detection)),
    n_missing_stat_cat_2040 = sum(is.na(stat_cat_2040))
  ) |>
  print(width = Inf)

culled_sample_points_map |>
  st_drop_geometry() |>
  count(stat_cat_2040, .drop = FALSE, name = "n") |>
  print(n = Inf)


# Update downstream stat_cat to corrected 2040 classification ------------------
BT_reach_field_culled_2040 <- BT_reach_field_culled |>
  select(-any_of("stat_cat")) |>
  left_join(
    culled_sample_points_map |>
      st_drop_geometry() |>
      distinct(sample_ID, stat_cat_2040) |>
      transmute(sample_ID, stat_cat = stat_cat_2040),
    by = "sample_ID"
  ) |>
  mutate(stat_cat = factor(stat_cat, levels = names(status_colors)))

BT_reach_field_culled <- BT_reach_field_culled_2040


# Map corrected 2040 sample status --------------------------------------------
# Prepare physical barrier shapefile for map 
# Map corrected 2040 sample status --------------------------------------------
# Build map data from corrected stat_cat_2040
sample_status_map_data_2040 <- culled_sample_points_map |>
  distinct(sample_ID, stat_cat_2040, .keep_all = TRUE) |>
  select(
    sample_ID,
    field_detection,
    expected_present_2040,
    stat_cat_2040,
    geometry
  ) |>
  mutate(
    stat_cat_model = factor(stat_cat_2040, levels = names(status_colors))
  )

stopifnot(n_distinct(sample_status_map_data_2040$sample_ID) == 127)

# Prepare physical barrier shapefile for map
analysis_crs <- 5070  # NAD83 / Conus Albers, meters

if (!exists("barriers_sf")) {
  barriers_path <- file.path(
    "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project",
    "ArcGIS/BMWC_Fire_Data/Barriers/Barriers_all_07252026.shp"
  )
  
  stopifnot(file.exists(barriers_path))
  
  barriers_sf <- st_read(barriers_path, quiet = TRUE) |>
    st_zm(drop = TRUE, what = "ZM") |>
    st_make_valid()
}

barriers_map <- barriers_sf |>
  st_transform(st_crs(huc12_map_sf)) |>
  filter(lengths(st_intersects(geometry, huc12_map_sf)) > 0)

# Map counts for dynamic subtitle + legend labels
map_2040_category_counts <- sample_status_map_data_2040 |>
  st_drop_geometry() |>
  count(stat_cat_model, name = "n", .drop = FALSE)

map_2040_total_n <- sum(map_2040_category_counts$n, na.rm = TRUE)

status_labels_2040 <- map_2040_category_counts |>
  mutate(label = paste0(stat_cat_model, " (n = ", n, ")")) |>
  select(stat_cat_model, label) |>
  tibble::deframe()

# 2040 sample status map + physical barriers
Sample_status_2040_habitat_only_map <- ggplot() +
  geom_sf(data = huc12_map_sf, fill = "#F2F2F2", color = "white", linewidth = 0.25) +
  geom_sf(data = streams_map, color = "#BDBDBD", linewidth = 0.20, alpha = 0.65) +
  geom_sf(data = bt_2040_map_aligned, color = "#08519C", linewidth = 0.75, alpha = 0.90) +
  geom_sf(data = huc12_map_sf, fill = NA, color = "#636363", linewidth = 0.35) +
  geom_sf(
    data = sample_status_map_data_2040,
    aes(fill = stat_cat_model),
    shape = 21,
    color = "black",
    stroke = 0.35,
    size = 3,
    alpha = 0.95
  ) +
  geom_sf(
    data = barriers_map,
    shape = 4,
    color = "#DE2D26",
    size = 2.4,
    stroke = 0.9,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = status_colors,
    breaks = names(status_colors),
    labels = status_labels_2040,
    name = NULL,
    drop = FALSE
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(size = 3, alpha = 0.95)
    )
  ) +
  coord_sf(datum = NA, expand = FALSE) +
  labs(
    title = "Sample Status by 2040 Projected Suitable Habitat",
    subtitle = paste0(
      "Total culled samples = ", map_2040_total_n,
      "; Present, Correct = ", map_2040_category_counts$n[map_2040_category_counts$stat_cat_model == "Present, Correct"],
      "; Absent, Correct = ", map_2040_category_counts$n[map_2040_category_counts$stat_cat_model == "Absent, Correct"],
      "; Present, Incorrect = ", map_2040_category_counts$n[map_2040_category_counts$stat_cat_model == "Present, Incorrect"],
      "; Absent, Incorrect = ", map_2040_category_counts$n[map_2040_category_counts$stat_cat_model == "Absent, Incorrect"]
    ),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.box = "horizontal",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5)
  )

print(Sample_status_2040_habitat_only_map)
# -----------------------------------------------------------------------------

# Print corrected 2040 confusion matrix to console -----------------------------
culled_stat_cat_summary_2040 <- culled_sample_points_map |>
  st_drop_geometry() |>
  distinct(sample_ID, field_detection, expected_present_2040, stat_cat_2040) |>
  mutate(
    model_habitat = case_when(
      expected_present_2040 ~ "2040 Suitable Habitat: Present",
      !expected_present_2040 ~ "2040 Unsuitable Habitat: Absent",
      TRUE ~ NA_character_
    ),
    field_detection = paste("Field:", field_detection)
  ) |>
  filter(!is.na(field_detection), !is.na(model_habitat)) |>
  count(field_detection, model_habitat, name = "n") |>
  mutate(
    percent_total = round(
      100 * n / n_distinct(culled_sample_points_map$sample_ID),
      1
    ),
    cell = paste0(n, " (", percent_total, "%)")
  ) |>
  select(field_detection, model_habitat, cell) |>
  pivot_wider(
    names_from = model_habitat,
    values_from = cell,
    values_fill = "0 (0%)"
  ) |>
  mutate(
    field_detection = factor(
      field_detection,
      levels = c("Field: Present", "Field: Absent")
    )
  ) |>
  arrange(field_detection)

print(culled_stat_cat_summary_2040, n = Inf, width = Inf)


# Write corrected 2040 culled sample shapefile ---------------------------------
culled_bt_sample_sites_2040_path <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/",
  "ArcGIS/BMWC_Fire_Data/eDNA_Results/culled_bt_sample_sites_2040.shp"
)

culled_sample_points_2040_sf <- culled_sample_points_map |>
  mutate(stat_cat = stat_cat_2040) |>
  select(
    sample_ID,
    stat_cat,
    field_detection,
    expected_present_2040,
    overlaps_2040_model_spatial,
    geometry
  )

#st_write(
#  culled_sample_points_2040_sf,
#  culled_bt_sample_sites_2040_path,
#  delete_dsn = TRUE
#)
# -----------------------------------------------------------------------------


### Downstream Barrier Map + 2040 Habitat Projection --------------------------
library(dplyr)
library(ggplot2)
library(sf)

# 1. Identify culled samples with downstream barriers --------------------------
barrier_col <- case_when(
  "no_of_ds_barriers" %in% names(culled_sample_points_map) ~ "no_of_ds_barriers",
  "no_ds_barriers" %in% names(culled_sample_points_map) ~ "no_ds_barriers",
  TRUE ~ NA_character_
)

stopifnot(!is.na(barrier_col))

culled_barrier_points <- culled_sample_points_map |>
  distinct(sample_ID, .keep_all = TRUE) |>
  mutate(
    no_of_ds_barriers_map = as.numeric(.data[[barrier_col]]),
    has_downstream_barrier = no_of_ds_barriers_map > 0
  )

stopifnot(n_distinct(culled_barrier_points$sample_ID) == 127)

# 2. Add 2040 model-overlap status for above-barrier samples -------------------
bt_2040_for_barrier_map <- bt_2040_map |>
  st_transform(st_crs(culled_barrier_points))

downstream_barrier_samples <- culled_barrier_points |>
  filter(has_downstream_barrier) |>
  mutate(
    overlaps_2040_model = lengths(
      st_is_within_distance(
        geometry,
        bt_2040_for_barrier_map,
        dist = sample_model_overlap_distance_m
      )
    ) > 0,
    barrier_map_class = case_when(
      overlaps_2040_model ~ "Above barrier + overlaps 2040 habitat",
      TRUE ~ "Above barrier + no 2040 habitat overlap"
    ),
    barrier_map_class = factor(
      barrier_map_class,
      levels = c(
        "Above barrier + overlaps 2040 habitat",
        "Above barrier + no 2040 habitat overlap"
      )
    )
  )

downstream_barrier_samples |>
  st_drop_geometry() |>
  count(barrier_map_class, name = "n") |>
  print(n = Inf)

downstream_barrier_samples |>
  st_drop_geometry() |>
  select(
    sample_ID,
    no_of_ds_barriers = no_of_ds_barriers_map,
    overlaps_2040_model,
    barrier_map_class,
    any_of(c("stat_cat_2040", "stat_cat", "field_detection"))
  ) |>
  arrange(desc(overlaps_2040_model), desc(no_of_ds_barriers), sample_ID) |>
  print(n = Inf, width = Inf)

# 3. Map: 2040 projection + above-barrier samples ------------------------------
Downstream_barrier_2040_overlap_map <- ggplot() +
  geom_sf(
    data = huc12_map_sf,
    fill = "#F2F2F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = streams_map,
    color = "#BDBDBD",
    linewidth = 0.20,
    alpha = 0.65
  ) +
  geom_sf(
    data = bt_2040_for_barrier_map,
    color = "#08519C",
    linewidth = 0.85,
    alpha = 0.90
  ) +
  geom_sf(
    data = downstream_barrier_samples,
    aes(fill = barrier_map_class),
    shape = 21,
    color = "black",
    size = 4,
    stroke = 0.45,
    alpha = 0.95
  ) +
  scale_fill_manual(
    values = c(
      "Above barrier + overlaps 2040 habitat" = "#D7301F",
      "Above barrier + no 2040 habitat overlap" = "gray"
    ),
    name = "Above-barrier sample"
  ) +
  scale_size_continuous(
    range = c(3, 6),
    name = "Downstream barriers"
  ) +
  coord_sf(
    datum = NA,
    expand = FALSE
  ) +
  labs(
    title = "Above-barrier culled samples and 2040 projected suitable habitat",
    subtitle = paste0(
      n_distinct(downstream_barrier_samples$sample_ID),
      " above-barrier samples from 127 culled samples"
    ),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(Downstream_barrier_2040_overlap_map)
# ----------------------------------------------------------------------------



################################################################################
############# Evaluating trends - histograms, box plots, maps #################

### SUMMER CLIMATIC WATER DEFICIT Z-SCORE
# Culled-sample figures and Bull Trout map ---------------------------------
stopifnot(
  exists("BT_reach_field_culled"),
  nrow(BT_reach_field_culled) > 0
)

cat(
  "Unique culled samples:",
  n_distinct(BT_reach_field_culled$sample_ID),
  "\n"
)

# Prepare prior-year and sample-year CWD data --------------------------------
cwd_plot_data_culled <- bind_rows(
  BT_reach_field_culled |>
    filter(sample_year == 2023) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2023 samples: Prior-year CWD (2022)",
      cwd_zscore = cwdz_JJA_2022_TC
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2023) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2023 samples: Sample-year CWD (2023)",
      cwd_zscore = cwdz_JJA_2023_TC
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2024) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2024 samples: Prior-year CWD (2023)",
      cwd_zscore = cwdz_JJA_2023_TC
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2024) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2024 samples: Sample-year CWD (2024)",
      cwd_zscore = cwdz_JJA_2024_TC
    )
) |>
  mutate(
    panel = factor(
      panel,
      levels = c(
        "2023 samples: Prior-year CWD (2022)",
        "2023 samples: Sample-year CWD (2023)",
        "2024 samples: Prior-year CWD (2023)",
        "2024 samples: Sample-year CWD (2024)"
      )
    ),
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    )
  )

# Confirm the culled sample counts in each panel
cwd_plot_data_culled |>
  count(
    panel,
    stat_cat,
    .drop = FALSE
  ) |>
  print(n = Inf)

# Four-panel CWD boxplots -----------------------------------------------------
CWD_culled_boxplots <- ggplot(
  cwd_plot_data_culled,
  aes(
    x = stat_cat,
    y = cwd_zscore,
    color = stat_cat,
    fill = stat_cat
  )
) +
  geom_boxplot(
    alpha = 0.20,
    width = 0.65,
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 2.2,
    alpha = 0.70,
    na.rm = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +
  status_color_scale +
  status_fill_scale +
  scale_x_discrete(
    labels = status_labels
  ) +
  labs(
    title = "Summer CWD by bull trout classification",
    subtitle = paste(
      "Culled sample set: prior-year and sample-year\n",
      "reach-average z-scores relative to 1986–2015"
    ),
    x = NULL,
    y = "Summer CWD z-score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )

print(CWD_culled_boxplots)

# Four-panel CWD histograms ---------------------------------------------------
cwd_values <- cwd_plot_data_culled$cwd_zscore
cwd_values <- cwd_values[is.finite(cwd_values)]

stopifnot(length(cwd_values) > 1)

cwd_range <- diff(range(cwd_values))

# Freedman-Diaconis bin width
cwd_binwidth <- 2 *
  IQR(cwd_values) /
  length(cwd_values)^(1 / 3)

# Fallback for little or no spread
if (
  !is.finite(cwd_binwidth) ||
  cwd_binwidth <= 0
) {
  cwd_binwidth <- cwd_range / 12
}

# Keep approximately 8–30 bins
if (
  is.finite(cwd_range) &&
  cwd_range > 0
) {
  cwd_binwidth <- max(
    cwd_range / 30,
    min(
      cwd_binwidth,
      cwd_range / 8
    )
  )
}

cat(
  "Culled-sample histogram bin width:",
  round(cwd_binwidth, 3),
  "\n"
)

CWD_culled_histograms <- ggplot(
  cwd_plot_data_culled,
  aes(
    x = cwd_zscore,
    fill = stat_cat,
    color = stat_cat
  )
) +
  geom_histogram(
    binwidth = cwd_binwidth,
    boundary = 0,
    position = "stack",
    color = "white",
    linewidth = 0.25,
    alpha = 1,
    na.rm = TRUE
  ) +
  geom_rug(
    sides = "b",
    alpha = 0.55,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +
  status_color_scale +
  status_fill_scale +
  labs(
    title = "Distribution of summer CWD",
    subtitle = paste(
      "Culled sample set; consistent bins across",
      "prior-year and sample-year panels"
    ),
    x = "Summer CWD z-score",
    y = "Number of sample reaches",
    fill = "Sample status",
    color = "Sample status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE
    ),
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

print(CWD_culled_histograms)
# Optional exports -----------------------------------------------------------
# ggsave(
#   filename = file.path(
#     folder_path,
#     "CWD_culled_four_panel_boxplots.png"
#   ),
#   plot = CWD_culled_boxplots,
#   width = 12,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   filename = file.path(
#     folder_path,
#     "CWD_culled_four_panel_histograms.png"
#   ),
#   plot = CWD_culled_histograms,
#   width = 12,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   filename = file.path(
#     folder_path,
#     "Bull_Trout_culled_sample_status_map.png"
#   ),
#   plot = culled_sample_map,
#   width = 10,
#   height = 10,
#   dpi = 300
# )

# End culled-sample CWD plots and map ----------------------------------------

### APRIL SWE Z-SCORE 
# Culled-sample April SWE plots -----------------------------------------------
stopifnot(
  all(
    c(
      "swez_Apr_2022_TC",
      "swez_Apr_2023_TC",
      "swez_Apr_2024_TC"
    ) %in% names(BT_reach_field_culled)
  )
)

# Prepare prior-year and sample-year SWE data --------------------------------
swe_plot_data_culled <- bind_rows(
  BT_reach_field_culled |>
    filter(sample_year == 2023) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2023 samples: Prior-year April SWE (2022)",
      swe_zscore = swez_Apr_2022_TC
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2023) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2023 samples: Sample-year April SWE (2023)",
      swe_zscore = swez_Apr_2023_TC
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2024) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2024 samples: Prior-year April SWE (2023)",
      swe_zscore = swez_Apr_2023_TC
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2024) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2024 samples: Sample-year April SWE (2024)",
      swe_zscore = swez_Apr_2024_TC
    )
) |>
  mutate(
    panel = factor(
      panel,
      levels = c(
        "2023 samples: Prior-year April SWE (2022)",
        "2023 samples: Sample-year April SWE (2023)",
        "2024 samples: Prior-year April SWE (2023)",
        "2024 samples: Sample-year April SWE (2024)"
      )
    ),
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    )
  )

# Confirm sample counts
swe_plot_data_culled |>
  count(
    panel,
    stat_cat,
    .drop = FALSE
  ) |>
  print(n = Inf)

# Four-panel April SWE boxplots ----------------------------------------------
SWE_culled_boxplots <- ggplot(
  swe_plot_data_culled,
  aes(
    x = stat_cat,
    y = swe_zscore,
    color = stat_cat,
    fill = stat_cat
  )
) +
  geom_boxplot(
    alpha = 0.20,
    width = 0.65,
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 2.2,
    alpha = 0.70,
    na.rm = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +
  status_color_scale +
  status_fill_scale +
  scale_x_discrete(
    labels = status_labels
  ) +
  labs(
    title = "April SWE by bull trout classification",
    subtitle = paste(
      "Culled sample set: prior-year and sample-year",
      "reach-average z-scores relative to 1986–2015"
    ),
    x = NULL,
    y = "April SWE z-score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )

print(SWE_culled_boxplots)

# Calculate a consistent histogram bin width ---------------------------------
swe_values <- swe_plot_data_culled$swe_zscore
swe_values <- swe_values[is.finite(swe_values)]

stopifnot(length(swe_values) > 1)

swe_range <- diff(range(swe_values))

swe_binwidth <- 2 *
  IQR(swe_values) /
  length(swe_values)^(1 / 3)

if (
  !is.finite(swe_binwidth) ||
  swe_binwidth <= 0
) {
  swe_binwidth <- swe_range / 12
}

# Keep approximately 8–30 bins
if (
  is.finite(swe_range) &&
  swe_range > 0
) {
  swe_binwidth <- max(
    swe_range / 30,
    min(
      swe_binwidth,
      swe_range / 8
    )
  )
}

cat(
  "April SWE histogram bin width:",
  round(swe_binwidth, 3),
  "\n"
)

# Four-panel stacked April SWE histograms ------------------------------------
SWE_culled_histograms <- ggplot(
  swe_plot_data_culled,
  aes(
    x = swe_zscore,
    fill = stat_cat,
    color = stat_cat
  )
) +
  geom_histogram(
    binwidth = swe_binwidth,
    boundary = 0,
    position = "stack",
    color = "white",
    linewidth = 0.25,
    alpha = 1,
    na.rm = TRUE
  ) +
  geom_rug(
    sides = "b",
    alpha = 0.55,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +
  status_color_scale +
  status_fill_scale +
  labs(
    title = "Distribution of April SWE",
    subtitle = paste(
      "Culled sample set; consistent bins across",
      "prior-year and sample-year panels"
    ),
    x = "April SWE z-score",
    y = "Number of sample reaches",
    fill = "Sample status",
    color = "Sample status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE
    ),
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

print(SWE_culled_histograms)

# Optional exports -----------------------------------------------------------
# ggsave(
#   filename = file.path(
#     folder_path,
#     "SWE_culled_four_panel_boxplots.png"
#   ),
#   plot = SWE_culled_boxplots,
#   width = 12,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   filename = file.path(
#     folder_path,
#     "SWE_culled_four_panel_histograms.png"
#   ),
#   plot = SWE_culled_histograms,
#   width = 12,
#   height = 8,
#   dpi = 300
# )

# End culled-sample April SWE plots ------------------------------------------

### SUMMER PRECIP Z-SCORE
# Culled-sample summer precipitation plots -----------------------------------
stopifnot(
  all(
    c(
      "pptz_JJA_2022_PR",
      "pptz_JJA_2023_PR",
      "pptz_JJA_2024_PR"
    ) %in% names(BT_reach_field_culled)
  )
)

# Prepare prior-year and sample-year precipitation data ----------------------
ppt_plot_data_culled <- bind_rows(
  BT_reach_field_culled |>
    filter(sample_year == 2023) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2023 samples: Prior-year summer precipitation (2022)",
      ppt_zscore = pptz_JJA_2022_PR
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2023) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2023 samples: Sample-year summer precipitation (2023)",
      ppt_zscore = pptz_JJA_2023_PR
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2024) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2024 samples: Prior-year summer precipitation (2023)",
      ppt_zscore = pptz_JJA_2023_PR
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2024) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2024 samples: Sample-year summer precipitation (2024)",
      ppt_zscore = pptz_JJA_2024_PR
    )
) |>
  mutate(
    panel = factor(
      panel,
      levels = c(
        "2023 samples: Prior-year summer precipitation (2022)",
        "2023 samples: Sample-year summer precipitation (2023)",
        "2024 samples: Prior-year summer precipitation (2023)",
        "2024 samples: Sample-year summer precipitation (2024)"
      )
    ),
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    )
  )

# Confirm sample counts
ppt_plot_data_culled |>
  count(
    panel,
    stat_cat,
    .drop = FALSE
  ) |>
  print(n = Inf)

# Four-panel precipitation boxplots ------------------------------------------
PPT_culled_boxplots <- ggplot(
  ppt_plot_data_culled,
  aes(
    x = stat_cat,
    y = ppt_zscore,
    color = stat_cat,
    fill = stat_cat
  )
) +
  geom_boxplot(
    alpha = 0.20,
    width = 0.65,
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 2.2,
    alpha = 0.70,
    na.rm = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +
  status_color_scale +
  status_fill_scale +
  scale_x_discrete(
    labels = status_labels
  ) +
  labs(
    title = "Summer precipitation by bull trout classification",
    subtitle = paste(
      "Culled sample set: prior-year and sample-year",
      "reach-average z-scores relative to 1986–2015"
    ),
    x = NULL,
    y = "Summer precipitation z-score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )

print(PPT_culled_boxplots)

# Calculate a consistent histogram bin width ---------------------------------
ppt_values <- ppt_plot_data_culled$ppt_zscore
ppt_values <- ppt_values[is.finite(ppt_values)]

stopifnot(length(ppt_values) > 1)

ppt_range <- diff(range(ppt_values))

ppt_binwidth <- 2 *
  IQR(ppt_values) /
  length(ppt_values)^(1 / 3)

if (
  !is.finite(ppt_binwidth) ||
  ppt_binwidth <= 0
) {
  ppt_binwidth <- ppt_range / 12
}

# Keep approximately 8–30 bins
if (
  is.finite(ppt_range) &&
  ppt_range > 0
) {
  ppt_binwidth <- max(
    ppt_range / 30,
    min(
      ppt_binwidth,
      ppt_range / 8
    )
  )
}

cat(
  "Precipitation histogram bin width:",
  round(ppt_binwidth, 3),
  "\n"
)

# Four-panel stacked precipitation histograms --------------------------------
PPT_culled_histograms <- ggplot(
  ppt_plot_data_culled,
  aes(
    x = ppt_zscore,
    fill = stat_cat,
    color = stat_cat
  )
) +
  geom_histogram(
    binwidth = ppt_binwidth,
    boundary = 0,
    position = "stack",
    color = "white",
    linewidth = 0.25,
    alpha = 1,
    na.rm = TRUE
  ) +
  geom_rug(
    sides = "b",
    alpha = 0.55,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +
  status_color_scale +
  status_fill_scale +
  labs(
    title = "Distribution of summer precipitation",
    subtitle = paste(
      "Culled sample set; consistent bins across",
      "prior-year and sample-year panels"
    ),
    x = "Summer precipitation z-score",
    y = "Number of sample reaches",
    fill = "Sample status",
    color = "Sample status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE
    ),
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

print(PPT_culled_histograms)

# Optional exports -----------------------------------------------------------
# ggsave(
#   filename = file.path(
#     folder_path,
#     "PPT_culled_four_panel_boxplots.png"
#   ),
#   plot = PPT_culled_boxplots,
#   width = 12,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   filename = file.path(
#     folder_path,
#     "PPT_culled_four_panel_histograms.png"
#   ),
#   plot = PPT_culled_histograms,
#   width = 12,
#   height = 8,
#   dpi = 300
# )

# End culled-sample summer precipitation plots -------------------------------

### SUMMER AVG TEMP Z-SCORE
# Culled-sample summer mean-temperature plots --------------------------------
stopifnot(
  all(
    c(
      "tmeanz_JJA_2022_PR",
      "tmeanz_JJA_2023_PR",
      "tmeanz_JJA_2024_PR"
    ) %in% names(BT_reach_field_culled)
  )
)

# Prepare prior-year and sample-year temperature data ------------------------
tmean_plot_data_culled <- bind_rows(
  BT_reach_field_culled |>
    filter(sample_year == 2023) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2023 samples: Prior-year summer temperature (2022)",
      tmean_zscore = tmeanz_JJA_2022_PR
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2023) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2023 samples: Sample-year summer temperature (2023)",
      tmean_zscore = tmeanz_JJA_2023_PR
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2024) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2024 samples: Prior-year summer temperature (2023)",
      tmean_zscore = tmeanz_JJA_2023_PR
    ),
  
  BT_reach_field_culled |>
    filter(sample_year == 2024) |>
    transmute(
      sample_ID,
      stat_cat,
      panel = "2024 samples: Sample-year summer temperature (2024)",
      tmean_zscore = tmeanz_JJA_2024_PR
    )
) |>
  mutate(
    panel = factor(
      panel,
      levels = c(
        "2023 samples: Prior-year summer temperature (2022)",
        "2023 samples: Sample-year summer temperature (2023)",
        "2024 samples: Prior-year summer temperature (2023)",
        "2024 samples: Sample-year summer temperature (2024)"
      )
    ),
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    )
  )

# Confirm sample counts
tmean_plot_data_culled |>
  dplyr::count(
    panel,
    stat_cat,
    .drop = FALSE
  ) |>
  tibble::as_tibble() |>
  print(n = Inf)

# Four-panel summer temperature boxplots -------------------------------------
Tmean_culled_boxplots <- ggplot(
  tmean_plot_data_culled,
  aes(
    x = stat_cat,
    y = tmean_zscore,
    color = stat_cat,
    fill = stat_cat
  )
) +
  geom_boxplot(
    alpha = 0.20,
    width = 0.65,
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 2.2,
    alpha = 0.70,
    na.rm = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +
  status_color_scale +
  status_fill_scale +
  scale_x_discrete(
    labels = status_labels
  ) +
  labs(
    title = "Summer mean temperature by bull trout classification",
    subtitle = paste(
      "Culled sample set: prior-year and sample-year",
      "reach-average z-scores relative to 1986–2015"
    ),
    x = NULL,
    y = "Summer mean-temperature z-score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )

print(Tmean_culled_boxplots)

# Calculate a consistent histogram bin width ---------------------------------
tmean_values <- tmean_plot_data_culled$tmean_zscore
tmean_values <- tmean_values[
  is.finite(tmean_values)
]

stopifnot(length(tmean_values) > 1)

tmean_range <- diff(range(tmean_values))

tmean_binwidth <- 2 *
  IQR(tmean_values) /
  length(tmean_values)^(1 / 3)

if (
  !is.finite(tmean_binwidth) ||
  tmean_binwidth <= 0
) {
  tmean_binwidth <- tmean_range / 12
}

# Keep approximately 8–30 bins
if (
  is.finite(tmean_range) &&
  tmean_range > 0
) {
  tmean_binwidth <- max(
    tmean_range / 30,
    min(
      tmean_binwidth,
      tmean_range / 8
    )
  )
}

cat(
  "Summer temperature histogram bin width:",
  round(tmean_binwidth, 3),
  "\n"
)

# Four-panel stacked temperature histograms ----------------------------------
Tmean_culled_histograms <- ggplot(
  tmean_plot_data_culled,
  aes(
    x = tmean_zscore,
    fill = stat_cat,
    color = stat_cat
  )
) +
  geom_histogram(
    binwidth = tmean_binwidth,
    boundary = 0,
    position = "stack",
    color = "white",
    linewidth = 0.25,
    alpha = 1,
    na.rm = TRUE
  ) +
  geom_rug(
    sides = "b",
    alpha = 0.55,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.5
  ) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +
  status_color_scale +
  status_fill_scale +
  scale_x_continuous(
    expand = expansion(
      mult = c(0.02, 0.08)
    )
  ) +
  labs(
    title = "Distribution of summer mean temperature",
    subtitle = paste(
      "Culled sample set; consistent bins across",
      "prior-year and sample-year panels"
    ),
    x = "Summer mean-temperature z-score",
    y = "Number of sample reaches",
    fill = "Sample status",
    color = "Sample status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE
    ),
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

print(Tmean_culled_histograms)

# Optional exports -----------------------------------------------------------
# ggsave(
#   filename = file.path(
#     folder_path,
#     "Tmean_culled_four_panel_boxplots.png"
#   ),
#   plot = Tmean_culled_boxplots,
#   width = 12,
#   height = 8,
#   dpi = 300
# )

# ggsave(
#   filename = file.path(
#     folder_path,
#     "Tmean_culled_four_panel_histograms.png"
#   ),
#   plot = Tmean_culled_histograms,
#   width = 12,
#   height = 8,
#   dpi = 300
# )

# End culled-sample summer temperature plots ---------------------------------

### ELEVATION
# Culled-sample elevation histogram ------------------------------------------
library(dplyr)
library(ggplot2)
library(sf)
library(terra)

# Load a fresh DEM object
dem10m_path <- file.path(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project",
  "ArcGIS/BMWC_Fire_Data/10m_DEM_SF_Flathead_HUC8",
  "South_Fork_Flathead_DEM.tif"
)

stopifnot(file.exists(dem10m_path))
dem10m <- terra::rast(dem10m_path)
dem_crs <- sf::st_crs(
  terra::crs(dem10m)
)
stopifnot(!is.na(dem_crs))

# Join sample classifications to authoritative locations ----------------------
elevation_points_sf <- BT_reach_field_culled_sf |>
  select(
    sample_ID,
    stat_cat,
    geometry
  ) |>
  filter(!is.na(stat_cat))

# Extract elevation using the DEM CRS ----------------------------------------
# Transform points to the DEM CRS
elevation_points_dem_sf <- elevation_points_sf |>
  sf::st_transform(dem_crs)

elevation_extraction <- terra::extract(
  dem10m,
  terra::vect(elevation_points_dem_sf)
)


# First terra column is the feature ID
elevation_points_sf$elevation_m <- as.numeric(
  elevation_extraction[[2]]
)

# Prepare nonspatial plotting data
elevation_plot_data_culled <- elevation_points_sf |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  transmute(
    sample_ID = as.character(sample_ID),
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    ),
    elevation_m
  ) |>
  filter(
    is.finite(elevation_m),
    !is.na(stat_cat)
  )

stopifnot(nrow(elevation_plot_data_culled) > 1)

cat(
  "Samples with extracted elevation:",
  nrow(elevation_plot_data_culled),
  "\n"
)

elevation_plot_data_culled |>
  count(stat_cat, .drop = FALSE) |>
  print(n = Inf)

summary(elevation_plot_data_culled$elevation_m)

# Calculate histogram bin width
elevation_values <- elevation_plot_data_culled$elevation_m
elevation_range <- diff(range(elevation_values))

elevation_binwidth <- 2 *
  IQR(elevation_values) /
  length(elevation_values)^(1 / 3)

if (
  !is.finite(elevation_binwidth) ||
  elevation_binwidth <= 0
) {
  elevation_binwidth <- if (
    is.finite(elevation_range) &&
    elevation_range > 0
  ) {
    elevation_range / 12
  } else {
    1
  }
}

# Keep approximately 8–30 bins
if (
  is.finite(elevation_range) &&
  elevation_range > 0
) {
  elevation_binwidth <- max(
    elevation_range / 30,
    min(
      elevation_binwidth,
      elevation_range / 8
    )
  )
}

cat(
  "Elevation histogram bin width:",
  round(elevation_binwidth, 1),
  "m\n"
)

# Single stacked elevation histogram ------------------------------------------
Elevation_culled_histogram <- ggplot(
  elevation_plot_data_culled,
  aes(
    x = elevation_m,
    fill = stat_cat
  )
) +
  geom_histogram(
    binwidth = elevation_binwidth,
    boundary = 0,
    position = "stack",
    color = "white",
    linewidth = 0.25,
    na.rm = TRUE
  ) +
  geom_rug(
    aes(color = stat_cat),
    sides = "b",
    alpha = 0.55,
    linewidth = 0.45
  ) +
  status_fill_scale +
  status_color_scale +
  scale_x_continuous(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  labs(
    title = "Distribution of sample site elevation",
    #subtitle = "Culled bull trout sample set",
    x = "Elevation (m)",
    y = "Number of sample reaches",
    fill = "Sample status",
    color = "Sample status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  guides(
    color = "none",
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

print(Elevation_culled_histogram)
# Optional export ------------------------------------------------------------
# ggsave(
#   filename = file.path(
#     folder_path,
#     "Elevation_culled_histogram.png"
#   ),
#   plot = Elevation_culled_histogram,
#   width = 10,
#   height = 6,
#   dpi = 300
# )
# Elevation boxplot -----------------------------------------------------------
Elevation_culled_boxplot <- ggplot(
  elevation_plot_data_culled,
  aes(
    x = stat_cat,
    y = elevation_m,
    color = stat_cat,
    fill = stat_cat
  )
) +
  geom_boxplot(
    alpha = 0.20,
    width = 0.65,
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 2.2,
    alpha = 0.70,
    na.rm = TRUE
  ) +
  status_fill_scale +
  status_color_scale +
  scale_x_discrete(
    labels = status_labels
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Sample site elevation by bull trout classification",
    #subtitle = "Culled bull trout sample set",
    x = NULL,
    y = "Elevation (m)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )

print(Elevation_culled_boxplot)
# End culled-sample elevation plots --------------------------------------

### JULIAN DAY / SAMPLING DATE
# Pooled seasonal sampling-date histogram ------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)

stopifnot(
  exists("BT_reach_field_culled"),
  exists("sample_field_info"),
  exists("status_colors")
)

# Place dates from both years onto a shared June–September calendar.
# The reference year 2001 is arbitrary and is not a leap year.
sampling_date_plot_data <- BT_reach_field_culled |>
  transmute(
    sample_ID = as.character(sample_ID),
    stat_cat,
    sample_date = as.Date(sample_date),
    sample_year
  ) |>
  distinct(sample_ID, .keep_all = TRUE) |>
  filter(
    !is.na(sample_date),
    month(sample_date) %in% 6:9
  ) |>
  mutate(
    seasonal_date = make_date(
      year = 2001,
      month = month(sample_date),
      day = day(sample_date)
    ),
    season_day = as.integer(
      seasonal_date - as.Date("2001-06-01")
    ) + 1L,
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    )
  ) |>
  filter(!is.na(stat_cat))

stopifnot(nrow(sampling_date_plot_data) > 1)

cat(
  "Culled samples with June–September dates:",
  nrow(sampling_date_plot_data),
  "\n"
)

sampling_date_plot_data |>
  count(sample_year, stat_cat, .drop = FALSE) |>
  print(n = Inf)

# Month positions on the shared seasonal axis
season_month_dates <- as.Date(c(
  "2001-06-01",
  "2001-07-01",
  "2001-08-01",
  "2001-09-01",
  "2001-10-01"
))

season_month_breaks <- as.integer(
  season_month_dates - as.Date("2001-06-01")
) + 1L

# Plot the histogram ---------------------------------------------------------
Sampling_date_culled_histogram <- ggplot(
  sampling_date_plot_data,
  aes(
    x = season_day,
    fill = stat_cat
  )
) +
  geom_histogram(
    binwidth = 7,
    boundary = 1,
    position = "stack",
    color = "white",
    linewidth = 0.25,
    na.rm = TRUE
  ) +
  geom_rug(
    aes(color = stat_cat),
    sides = "b",
    alpha = 0.55,
    linewidth = 0.45
  ) +
  status_fill_scale +
  status_color_scale +
  scale_x_continuous(
    breaks = season_month_breaks,
    labels = c(
      "June",
      "July",
      "August",
      "September",
      ""
    ),
    limits = range(season_month_breaks),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Distribution of sample Julian day",
    subtitle = paste(
      "2023 and 2024 samples pooled by date within the season;",
      "seven-day bins"
    ),
    x = "Sampling date",
    y = "Number of sample reaches",
    fill = "Sample status",
    color = "Sample status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  guides(
    color = "none",
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

print(Sampling_date_culled_histogram)

# Optional export ------------------------------------------------------------
# ggsave(
#   filename = file.path(
#     folder_path,
#     "Sampling_date_culled_histogram.png"
#   ),
#   plot = Sampling_date_culled_histogram,
#   width = 10,
#   height = 6,
#   dpi = 300
# )
# Sampling-date boxplot -------------------------------------------------------
Sampling_date_culled_boxplot <- ggplot(
  sampling_date_plot_data,
  aes(
    x = stat_cat,
    y = season_day,
    color = stat_cat,
    fill = stat_cat
  )
) +
  geom_boxplot(
    alpha = 0.20,
    width = 0.65,
    outlier.shape = NA,
    na.rm = TRUE
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 2.2,
    alpha = 0.70,
    na.rm = TRUE
  ) +
  status_color_scale +
  status_fill_scale +
  scale_x_discrete(
    labels = status_labels
  ) +
  scale_y_continuous(
    breaks = season_month_breaks,
    labels = c(
      "June",
      "July",
      "August",
      "September",
      ""
    ),
    limits = range(season_month_breaks),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Sampling date by bull trout classification",
    subtitle = "2023 and 2024 samples pooled by date within the season",
    x = NULL,
    y = "Sampling date"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )

print(Sampling_date_culled_boxplot)
# ----------------------------------------------------------------------------

### UPSTREAM DRAINAGE AREA
# Pooled culled-sample plots --------------------------------------------------
reach_drainage_area <- BT_pixel_data |>
  group_by(sample_ID) |>
  summarise(
    upstream_da_sqkm = {
      valid_values <- upstream_da_sqkm[
        is.finite(upstream_da_sqkm) &
          upstream_da_sqkm >= 0
      ]
      
      if (length(valid_values) == 0) {
        NA_real_
      } else {
        mean(valid_values)
      }
    },
    .groups = "drop"
  )

BT_reach_field_culled <- BT_reach_field_culled |>
  select(-any_of("upstream_da_sqkm")) |>
  left_join(
    reach_drainage_area,
    by = "sample_ID"
  )

stopifnot(
  "upstream_da_sqkm" %in% names(BT_reach_field_culled)
)

summary(BT_reach_field_culled$upstream_da_sqkm)

drainage_plot_data_culled <- BT_reach_field_culled |>
  transmute(
    sample_ID,
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    ),
    upstream_da_sqkm
  ) |>
  filter(
    is.finite(upstream_da_sqkm),
    upstream_da_sqkm > 0,
    !is.na(stat_cat)
  )

# Boxplot ---------------------------------------------------------------------
Drainage_culled_boxplot <- ggplot(
  drainage_plot_data_culled,
  aes(
    x = stat_cat,
    y = upstream_da_sqkm,
    color = stat_cat,
    fill = stat_cat
  )
) +
  geom_boxplot(
    alpha = 0.20,
    width = 0.65,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.15,
    size = 2.2,
    alpha = 0.70
  ) +
  status_color_scale +
  status_fill_scale + 
  scale_x_discrete(
    labels = status_labels
  ) +
  scale_y_log10(labels = scales::label_comma()) +
  labs(
    title = "Upstream drainage area by bull trout classification",
    subtitle = "Logarithmic scale",
    x = NULL,
    y = expression("Upstream drainage area (km"^2*")")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold")
  )

print(Drainage_culled_boxplot)

# Histogram -------------------------------------------------------------------
Drainage_culled_histogram <- ggplot(
  drainage_plot_data_culled,
  aes(
    x = upstream_da_sqkm,
    fill = stat_cat
  )
) +
  geom_histogram(
    bins = 15,
    position = "stack",
    color = "white",
    linewidth = 0.25
  ) +
  geom_rug(
    aes(color = stat_cat),
    sides = "b",
    alpha = 0.55,
    linewidth = 0.45
  ) +
  scale_x_log10(labels = scales::label_comma()) +
  status_fill_scale +
  status_color_scale +
  labs(
    title = "Distribution of upstream drainage area",
    subtitle = "Logarithmic scale",
    x = expression("Upstream drainage area (km"^2*")"),
    y = "Number of sample reaches",
    fill = "Sample status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  guides(
    color = "none",
    fill = guide_legend(nrow = 2, byrow = TRUE)
  )

print(Drainage_culled_histogram)
# -----------------------------------------------------------------------------

### FIRE SEVERITY METRIC
# Culled-sample fire-severity data -------------------------------------------
stopifnot(
  exists("BT_reach_field_culled"),
  nrow(BT_reach_field_culled) > 0,
  all(c(
    "sample_ID",
    "stat_cat",
    "sample_year",
    "fire_n_pixels",
    "fire_n_burned_pixels",
    "fire_prop_burned",
    "fire_ever_burned",
    "fire_max_sev_num",
    "fire_mean_sev_num",
    "fire_most_recent_year",
    "fire_num_events"
  ) %in% names(BT_reach_field_culled))
)

cat(
  "Unique culled samples:",
  n_distinct(BT_reach_field_culled$sample_ID),
  "\n"
)

# Prepare fire severity plotting data ----------------------------------------
fire_metric_plot_data_culled <- BT_reach_field_culled |>
  transmute(
    sample_ID,
    sample_year,
    stat_cat,
    fire_n_pixels,
    fire_n_burned_pixels,
    fire_prop_burned,
    fire_ever_burned,
    fire_max_severity = fire_max_sev_num,
    fire_mean_severity = fire_mean_sev_num,
    fire_most_recent_year,
    fire_num_events
  ) |>
  mutate(
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    ),
    fire_status = factor(
      fire_ever_burned,
      levels = c(FALSE, TRUE),
      labels = c("No burned pixels", "At least one burned pixel")
    )
  )

# QA: fire coverage across the culled sample set ------------------------------
fire_metric_plot_data_culled |>
  summarise(
    n_samples = n(),
    n_missing_fire_max_severity = sum(is.na(fire_max_severity)),
    n_missing_fire_mean_severity = sum(is.na(fire_mean_severity)),
    n_burned_samples = sum(fire_ever_burned, na.rm = TRUE),
    n_unburned_samples = sum(!fire_ever_burned, na.rm = TRUE),
    mean_prop_burned = mean(fire_prop_burned, na.rm = TRUE),
    median_prop_burned = median(fire_prop_burned, na.rm = TRUE)
  ) |>
  print()

fire_metric_plot_data_culled |>
  count(
    sample_year,
    stat_cat,
    fire_status,
    .drop = FALSE
  ) |>
  print(n = Inf)

fire_metric_plot_data_culled |>
  count(
    fire_max_severity,
    stat_cat,
    .drop = FALSE
  ) |>
  print(n = Inf)

# Optional: burned-only dataset for severity-focused plots --------------------
fire_metric_burned_plot_data_culled <- fire_metric_plot_data_culled |>
  filter(
    fire_ever_burned,
    is.finite(fire_max_severity)
  )
cat(
  "Culled samples with at least one burned pixel:",
  nrow(fire_metric_burned_plot_data_culled),
  "\n"
)

## What do sample-level severity proportions look like? -----------------------
stopifnot(
  exists("BT_pixel_data"),
  exists("BT_reach_field_culled"),
  all(c("sample_ID", "pixel_ID", "sev_class", "sev_num") %in%
        names(BT_pixel_data)),
  "sample_ID" %in% names(BT_reach_field_culled)
)

severity_levels <- c("Unburned", "Low", "Moderate", "High")

# Raw severity coding check 
BT_pixel_data |>
  count(sev_class, sort = TRUE) |>
  print(n = Inf)

BT_pixel_data |>
  group_by(sev_class) |>
  summarise(
    n_pixels = n(),
    min_sev_num = min(sev_num, na.rm = TRUE),
    median_sev_num = median(sev_num, na.rm = TRUE),
    mean_sev_num = mean(sev_num, na.rm = TRUE),
    max_sev_num = max(sev_num, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(n = Inf)

# Pixel-level severity proportions within each sample 
sample_severity_proportions <- BT_pixel_data |>
  mutate(
    severity_class = case_when(
      is.na(sev_class) ~ "Unburned",
      sev_class %in% severity_levels ~ sev_class,
      TRUE ~ "Check coding"
    ),
    severity_class = factor(
      severity_class,
      levels = c(severity_levels, "Check coding")
    )
  ) |>
  count(sample_ID, severity_class, name = "n_pixels") |>
  complete(
    sample_ID,
    severity_class,
    fill = list(n_pixels = 0)
  ) |>
  group_by(sample_ID) |>
  mutate(
    total_pixels = sum(n_pixels),
    prop_pixels = n_pixels / total_pixels
  ) |>
  ungroup()

# Check for unexpected classes 
sample_severity_proportions |>
  filter(severity_class == "Check coding", n_pixels > 0) |>
  print(n = Inf)

# Wide sample-level table: one row per sample ---------------------------------
sample_severity_proportions_wide <- sample_severity_proportions |>
  filter(severity_class != "Check coding") |>
  select(sample_ID, severity_class, prop_pixels) |>
  pivot_wider(
    names_from = severity_class,
    values_from = prop_pixels,
    values_fill = 0,
    names_prefix = "prop_"
  ) |>
  semi_join(
    BT_reach_field_culled |> select(sample_ID),
    by = "sample_ID"
  ) |>
  left_join(
    BT_reach_field_culled |>
      select(sample_ID, stat_cat, sample_year) |>
      distinct(sample_ID, .keep_all = TRUE),
    by = "sample_ID"
  )

# Confirm expected columns exist 
stopifnot(
  all(c(
    "prop_Unburned",
    "prop_Low",
    "prop_Moderate",
    "prop_High"
  ) %in% names(sample_severity_proportions_wide))
)

# Overall distribution summaries ----------------------------------------------
sample_severity_proportions_wide |>
  summarise(
    n_samples = n(),
    mean_unburned = mean(prop_Unburned, na.rm = TRUE),
    median_unburned = median(prop_Unburned, na.rm = TRUE),
    mean_low = mean(prop_Low, na.rm = TRUE),
    median_low = median(prop_Low, na.rm = TRUE),
    mean_moderate = mean(prop_Moderate, na.rm = TRUE),
    median_moderate = median(prop_Moderate, na.rm = TRUE),
    mean_high = mean(prop_High, na.rm = TRUE),
    median_high = median(prop_High, na.rm = TRUE)
  ) |>
  print(width = Inf)

# Distribution summaries by sample status -------------------------------------
sample_severity_proportions_wide |>
  group_by(stat_cat) |>
  summarise(
    n_samples = n(),
    mean_unburned = mean(prop_Unburned, na.rm = TRUE),
    mean_low = mean(prop_Low, na.rm = TRUE),
    mean_moderate = mean(prop_Moderate, na.rm = TRUE),
    mean_high = mean(prop_High, na.rm = TRUE),
    median_unburned = median(prop_Unburned, na.rm = TRUE),
    median_low = median(prop_Low, na.rm = TRUE),
    median_moderate = median(prop_Moderate, na.rm = TRUE),
    median_high = median(prop_High, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(width = Inf)

# Long plot-ready table --------------------------------------------------------
sample_severity_proportions_plot <- sample_severity_proportions |>
  filter(severity_class %in% severity_levels) |>
  semi_join(
    BT_reach_field_culled |> select(sample_ID),
    by = "sample_ID"
  ) |>
  left_join(
    BT_reach_field_culled |>
      select(sample_ID, stat_cat, sample_year) |>
      distinct(sample_ID, .keep_all = TRUE),
    by = "sample_ID"
  ) |>
  mutate(
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    )
  )

# Quick count check 
sample_severity_proportions_plot |>
  count(severity_class, stat_cat, .drop = FALSE) |>
  print(n = Inf)

### Assigning severity class to each sample:
sample_dominant_severity <- sample_severity_proportions_wide |>
  rowwise() |>
  mutate(
    dominant_severity = severity_levels[
      which.max(c(
        prop_Unburned,
        prop_Low,
        prop_Moderate,
        prop_High
      ))
    ],
    dominant_prop = max(
      c(
        prop_Unburned,
        prop_Low,
        prop_Moderate,
        prop_High
      ),
      na.rm = TRUE
    )
  ) |>
  ungroup()

sample_dominant_severity |>
  count(stat_cat, dominant_severity) |>
  print(n = Inf)

sample_dominant_severity |>
  summarise(
    n_samples = n(),
    n_dominant_unburned = sum(dominant_severity == "Unburned"),
    n_dominant_low = sum(dominant_severity == "Low"),
    n_dominant_moderate = sum(dominant_severity == "Moderate"),
    n_dominant_high = sum(dominant_severity == "High"),
    mean_dominant_prop = mean(dominant_prop, na.rm = TRUE),
    median_dominant_prop = median(dominant_prop, na.rm = TRUE)
  )

### Visuals: sample-level severity proportions ---------------------------------

library(ggplot2)
library(scales)
library(dplyr)

severity_fill_colors <- c(
  "Unburned" = "#BDBDBD",
  "Low" = "#FEE08B",
  "Moderate" = "#F46D43",
  "High" = "#A50026"
)

## table... --------------------------
sample_severity_assignment_diagnostic <- sample_severity_proportions_wide |>
  mutate(
    prop_burned = prop_Low + prop_Moderate + prop_High,
    prop_moderate_high = prop_Moderate + prop_High,
    dominant_severity = case_when(
      prop_Unburned >= prop_Low &
        prop_Unburned >= prop_Moderate &
        prop_Unburned >= prop_High ~ "Unburned",
      prop_Low >= prop_Moderate &
        prop_Low >= prop_High ~ "Low",
      prop_Moderate >= prop_High ~ "Moderate",
      TRUE ~ "High"
    ),
    dominant_prop = pmax(
      prop_Unburned,
      prop_Low,
      prop_Moderate,
      prop_High,
      na.rm = TRUE
    )
  )

sample_severity_assignment_diagnostic |>
  summarise(
    n_samples = n(),
    mean_prop_burned = mean(prop_burned),
    median_prop_burned = median(prop_burned),
    n_any_burned = sum(prop_burned > 0),
    n_majority_burned = sum(prop_burned >= 0.5),
    n_any_high = sum(prop_High > 0),
    n_high_10pct = sum(prop_High >= 0.10),
    n_high_25pct = sum(prop_High >= 0.25),
    n_mod_high_25pct = sum(prop_moderate_high >= 0.25),
    n_mod_high_50pct = sum(prop_moderate_high >= 0.50),
    n_dominant_clear_50pct = sum(dominant_prop >= 0.50),
    n_dominant_unclear_lt_50pct = sum(dominant_prop < 0.50)
  ) |>
  print(width = Inf)

sample_severity_assignment_diagnostic |>
  count(stat_cat, dominant_severity) |>
  print(n = Inf)

sample_severity_assignment_diagnostic |>
  arrange(dominant_prop) |>
  select(
    sample_ID,
    stat_cat,
    sample_year,
    prop_Unburned,
    prop_Low,
    prop_Moderate,
    prop_High,
    prop_burned,
    prop_moderate_high,
    dominant_severity,
    dominant_prop
  ) |>
  print(n = 40, width = Inf)


# 1. Histograms: distribution of each severity proportion ----------------------
Severity_proportion_histograms <- ggplot(
  sample_severity_proportions_plot,
  aes(
    x = prop_pixels,
    fill = severity_class
  )
) +
  geom_histogram(
    binwidth = 0.05,
    boundary = 0,
    color = "white",
    linewidth = 0.25
  ) +
  facet_wrap(
    ~ severity_class,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_fill_manual(values = severity_fill_colors) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Distribution of fire severity proportions across samples",
    x = "Proportion of sample pixels",
    y = "Number of samples",
    fill = "Severity class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

print(Severity_proportion_histograms)

### Histogram: sample-level mean fire severity by bull trout classification ----
fire_status_colors <- c(
  "Present / Suitable Habitat" = "forestgreen",
  "Present / Unsuitable Habitat" = "gold",
  "Absent / Unsuitable Habitat" = "gray60",
  "Absent / Suitable Habitat" = "red3"
)

fire_status_labels <- names(fire_status_colors)

sample_status_lookup <- BT_reach_field_culled |>
  select(sample_ID, stat_cat) |>
  distinct(sample_ID, .keep_all = TRUE) |>
  mutate(
    sample_stat_cat_clean = tolower(trimws(as.character(stat_cat))),
    bull_trout_class = case_when(
      sample_stat_cat_clean == "present, correct" ~
        "Present / Suitable Habitat",
      
      sample_stat_cat_clean == "present, incorrect" ~
        "Present / Unsuitable Habitat",
      
      sample_stat_cat_clean == "absent, correct" ~
        "Absent / Unsuitable Habitat",
      
      sample_stat_cat_clean == "absent, incorrect" ~
        "Absent / Suitable Habitat",
      
      TRUE ~ NA_character_
    ),
    bull_trout_class = factor(
      bull_trout_class,
      levels = fire_status_labels
    )
  ) |>
  select(sample_ID, stat_cat, bull_trout_class)

sample_status_lookup |>
  count(stat_cat, bull_trout_class, sort = TRUE) |>
  print(n = Inf)

sample_mean_severity <- BT_pixel_data |>
  mutate(
    sev_num = as.numeric(sev_num)
  ) |>
  group_by(sample_ID) |>
  summarise(
    mean_sev_num = mean(sev_num, na.rm = TRUE),
    median_sev_num = median(sev_num, na.rm = TRUE),
    max_sev_num = max(sev_num, na.rm = TRUE),
    n_pixels = sum(!is.na(sev_num)),
    .groups = "drop"
  ) |>
  filter(
    n_pixels > 0,
    is.finite(mean_sev_num)
  ) |>
  semi_join(
    BT_reach_field_culled |> select(sample_ID),
    by = "sample_ID"
  ) |>
  left_join(
    sample_status_lookup,
    by = "sample_ID"
  ) |>
  filter(!is.na(bull_trout_class))

sample_mean_severity |>
  count(bull_trout_class) |>
  print(n = Inf)

Sample_mean_sev_num_histogram <- ggplot(
  sample_mean_severity,
  aes(
    x = mean_sev_num,
    fill = bull_trout_class
  )
) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 0,
    color = "white",
    linewidth = 0.25,
    position = "stack"
  ) +
  scale_fill_manual(
    values = fire_status_colors,
    drop = FALSE,
    guide = guide_legend(
      nrow = 2,
      byrow = TRUE,
      title.position = "left",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Distribution of sample-level mean fire severity",
    #subtitle = "Fire severity averaged across pixels assigned to each sample.",
    x = "Mean fire severity",
    y = "Number of samples",
    fill = "Bull trout DNA classification"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic")
  )

print(Sample_mean_sev_num_histogram)

# 2. Stacked bars: composition of each sample ---------------------------------

sample_order_by_high <- sample_severity_proportions_wide |>
  arrange(prop_High, prop_Moderate, prop_Low) |>
  pull(sample_ID)

Severity_proportion_stacked_bars <- sample_severity_proportions_plot |>
  mutate(
    sample_ID = factor(sample_ID, levels = sample_order_by_high)
  ) |>
  ggplot(
    aes(
      x = sample_ID,
      y = prop_pixels,
      fill = severity_class
    )
  ) +
  geom_col(width = 0.9) +
  scale_fill_manual(values = severity_fill_colors) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Fire severity composition by sample",
    x = "Sample reaches ordered by high-severity proportion",
    y = "Proportion of pixels",
    fill = "Severity class"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(Severity_proportion_stacked_bars)


# 3. Boxplots: severity proportions by bull trout status ----------------------

Severity_proportion_boxplots <- sample_severity_proportions_plot |>
  mutate(
    stat_cat = factor(
      stat_cat,
      levels = names(status_colors)
    )
  ) |>
  ggplot(
    aes(
      x = stat_cat,
      y = prop_pixels,
      color = stat_cat,
      fill = stat_cat
    )
  ) +
  geom_boxplot(
    alpha = 0.20,
    width = 0.65,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.16,
    height = 0,
    size = 1.9,
    alpha = 0.65
  ) +
  facet_wrap(
    ~ severity_class,
    ncol = 2
  ) +
  status_color_scale +
  status_fill_scale +
  scale_x_discrete(labels = status_labels) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Fire severity proportions by bull trout classification",
    x = NULL,
    y = "Proportion of sample pixels"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )

print(Severity_proportion_boxplots)
#### --------------------------------------------------------------------------


### FIRE SEVERITY PLOTS: 2040 SUITABLE HABITAT ONLY ---------------------------
# Step 1: Identify 2040 suitable-habitat samples with NO downstream barriers ----
barrier_col <- case_when(
  "no_of_ds_barriers" %in% names(culled_sample_points_map) ~ "no_of_ds_barriers",
  "no_ds_barriers" %in% names(culled_sample_points_map) ~ "no_ds_barriers",
  TRUE ~ NA_character_
)

stopifnot(!is.na(barrier_col))

suitable_2040_sample_ids_no_barriers <- culled_sample_points_map |>
  st_drop_geometry() |>
  distinct(sample_ID, stat_cat_2040, .data[[barrier_col]]) |>
  mutate(
    no_of_ds_barriers_map = as.numeric(.data[[barrier_col]])
  ) |>
  filter(
    stat_cat_2040 %in% c("Present, Correct", "Absent, Incorrect"),
    is.na(no_of_ds_barriers_map) | no_of_ds_barriers_map == 0
  ) |>
  pull(sample_ID)

suitable_2040_n_no_barriers <- length(suitable_2040_sample_ids_no_barriers)

cat(
  "2040 suitable-habitat samples with no downstream barriers:",
  suitable_2040_n_no_barriers,
  "\n"
)

# Diagnostic: what got removed?
culled_sample_points_map |>
  st_drop_geometry() |>
  distinct(sample_ID, stat_cat_2040, .data[[barrier_col]]) |>
  mutate(
    no_of_ds_barriers_map = as.numeric(.data[[barrier_col]]),
    in_2040_suitable_habitat = stat_cat_2040 %in% c("Present, Correct", "Absent, Incorrect"),
    removed_from_fire_plots = in_2040_suitable_habitat & no_of_ds_barriers_map > 0
  ) |>
  count(in_2040_suitable_habitat, removed_from_fire_plots, name = "n") |>
  print(n = Inf)

# Step 2: Build the sample-level fire severity dataset -------------------------
sample_status_lookup_2040_suitable_no_barriers <- culled_sample_points_map |>
  st_drop_geometry() |>
  distinct(sample_ID, stat_cat_2040, .data[[barrier_col]]) |>
  mutate(
    no_of_ds_barriers_map = as.numeric(.data[[barrier_col]])
  ) |>
  filter(sample_ID %in% suitable_2040_sample_ids_no_barriers) |>
  mutate(
    stat_cat = stat_cat_2040,
    field_status = case_when(
      stat_cat_2040 == "Present, Correct" ~ "Present",
      stat_cat_2040 == "Absent, Incorrect" ~ "Absent",
      TRUE ~ NA_character_
    ),
    bt_presence = case_when(
      field_status == "Present" ~ 1,
      field_status == "Absent" ~ 0,
      TRUE ~ NA_real_
    ),
    field_status = factor(field_status, levels = c("Present", "Absent"))
  ) |>
  select(sample_ID, stat_cat, field_status, bt_presence, no_of_ds_barriers_map)

sample_mean_severity_2040_suitable_no_barriers <- BT_pixel_data |>
  mutate(
    sev_num = as.numeric(sev_num)
  ) |>
  group_by(sample_ID) |>
  summarise(
    mean_sev_num = mean(sev_num, na.rm = TRUE),
    median_sev_num = median(sev_num, na.rm = TRUE),
    max_sev_num = max(sev_num, na.rm = TRUE),
    n_pixels = sum(!is.na(sev_num)),
    .groups = "drop"
  ) |>
  filter(
    sample_ID %in% suitable_2040_sample_ids_no_barriers,
    n_pixels > 0,
    is.finite(mean_sev_num)
  ) |>
  left_join(
    sample_status_lookup_2040_suitable_no_barriers,
    by = "sample_ID"
  ) |>
  filter(!is.na(field_status))

# Step 3: Quick diagnostic -----------------------------------------------------
sample_mean_severity_2040_suitable_no_barriers |>
  summarise(
    n_samples = n_distinct(sample_ID),
    n_present = sum(field_status == "Present"),
    n_absent = sum(field_status == "Absent"),
    n_with_downstream_barriers = sum(no_of_ds_barriers_map > 0, na.rm = TRUE),
    n_missing_mean_sev = sum(is.na(mean_sev_num)),
    min_mean_sev = min(mean_sev_num, na.rm = TRUE),
    median_mean_sev = median(mean_sev_num, na.rm = TRUE),
    mean_mean_sev = mean(mean_sev_num, na.rm = TRUE),
    max_mean_sev = max(mean_sev_num, na.rm = TRUE)
  ) |>
  print(width = Inf)

sample_mean_severity_2040_suitable_no_barriers |>
  count(field_status) |>
  print(n = Inf)

# Step 4: Recreate histogram and scatterplot -----------------------------------
Sample_mean_sev_num_histogram_2040_suitable_no_barriers <- ggplot(
  sample_mean_severity_2040_suitable_no_barriers,
  aes(
    x = mean_sev_num,
    fill = field_status
  )
) +
  geom_histogram(
    binwidth = 0.25,
    boundary = 0,
    color = "white",
    linewidth = 0.25,
    position = "stack"
  ) +
  scale_fill_manual(
    values = c(
      "Present" = "forestgreen",
      "Absent" = "gray55"
    ),
    drop = FALSE
  ) +
  labs(
    #title = "Distribution of sample-level mean fire severity",
    #subtitle = paste0(
    #  "2040 suitable habitat only; downstream-barrier samples excluded (n = ",
    #  n_distinct(sample_mean_severity_2040_suitable_no_barriers$sample_ID),
    #  ")"
    #),
    x = "Mean fire severity",
    y = "Number of samples",
    fill = "Field result"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(Sample_mean_sev_num_histogram_2040_suitable_no_barriers)

Mean_sev_num_presence_scatterplot_2040_suitable_no_barriers <- ggplot(
  sample_mean_severity_2040_suitable_no_barriers,
  aes(
    x = mean_sev_num,
    y = bt_presence,
    color = field_status
  )
) +
  geom_jitter(
    width = 0,
    height = 0.06,
    size = 2.6,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = c(
      "Present" = "forestgreen",
      "Absent" = "gray55"
    )
  ) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("Absent", "Present"),
    limits = c(-0.12, 1.12)
  ) +
  labs(
    #title = "Bull trout eDNA detection and sample-level mean fire severity",
    #subtitle = paste0(
    #  "2040 suitable habitat only; downstream-barrier samples excluded (n = ",
    #  n_distinct(sample_mean_severity_2040_suitable_no_barriers$sample_ID),
    #  ")"
    #),
    x = "Mean fire severity value per sample",
    y = "Bull trout detection",
    color = "Field result"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

print(Mean_sev_num_presence_scatterplot_2040_suitable_no_barriers)
# -----------------------------------------------------------------------------

### Scatterplot: 2040 suitable habitat only, fires from 2013 onward,
### moderate/high severity burns only -----------------------------------------

library(dplyr)
library(ggplot2)
library(sf)

barrier_col <- case_when(
  "no_of_ds_barriers" %in% names(culled_sample_points_map) ~ "no_of_ds_barriers",
  "no_ds_barriers" %in% names(culled_sample_points_map) ~ "no_ds_barriers",
  TRUE ~ NA_character_
)

stopifnot(!is.na(barrier_col))

sample_status_lookup_2040_suitable_no_barriers <- culled_sample_points_map |>
  st_drop_geometry() |>
  distinct(sample_ID, stat_cat_2040, .data[[barrier_col]]) |>
  mutate(
    no_of_ds_barriers_map = as.numeric(.data[[barrier_col]])
  ) |>
  filter(
    stat_cat_2040 %in% c("Present, Correct", "Absent, Incorrect"),
    is.na(no_of_ds_barriers_map) | no_of_ds_barriers_map == 0
  ) |>
  mutate(
    field_status = case_when(
      stat_cat_2040 == "Present, Correct" ~ "Present",
      stat_cat_2040 == "Absent, Incorrect" ~ "Absent",
      TRUE ~ NA_character_
    ),
    bt_presence = case_when(
      field_status == "Present" ~ 1,
      field_status == "Absent" ~ 0,
      TRUE ~ NA_real_
    ),
    field_status = factor(field_status, levels = c("Present", "Absent"))
  ) |>
  select(sample_ID, stat_cat_2040, field_status, bt_presence, no_of_ds_barriers_map)

sample_mean_severity_2013_mod_high <- BT_pixel_data |>
  mutate(
    ignition_year = as.numeric(ignition_year),
    sev_num = as.numeric(sev_num),
    severity_class = case_when(
      sev_num >= 1.25 & sev_num < 2.25 ~ "Moderate",
      sev_num >= 2.25 ~ "High",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    sample_ID %in% sample_status_lookup_2040_suitable_no_barriers$sample_ID,
    ignition_year >= 2013,
    severity_class %in% c("Moderate", "High")
  ) |>
  group_by(sample_ID) |>
  summarise(
    mean_sev_num_2013_mod_high = mean(sev_num, na.rm = TRUE),
    max_sev_num_2013_mod_high = max(sev_num, na.rm = TRUE),
    n_mod_high_fire_pixels_2013_on = n(),
    .groups = "drop"
  ) |>
  left_join(
    sample_status_lookup_2040_suitable_no_barriers,
    by = "sample_ID"
  ) |>
  filter(!is.na(field_status))

sample_mean_severity_2013_mod_high |>
  count(field_status) |>
  print(n = Inf)

Mean_sev_num_presence_scatterplot_2013_mod_high <- ggplot(
  sample_mean_severity_2013_mod_high,
  aes(
    x = mean_sev_num_2013_mod_high,
    y = bt_presence,
    color = field_status
  )
) +
  geom_jitter(
    width = 0.03,
    height = 0.06,
    size = 2.6,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = c(
      "Present" = "forestgreen",
      "Absent" = "gray55"
    )
  ) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("Absent", "Present"),
    limits = c(-0.12, 1.12)
  ) +
  labs(
    #title = "Bull trout eDNA detection and sample-level mean fire severity",
    #subtitle = paste0(
    #  "2040 suitable habitat only; downstream-barrier samples excluded (n = ",
    #  n_distinct(sample_mean_severity_2040_suitable_no_barriers$sample_ID),
    #  ")"
    #),
    x = "Mean fire severity value per sample",
    y = "Bull trout detection",
    color = "Field result"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

print(Mean_sev_num_presence_scatterplot_2013_mod_high)

### How many fire events? ----------------------------------------------------
fire_event_col <- "fire_event"

fire_pixels_2013_mod_high_in_scatterplot <- BT_pixel_data |>
  mutate(
    ignition_year = as.numeric(ignition_year),
    sev_num = as.numeric(sev_num),
    severity_class = case_when(
      sev_num >= 1.25 & sev_num < 2.25 ~ "Moderate",
      sev_num >= 2.25 ~ "High",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    sample_ID %in% sample_mean_severity_2013_mod_high$sample_ID,
    ignition_year >= 2013,
    severity_class %in% c("Moderate", "High")
  )

fire_pixels_2013_mod_high_in_scatterplot |>
  summarise(
    n_samples_in_scatterplot = n_distinct(sample_ID),
    n_fire_pixels = n(),
    n_individual_fire_events = n_distinct(.data[[fire_event_col]], na.rm = TRUE)
  ) |>
  print(width = Inf)
# -----------------------------------------------------------------------------


################################################################################
################################################################################
################################################################################
################################################################################

###  SCATTER PLOTS =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
### Using the below-barrier samples in 2040 suitable habitat, ONLY!!! 

## Restrict all downstream scatterplot data to 2040 suitable habitat only -------
suitable_2040_sample_ids <- suitable_2040_sample_ids_no_barriers
suitable_2040_n <- length(suitable_2040_sample_ids)

cat(
  "2040 suitable-habitat samples with no downstream barriers:",
  suitable_2040_n,
  "\n"
)

cwd_plot_data_culled <- cwd_plot_data_culled |>
  filter(sample_ID %in% suitable_2040_sample_ids)

swe_plot_data_culled <- swe_plot_data_culled |>
  filter(sample_ID %in% suitable_2040_sample_ids)

ppt_plot_data_culled <- ppt_plot_data_culled |>
  filter(sample_ID %in% suitable_2040_sample_ids)

tmean_plot_data_culled <- tmean_plot_data_culled |>
  filter(sample_ID %in% suitable_2040_sample_ids)

elevation_plot_data_culled <- elevation_plot_data_culled |>
  filter(sample_ID %in% suitable_2040_sample_ids)

sampling_date_plot_data <- sampling_date_plot_data |>
  filter(sample_ID %in% suitable_2040_sample_ids)

drainage_plot_data_culled <- drainage_plot_data_culled |>
  filter(sample_ID %in% suitable_2040_sample_ids)
# -----------------------------------------------------------------------------

# FUNCTION: stat-cat scatterplot-creation -------------------------------------
make_presence_scatter <- function(
    data,
    x_variable,
    plot_title,
    x_label,
    panel_variable = NULL,
    x_scale = NULL
) {
  
  plot_data <- data |>
    mutate(
      bt_presence = case_when(
        as.character(stat_cat) %in% c(
          "Present, Correct",
          "Present, Incorrect"
        ) ~ 1,
        
        as.character(stat_cat) %in% c(
          "Absent, Correct",
          "Absent, Incorrect"
        ) ~ 0,
        
        TRUE ~ NA_real_
      )
    ) |>
    filter(
      is.finite(.data[[x_variable]]),
      !is.na(bt_presence),
      !is.na(stat_cat)
    )
  
  stopifnot(nrow(plot_data) > 0)
  
  p <- ggplot(
    plot_data,
    aes(
      x = .data[[x_variable]],
      y = bt_presence,
      color = stat_cat
    )
  ) +
    geom_jitter(
      width = 0,
      height = 0.06,
      size = 2.5,
      alpha = 0.70,
      na.rm = TRUE
    ) +
    status_color_scale +
    scale_y_continuous(
      breaks = c(0, 1),
      labels = c("Absent", "Present"),
      limits = c(-0.12, 1.12)
    ) +
    labs(
      title = plot_title,
      subtitle = paste0(
        "Suitable 2040 Habitat Only (n = ",
        dplyr::n_distinct(plot_data$sample_ID),
        ")"
      ),
      x = x_label,
      y = "Observed eDNA result",
      color = "Sample status"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    ) +
    guides(
      color = guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = list(
          size = 3,
          alpha = 1
        )
      )
    )
  
  # Add four-panel layout when appropriate
  if (!is.null(panel_variable)) {
    p <- p +
      facet_wrap(
        stats::as.formula(
          paste("~", panel_variable)
        ),
        ncol = 2,
        scales = "free_x"
      ) +
      theme(
        panel.border = element_rect(
          color = "grey35",
          fill = NA,
          linewidth = 0.8
        ),
        panel.spacing = grid::unit(0, "pt")
      )
  }
  
  if (!is.null(x_scale)) {
    p <- p + x_scale
  }
  
  p
}
# -----------------------------------------------------------------------------

# Create all bull trout presence scatterplots --------------------------------
# Summer climatic water deficit
CWD_culled_scatterplots <- make_presence_scatter(
  data = cwd_plot_data_culled,
  x_variable = "cwd_zscore",
  plot_title = "Bull trout presence and summer CWD",
  x_label = "Summer CWD z-score",
  panel_variable = "panel"
)

# April snow-water equivalent
SWE_culled_scatterplots <- make_presence_scatter(
  data = swe_plot_data_culled,
  x_variable = "swe_zscore",
  plot_title = "Bull trout presence and April SWE",
  x_label = "April SWE z-score",
  panel_variable = "panel"
)

# Summer precipitation
PPT_culled_scatterplots <- make_presence_scatter(
  data = ppt_plot_data_culled,
  x_variable = "ppt_zscore",
  plot_title = "Bull trout presence and summer precipitation",
  x_label = "Summer precipitation z-score",
  panel_variable = "panel"
)

# Summer mean temperature
Tmean_culled_scatterplots <- make_presence_scatter(
  data = tmean_plot_data_culled,
  x_variable = "tmean_zscore",
  plot_title = "Bull trout presence and summer mean temperature",
  x_label = "Summer mean-temperature z-score",
  panel_variable = "panel"
)

# Sample-site elevation
Elevation_culled_scatterplot <- make_presence_scatter(
  data = elevation_plot_data_culled,
  x_variable = "elevation_m",
  plot_title = "Bull trout presence and sample-site elevation",
  x_label = "Elevation (m)",
  x_scale = scale_x_continuous(
    labels = scales::label_comma()
  )
)

# Sampling date pooled across years
Sampling_date_culled_scatterplot <- make_presence_scatter(
  data = sampling_date_plot_data,
  x_variable = "season_day",
  plot_title = "Bull trout presence and sampling date",
  x_label = "Sampling date",
  x_scale = scale_x_continuous(
    breaks = season_month_breaks,
    labels = c(
      "June",
      "July",
      "August",
      "September",
      ""
    ),
    limits = range(season_month_breaks),
    expand = expansion(mult = c(0, 0))
  )
)

# Upstream drainage area
Drainage_culled_scatterplot <- make_presence_scatter(
  data = drainage_plot_data_culled,
  x_variable = "upstream_da_sqkm",
  plot_title = "Bull trout presence and upstream drainage area",
  x_label = expression(
    "Upstream drainage area (km"^2*")"
  ),
  x_scale = scale_x_log10(
    labels = scales::label_comma()
  )
)

# Display plots ---------------------------------------------------------------

print(CWD_culled_scatterplots)
print(SWE_culled_scatterplots)
print(PPT_culled_scatterplots)
print(Tmean_culled_scatterplots)
print(Elevation_culled_scatterplot)
print(Sampling_date_culled_scatterplot)
print(Drainage_culled_scatterplot)

### Scatterplot: sample-level mean fire severity by eDNA presence/absence ------
sample_mean_severity_presence <- BT_pixel_data |>
  mutate(
    sev_num = as.numeric(sev_num)
  ) |>
  group_by(sample_ID) |>
  summarise(
    mean_sev_num = mean(sev_num, na.rm = TRUE),
    median_sev_num = median(sev_num, na.rm = TRUE),
    max_sev_num = max(sev_num, na.rm = TRUE),
    n_pixels = sum(!is.na(sev_num)),
    .groups = "drop"
  ) |>
  filter(
    sample_ID %in% suitable_2040_sample_ids,
    n_pixels > 0,
    is.finite(mean_sev_num)
  ) |>
  left_join(
    BT_reach_field_culled |>
      filter(sample_ID %in% suitable_2040_sample_ids) |>
      select(sample_ID, stat_cat) |>
      distinct(sample_ID, .keep_all = TRUE),
    by = "sample_ID"
  ) |>
  mutate(
    field_status = case_when(
      grepl("^Present", as.character(stat_cat)) ~ "Present",
      grepl("^Absent", as.character(stat_cat)) ~ "Absent",
      TRUE ~ NA_character_
    ),
    bt_presence = case_when(
      field_status == "Present" ~ 1,
      field_status == "Absent" ~ 0,
      TRUE ~ NA_real_
    ),
    field_status = factor(
      field_status,
      levels = c("Present", "Absent")
    )
  ) |>
  filter(!is.na(field_status))

sample_mean_severity_presence |>
  count(field_status) |>
  print(n = Inf)

Mean_sev_num_presence_scatterplot <- ggplot(
  sample_mean_severity_presence,
  aes(
    x = mean_sev_num,
    y = bt_presence,
    color = field_status
  )
) +
  geom_jitter(
    width = 0,
    height = 0.06,
    size = 2.6,
    alpha = 0.75
  ) +
  scale_color_manual(
    values = c(
      "Present" = "forestgreen",
      "Absent" = "gray55"
    )
  ) +
  scale_y_continuous(
    breaks = c(0, 1),
    labels = c("Absent", "Present"),
    limits = c(-0.12, 1.12)
  ) +
  labs(
    title = "Bull trout eDNA detection and sample-level mean fire severity",
    subtitle = paste0(
      "Suitable 2040 Habitat Only (n = ",
      dplyr::n_distinct(sample_mean_severity_presence$sample_ID),
      ")"
    ),
    x = "Mean fire severity value per sample",
    y = "Observed eDNA result",
    color = "Field result"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

print(Mean_sev_num_presence_scatterplot)
# End bull trout presence scatterplots ---------------------------------------






################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
######## ===== PRESENCE / ABSENCE CUTOFFS: ## Map culled bull trout eDNA sample reaches -----------------------------------
stopifnot(
  exists("BT_reach_field_culled"),
  exists("huc12_map_sf"),
  all(c("reach_longitude", "reach_latitude", "stat_cat") %in% names(BT_reach_field_culled))
)

## Restrict downstream scatterplot data to 2040 suitable habitat,
## excluding samples with downstream barriers -------------------------------
barrier_col <- case_when(
  "no_of_ds_barriers" %in% names(culled_sample_points_map) ~ "no_of_ds_barriers",
  "no_ds_barriers" %in% names(culled_sample_points_map) ~ "no_ds_barriers",
  TRUE ~ NA_character_
)

stopifnot(!is.na(barrier_col))

suitable_2040_sample_ids <- culled_sample_points_map |>
  st_drop_geometry() |>
  distinct(sample_ID, stat_cat_2040, .data[[barrier_col]]) |>
  mutate(
    no_of_ds_barriers_map = as.numeric(.data[[barrier_col]])
  ) |>
  filter(
    stat_cat_2040 %in% c("Present, Correct", "Absent, Incorrect"),
    is.na(no_of_ds_barriers_map) | no_of_ds_barriers_map == 0
  ) |>
  pull(sample_ID)

suitable_2040_n <- length(suitable_2040_sample_ids)

cat(
  "2040 suitable-habitat samples with no downstream barriers:",
  suitable_2040_n,
  "\n"
)

BT_reach_field_culled_2040_suitable <- BT_reach_field_culled |>
  filter(sample_ID %in% suitable_2040_sample_ids)
# ----------------------------------------------------------------------------

## Map out samples being included, colored by stat_cat -----------------------
BT_reach_field_culled_sf <- BT_reach_field_culled_2040_suitable |>
  filter(
    is.finite(reach_longitude),
    is.finite(reach_latitude)
  ) |>
  st_as_sf(
    coords = c("reach_longitude", "reach_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
  st_transform(st_crs(huc12_map_sf))

BT_reach_field_culled_map <- ggplot() +
  geom_sf(
    data = huc12_map_sf,
    fill = "#F2F2F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = BT_reach_field_culled_sf,
    aes(color = stat_cat),
    size = 2.4,
    alpha = 0.85
  ) +
  geom_sf(
    data = huc12_map_sf,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  scale_color_manual(
    values = status_colors,
    labels = status_labels,
    drop = FALSE,
    name = "Sample status"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Culled bull trout eDNA sample reaches",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(BT_reach_field_culled_map)
# ---------------------------------------------------------------------------

### ELEVATION + UPSTREAM DRAINAGE AREA CUTOFFS #################################
################################################################################
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# Convert stat_cat to presence/absence only
make_pa <- function(x) {
  case_when(
    str_detect(as.character(x), "^Present") ~ "Present",
    str_detect(as.character(x), "^Absent")  ~ "Absent",
    TRUE ~ NA_character_
  )
}

pa_colors <- c(
  "Absent" = "grey70",
  "Present" = "darkgreen"
)

################################################################################
######## ===== CALCULATE ELEVATION CUTOFF ================================================
elevation_pa <- elevation_plot_data_culled %>%
  filter(sample_ID %in% suitable_2040_sample_ids) %>%
  mutate(
    bt_pa = make_pa(stat_cat),
    bt_pa = factor(bt_pa, levels = c("Absent", "Present"))
  ) %>%
  filter(
    !is.na(bt_pa),
    is.finite(elevation_m)
  )

# Highest elevation where bull trout were detected
elev_cutoff <- elevation_pa %>%
  filter(bt_pa == "Present") %>%
  summarise(max_present_elevation_m = max(elevation_m, na.rm = TRUE)) %>%
  pull(max_present_elevation_m)

# QA: confirm no detections above cutoff
elevation_cutoff_check <- elevation_pa %>%
  summarise(
    max_present_elevation_m = elev_cutoff,
    n_samples_above_cutoff = sum(elevation_m > elev_cutoff),
    n_present_above_cutoff = sum(bt_pa == "Present" & elevation_m > elev_cutoff),
    n_absent_above_cutoff = sum(bt_pa == "Absent" & elevation_m > elev_cutoff)
  )

print(elevation_cutoff_check)

Elevation_PA_cutoff_plot <- ggplot(
  elevation_pa,
  aes(
    x = elevation_m,
    y = bt_pa,
    color = bt_pa
  )
) +
  geom_jitter(
    height = 0.10,
    width = 0,
    size = 2.6,
    alpha = 0.75
  ) +
  geom_vline(
    xintercept = elev_cutoff,
    linetype = "dashed",
    linewidth = 0.8,
    color = "red3"
  ) +
  annotate(
    "text",
    x = elev_cutoff,
    y = 1.55,
    label = NULL,
    color = "red3",
    angle = 90,
    vjust = -0.5,
    size = 3.5
  ) +
  scale_color_manual(values = pa_colors) +
  scale_x_continuous(labels = label_comma()) +
  labs(
    #title = "Bull trout DNA presence/absence by elevation",
    #subtitle = "Dashed red line marks highest elevation with bull trout DNA detected; n = 66",
    x = "Elevation (m)",
    y = "Bull trout detection",
    color = "eDNA result"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

print(Elevation_PA_cutoff_plot)

################################################################################
######## ===== CALCULATE UPSTREAM DRAINAGE AREA CUTOFF ===================================
drainage_pa <- drainage_plot_data_culled %>%
  filter(sample_ID %in% suitable_2040_sample_ids) %>%
  mutate(
    bt_pa = make_pa(stat_cat),
    bt_pa = factor(bt_pa, levels = c("Absent", "Present"))
  ) %>%
  filter(
    !is.na(bt_pa),
    is.finite(upstream_da_sqkm),
    upstream_da_sqkm > 0
  )

# Smallest upstream drainage area where bull trout were detected
da_cutoff <- drainage_pa %>%
  filter(bt_pa == "Present") %>%
  summarise(min_present_upstream_da_sqkm = min(upstream_da_sqkm, na.rm = TRUE)) %>%
  pull(min_present_upstream_da_sqkm)

# QA: confirm no detections above cutoff
drainage_cutoff_check <- drainage_pa %>%
  summarise(
    max_present_upstream_da_sqkm = da_cutoff,
    n_samples_above_cutoff = sum(upstream_da_sqkm > da_cutoff),
    n_present_above_cutoff = sum(bt_pa == "Present" & upstream_da_sqkm > da_cutoff),
    n_absent_above_cutoff = sum(bt_pa == "Absent" & upstream_da_sqkm > da_cutoff)
  )

print(drainage_cutoff_check)

Drainage_PA_cutoff_plot <- ggplot(
  drainage_pa,
  aes(
    x = upstream_da_sqkm,
    y = bt_pa,
    color = bt_pa
  )
) +
  geom_jitter(
    height = 0.10,
    width = 0,
    size = 2.6,
    alpha = 0.75
  ) +
  geom_vline(
    xintercept = da_cutoff,
    linetype = "dashed",
    linewidth = 0.8,
    color = "red3"
  ) +
  annotate(
    "text",
    x = da_cutoff,
    y = 1.55,
    label = NULL,
    color = "red3",
    angle = 90,
    vjust = -0.5,
    size = 3.5
  ) +
  scale_color_manual(values = pa_colors) +
  scale_x_log10(labels = label_comma()) +
  labs(
    #title = "Bull Trout DNA presence/absence by upstream drainage area",
    #subtitle = "Dashed line marks smallest upstream drainage area with a detection; n = 66",
    x = expression("Upstream drainage area (km"^2*")"),
    y = "Bull trout detection",
    color = "eDNA result"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

print(Drainage_PA_cutoff_plot)
################################################################################



################################################################################
##################       BULL TROUT MODEL SHAPEFILES     #######################
################################################################################

# 2022 model shapefiles --------------------------------------------------------

bt_model_2023_path <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/",
  "ArcGIS/BMWC_Fire_Data/bull_trout_model/Revised_2023"
)

bt_baseline_path <- paste0(
  bt_model_2023_path,
  "BullTroutPatchScenarios_BaselineClimate_Isaak_et_al_2022_AppendixS6.shp"
)

mod23_path <- file.path(
  bt_model_2023_path,
  "BullTroutPatchScenarios_ModerateClimate_Isaak_et_al_2022_AppendixS6.shp"
)

ext23_path <- file.path(
  bt_model_2023_path,
  "BullTroutPatchScenarios_ExtremeClimate_Isaak_et_al_2022_AppendixS6.shp"
)

# 2015 model shapefiles --------------------------------------------------------

base17_path <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/",
  "ArcGIS/BMWC_Fire_Data/bull_trout_model/2017_model/",
  "ClimateShield_1980/B0BK01980_SpoKoot.shp" 
)

mod17_path <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/",
  "ArcGIS/BMWC_Fire_Data/bull_trout_model/2017_model/",
  "ClimateShield_2040/B0BK02040_SpoKoot.shp"
)

ext17_path <- paste0(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project/",
  "ArcGIS/BMWC_Fire_Data/bull_trout_model/2017_model/",
  "ClimateShield_2080/B0BK02080_SpoKoot.shp"
)

stopifnot(
  file.exists(mod23_path),
  file.exists(ext23_path),
  file.exists(base17_path),
  file.exists(mod17_path),
  file.exists(ext17_path)
)

# Read in model layers ------------------------------------------------------------

mod23 <- st_read(mod23_path, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_make_valid() |>
  mutate(model_year = 2023, climate_scenario = "Moderate")

ext23 <- st_read(ext23_path, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_make_valid() |>
  mutate(model_year = 2023, climate_scenario = "Extreme")

base17 <- st_read(base17_path, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_make_valid() |>
  mutate(model_year = 2017, climate_scenario = "Baseline")

mod17 <- st_read(mod17_path, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_make_valid() |>
  mutate(model_year = 2017, climate_scenario = "Moderate")

ext17 <- st_read(ext17_path, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_make_valid() |>
  mutate(model_year = 2017, climate_scenario = "Extreme")

# QA --------------------------------------------------------------------------
model_layers <- list(
  mod23 = mod23,
  ext23 = ext23,
  base17 = base17,
  mod17 = mod17,
  ext17 = ext17
)

model_summary <- bind_rows(
  lapply(
    names(model_layers),
    function(layer_name) {
      model_sf <- model_layers[[layer_name]]
      
      geometry_types <- paste(
        unique(as.character(sf::st_geometry_type(model_sf))),
        collapse = ", "
      )
      
      model_crs <- sf::st_crs(model_sf)$input
      
      tibble(
        layer_name = layer_name,
        rows = nrow(model_sf),
        geometry_types = geometry_types,
        crs = model_crs
      )
    }
  )
)

print(model_summary, n = Inf)
## ----------------------------------------------------------------------------

## Simple map: 2022 model, 2040 projection occurrence probability --------------
library(sf)
library(dplyr)
library(ggplot2)

stopifnot(
  exists("mod23"),
  inherits(mod23, "sf"),
  "S9" %in% names(mod23),
  exists("huc12_map_sf"),
  inherits(huc12_map_sf, "sf")
)

analysis_crs <- 5070  # NAD83 / Conus Albers, meters

huc12_bob_simple <- huc12_map_sf |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(analysis_crs) |>
  st_make_valid()

huc12_bob_simple_sf <- st_sf(
  geometry = st_sfc(
    huc12_bob_simple |>
      st_union() |>
      st_make_valid(),
    crs = analysis_crs
  )
)

mod23_s9_bob <- mod23 |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(analysis_crs) |>
  st_make_valid() |>
  st_intersection(huc12_bob_simple_sf) |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    S9 = as.numeric(S9)
  )

Mod23_S9_probability_map <- ggplot() +
  geom_sf(
    data = huc12_bob_simple,
    fill = "#F2F2F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = mod23_s9_bob,
    aes(color = S9),
    linewidth = 1.25,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob_simple,
    fill = NA,
    color = "#525252",
    linewidth = 0.35
  ) +
  scale_color_gradient(
    low = "#FDD49E",
    high = "#E6550D",
    limits = c(0, 1),
    name = "Occurrence\nprobability"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "2023 bull trout model: 2040 projection",
    subtitle = "Patches colored by S9 occurrence probability; darker lines indicate higher predicted occurrence.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "#D9D9D9", linewidth = 0.20),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic")
  )

print(Mod23_S9_probability_map)

## Histogram: distribution of S9 occurrence probabilities ----------------------
S9_probability_histogram <- ggplot(
  mod23_s9_bob,
  aes(x = S9)
) +
  geom_histogram(
    binwidth = 0.05,
    boundary = 0,
    fill = "orange",
    color = "white",
    linewidth = 0.25
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  labs(
    title = "Distribution of S9 occurrence probabilities",
    subtitle = "2023 bull trout model, 2040 projection within the study area.",
    x = "S9 occurrence probability",
    y = "Number of patches"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

print(S9_probability_histogram)
# ----------------------------------------------------------------------------

## Simple map: 2017 model, 2040 projection occurrence probability --------------
stopifnot(
  exists("mod17"),
  inherits(mod17, "sf"),
  "BT_0BRK" %in% names(mod17),
  exists("huc12_map_sf"),
  inherits(huc12_map_sf, "sf")
)

analysis_crs <- 5070  # NAD83 / Conus Albers, meters

huc12_bob_simple <- huc12_map_sf |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(analysis_crs) |>
  st_make_valid()

huc12_bob_simple_sf <- st_sf(
  geometry = st_sfc(
    huc12_bob_simple |>
      st_union() |>
      st_make_valid(),
    crs = analysis_crs
  )
)

mod17_bt0brk_bob <- mod17 |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(analysis_crs) |>
  st_make_valid() |>
  st_intersection(huc12_bob_simple_sf) |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    BT_0BRK = as.numeric(BT_0BRK)
  )

Mod17_BT0BRK_probability_map <- ggplot() +
  geom_sf(
    data = huc12_bob_simple,
    fill = "#F2F2F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = mod17_bt0brk_bob,
    aes(color = BT_0BRK),
    linewidth = 1.25,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob_simple,
    fill = NA,
    color = "#525252",
    linewidth = 0.35
  ) +
  scale_color_gradient(
    low = "#FDD49E",
    high = "#E6550D",
    limits = c(0, 1),
    name = "Occurrence\nprobability"
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "2017 bull trout model: 2040 projection",
    subtitle = "Patches colored by BT_0BRK occurrence probability; darker lines indicate higher predicted occurrence.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "#D9D9D9", linewidth = 0.20),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic")
  )

print(Mod17_BT0BRK_probability_map)

## Histogram: distribution of BT_0BRK occurrence probabilities -----------------
BT0BRK_probability_histogram <- ggplot(
  mod17_bt0brk_bob,
  aes(x = BT_0BRK)
) +
  geom_histogram(
    binwidth = 0.05,
    boundary = 0,
    fill = "orange",
    color = "white",
    linewidth = 0.25
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  labs(
    title = "Distribution of BT_0BRK occurrence probabilities",
    subtitle = "2017 bull trout model, 2040 projection within the study area.",
    x = "BT_0BRK occurrence probability",
    y = "Number of patches"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

print(BT0BRK_probability_histogram)
### ----------------------------------------------------------------------------



################################################################################
##################   MODEL PATCH OVERLAP WITH CUTOFF STREAMS   ################
################################################################################

## Load libraries ------------
library(sf)
library(dplyr)
library(units)
library(tibble)
library(ggplot2)
# -----------------------------

################################################################################
######## ===== BUILD 3 STREAM CUTOFF LAYERS ====================================

stopifnot(
  exists("streams_map"),
  exists("huc12_map_sf"),
  exists("map_crs"),
  exists("dem10m"),
  exists("da_cutoff")
)

if (!exists("dem_crs") || is.na(dem_crs)) {
  dem_crs <- sf::st_crs(terra::crs(dem10m))
}
stopifnot(!is.na(dem_crs))

stream_elevation_cutoff_m <- if (exists("elev_cutoff")) elev_cutoff else 1691

stream_elevation_labels <- c(
  paste0("At or below ", round(stream_elevation_cutoff_m, 0), " m"),
  paste0("Above ", round(stream_elevation_cutoff_m, 0), " m")
)

stream_sampling_crs <- dem_crs

if (sf::st_is_longlat(stream_sampling_crs)) {
  stream_sampling_crs <- if (!sf::st_is_longlat(map_crs)) {
    map_crs
  } else {
    sf::st_crs(5070)
  }
}

stopifnot(!sf::st_is_longlat(stream_sampling_crs))

stream_sample_spacing_m <- 10

streams_for_elevation <- streams_map |>
  st_transform(stream_sampling_crs) |>
  st_cast("LINESTRING", warn = FALSE) |>
  mutate(
    stream_row_id = row_number(),
    stream_length_m = as.numeric(st_length(geometry))
  ) |>
  filter(stream_length_m > 0)

make_stream_sample_points <- function(stream_feature, spacing_m) {
  line_length <- stream_feature$stream_length_m[1]
  
  sample_fractions <- unique(
    c(
      0,
      seq(0, 1, by = spacing_m / line_length),
      1
    )
  )
  
  sampled_points <- st_line_sample(
    st_geometry(stream_feature),
    sample = sample_fractions
  ) |>
    st_cast("POINT")
  
  sampled_coordinates <- st_coordinates(sampled_points)
  
  tibble(
    stream_row_id = stream_feature$stream_row_id[1],
    stream_point_id = seq_len(nrow(sampled_coordinates)),
    stream_distance_m = sample_fractions * line_length,
    x = sampled_coordinates[, "X"],
    y = sampled_coordinates[, "Y"]
  )
}

stream_elevation_points <- bind_rows(
  lapply(seq_len(nrow(streams_for_elevation)), function(i) {
    make_stream_sample_points(
      streams_for_elevation[i, ],
      stream_sample_spacing_m
    )
  })
)

stream_elevation_point_sf <- st_as_sf(
  stream_elevation_points,
  coords = c("x", "y"),
  crs = stream_sampling_crs,
  remove = FALSE
)

stream_elevation_point_extract_sf <- stream_elevation_point_sf |>
  st_transform(dem_crs)

stream_elevation_values <- terra::extract(
  dem10m,
  terra::vect(stream_elevation_point_extract_sf)
)

stream_elevation_value_column <- setdiff(
  names(stream_elevation_values),
  "ID"
)[1]

stream_elevation_points <- stream_elevation_points |>
  mutate(
    stream_elevation_m = as.numeric(
      stream_elevation_values[[stream_elevation_value_column]]
    )
  )

make_cutoff_segments <- function(points, cutoff_m, labels, crs) {
  points <- points |>
    arrange(stream_distance_m, stream_point_id)
  
  if (nrow(points) < 2) return(NULL)
  
  output <- vector("list", nrow(points) * 2)
  output_index <- 0L
  
  add_segment <- function(x1, y1, x2, y2, class_label) {
    if (isTRUE(all.equal(c(x1, y1), c(x2, y2)))) return()
    
    output_index <<- output_index + 1L
    output[[output_index]] <<- list(
      stream_row_id = points$stream_row_id[1],
      elevation_cutoff_class = class_label,
      geometry = st_linestring(
        matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE)
      )
    )
  }
  
  for (i in seq_len(nrow(points) - 1L)) {
    p1 <- points[i, ]
    p2 <- points[i + 1L, ]
    
    if (!is.finite(p1$stream_elevation_m) ||
        !is.finite(p2$stream_elevation_m)) {
      next
    }
    
    if (p1$stream_elevation_m <= cutoff_m &&
        p2$stream_elevation_m <= cutoff_m) {
      add_segment(p1$x, p1$y, p2$x, p2$y, labels[1])
    } else if (p1$stream_elevation_m > cutoff_m &&
               p2$stream_elevation_m > cutoff_m) {
      add_segment(p1$x, p1$y, p2$x, p2$y, labels[2])
    } else {
      cutoff_fraction <- (cutoff_m - p1$stream_elevation_m) /
        (p2$stream_elevation_m - p1$stream_elevation_m)
      
      cutoff_x <- p1$x + cutoff_fraction * (p2$x - p1$x)
      cutoff_y <- p1$y + cutoff_fraction * (p2$y - p1$y)
      
      first_label <- if (p1$stream_elevation_m <= cutoff_m) labels[1] else labels[2]
      second_label <- if (p2$stream_elevation_m <= cutoff_m) labels[1] else labels[2]
      
      add_segment(p1$x, p1$y, cutoff_x, cutoff_y, first_label)
      add_segment(cutoff_x, cutoff_y, p2$x, p2$y, second_label)
    }
  }
  
  output <- output[seq_len(output_index)]
  if (length(output) == 0) return(NULL)
  
  st_sf(
    stream_row_id = vapply(output, `[[`, numeric(1), "stream_row_id"),
    elevation_cutoff_class = factor(
      vapply(output, `[[`, character(1), "elevation_cutoff_class"),
      levels = labels
    ),
    geometry = st_sfc(lapply(output, `[[`, "geometry"), crs = crs)
  )
}

stream_segment_groups <- split(
  stream_elevation_points,
  stream_elevation_points$stream_row_id,
  drop = TRUE
)

stream_segment_list <- Filter(
  Negate(is.null),
  lapply(
    stream_segment_groups,
    make_cutoff_segments,
    cutoff_m = stream_elevation_cutoff_m,
    labels = stream_elevation_labels,
    crs = stream_sampling_crs
  )
)

stopifnot(length(stream_segment_list) > 0)

streams_elevation_segments <- do.call(rbind, stream_segment_list) |>
  st_transform(map_crs)

streams_elevation_highlight <- streams_elevation_segments |>
  filter(elevation_cutoff_class == stream_elevation_labels[1])

stream_drainage_cutoff_sqkm <- da_cutoff

stream_drainage_labels <- c(
  paste0("At or above ", signif(stream_drainage_cutoff_sqkm, 3), " km2"),
  paste0("Below ", signif(stream_drainage_cutoff_sqkm, 3), " km2")
)

stream_drainage_column <- intersect(
  c("TotDASqKM", "upstream_da_sqkm", "TOTDASQKM", "TotDaSqKm"),
  names(streams_map)
)[1]

stopifnot(!is.na(stream_drainage_column))

streams_drainage_map <- streams_map |>
  mutate(
    stream_upstream_da_sqkm = as.numeric(.data[[stream_drainage_column]]),
    drainage_cutoff_class = if_else(
      stream_upstream_da_sqkm >= stream_drainage_cutoff_sqkm,
      stream_drainage_labels[1],
      stream_drainage_labels[2]
    ),
    drainage_cutoff_class = factor(
      drainage_cutoff_class,
      levels = stream_drainage_labels
    )
  ) |>
  filter(is.finite(stream_upstream_da_sqkm))

streams_drainage_highlight <- streams_drainage_map |>
  filter(drainage_cutoff_class == stream_drainage_labels[1])

stream_both_cutoff_labels <- c(
  "Meets both cutoffs",
  "Does not meet both cutoffs"
)

stream_both_drainage_column <- intersect(
  c("TotDASqKM", "upstream_da_sqkm", "TOTDASQKM", "TotDaSqKm"),
  names(streams_for_elevation)
)[1]

stopifnot(!is.na(stream_both_drainage_column))

stream_drainage_by_row <- streams_for_elevation |>
  st_drop_geometry() |>
  transmute(
    stream_row_id,
    stream_upstream_da_sqkm = as.numeric(.data[[stream_both_drainage_column]])
  )

streams_both_cutoffs_map <- streams_elevation_segments |>
  left_join(stream_drainage_by_row, by = "stream_row_id") |>
  mutate(
    both_cutoffs_class = if_else(
      elevation_cutoff_class == stream_elevation_labels[1] &
        stream_upstream_da_sqkm >= da_cutoff,
      stream_both_cutoff_labels[1],
      stream_both_cutoff_labels[2]
    ),
    both_cutoffs_class = factor(
      both_cutoffs_class,
      levels = stream_both_cutoff_labels
    )
  ) |>
  filter(is.finite(stream_upstream_da_sqkm))

streams_both_highlight <- streams_both_cutoffs_map |>
  filter(both_cutoffs_class == stream_both_cutoff_labels[1])

cat("Created stream layers:\n")
cat("streams_elevation_highlight:", nrow(streams_elevation_highlight), "segments\n")
cat("streams_drainage_highlight:", nrow(streams_drainage_highlight), "flowlines\n")
cat("streams_both_highlight:", nrow(streams_both_highlight), "segments\n")
# -----------------------------------------------------------------------------

## checks ------------
stopifnot(
  exists("huc12_map_sf"),
  exists("streams_both_highlight"),
  exists("streams_map"),
  exists("mod23"),
  exists("ext23"),
  exists("base17"),
  exists("mod17"),
  exists("ext17"),
  inherits(huc12_map_sf, "sf"),
  inherits(streams_both_highlight, "sf"),
  inherits(streams_map, "sf"),
  inherits(mod23, "sf"),
  inherits(ext23, "sf"),
  inherits(base17, "sf"),
  inherits(mod17, "sf"),
  inherits(ext17, "sf"),
  nrow(streams_both_highlight) > 0
)
# -------------------------------------

## Align CRS and overlay cutoff layer onto original patches -------------------
stopifnot(
  exists("huc12_map_sf"),
  inherits(huc12_map_sf, "sf"),
  inherits(streams_both_highlight, "sf"),
  inherits(mod23, "sf"),
  inherits(ext23, "sf"),
  inherits(base17, "sf"),
  inherits(mod17, "sf"),
  inherits(ext17, "sf")
)

# Use projected CRS in meters for clipping, buffering, and lengths
analysis_crs <- 5070  # NAD83 / Conus Albers, meters

huc12_bob <- huc12_map_sf |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(analysis_crs) |>
  st_make_valid()

huc12_bob_union <- huc12_bob |>
  st_union() |>
  st_make_valid()

huc12_bob_sf <- st_sf(
  geometry = st_sfc(huc12_bob_union, crs = analysis_crs)
)

cutoff_streams_analysis <- streams_both_highlight |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(analysis_crs) |>
  st_make_valid()

cutoff_union_analysis <- st_union(cutoff_streams_analysis)
cutoff_buffer_10m_analysis <- st_buffer(cutoff_union_analysis, dist = 10)
# -----------------------------------------------------------------------------

## Exclude habitat above 2 extra barriers -------------------------------------
additional_barrier_watersheds <- c(
  "Bunker Creek-South Fork Flathead River",
  "Upper Youngs Creek"
)

find_huc12_name_column <- function(huc_sf, target_names) {
  target_names_clean <- tolower(trimws(target_names))
  attribute_cols <- names(st_drop_geometry(huc_sf))
  
  matched_cols <- vapply(
    attribute_cols,
    function(col) {
      values <- tolower(trimws(as.character(huc_sf[[col]])))
      any(values %in% target_names_clean, na.rm = TRUE)
    },
    logical(1)
  )
  
  if (!any(matched_cols)) {
    stop(
      "Could not find the HUC12 name column containing the barrier watershed names. ",
      "Check names(huc12_bob)."
    )
  }
  
  attribute_cols[which(matched_cols)[1]]
}

barrier_huc12_name_col <- find_huc12_name_column(
  huc12_bob,
  additional_barrier_watersheds
)

additional_barrier_huc12 <- huc12_bob |>
  filter(
    tolower(trimws(as.character(.data[[barrier_huc12_name_col]]))) %in%
      tolower(trimws(additional_barrier_watersheds))
  ) |>
  st_make_valid()

stopifnot(nrow(additional_barrier_huc12) == length(additional_barrier_watersheds))

additional_barrier_union <- additional_barrier_huc12 |>
  st_union() |>
  st_make_valid()

cutoff_union_barrier_adjusted_analysis <- suppressWarnings(
  st_difference(cutoff_union_analysis, additional_barrier_union)
) |>
  st_make_valid()

cutoff_buffer_10m_barrier_adjusted_analysis <- suppressWarnings(
  st_difference(cutoff_buffer_10m_analysis, additional_barrier_union)
) |>
  st_make_valid()
## ----------------------------------------------------------------------------

## Clip layers to the Bob -----------------------------------------------------
clip_model_to_bob <- function(model_sf) {
  model_proj <- model_sf |>
    st_zm(drop = TRUE, what = "ZM") |>
    st_transform(analysis_crs) |>
    st_make_valid()
  
  clipped <- suppressWarnings(
    st_intersection(model_proj, huc12_bob_sf)
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    st_make_valid()
  
  clipped
}

model_layers_bob <- list(
  mod23 = clip_model_to_bob(mod23),
  ext23 = clip_model_to_bob(ext23),
  base17 = clip_model_to_bob(base17),
  mod17 = clip_model_to_bob(mod17),
  ext17 = clip_model_to_bob(ext17)
)

model_bob_summary <- bind_rows(
  lapply(names(model_layers_bob), function(layer_name) {
    x <- model_layers_bob[[layer_name]]
    
    tibble(
      layer_name = layer_name,
      n_features = nrow(x),
      total_length_km = sum(as.numeric(st_length(x)), na.rm = TRUE) / 1000,
      bbox = paste(round(st_bbox(x), 1), collapse = ", ")
    )
  })
)

print(model_bob_summary, width = Inf)

stopifnot(
  all(st_crs(model_layers_bob$mod23) == st_crs(huc12_bob)),
  all(st_crs(cutoff_streams_analysis) == st_crs(huc12_bob))
)

st_bbox(huc12_bob)
st_bbox(model_layers_bob$mod23)
# -----------------------------------------------------------------------------

# Exact vs buffered overlap diagnostic, Bob-only and analysis CRS --------------
diagnose_model_overlap <- function(model_sf, layer_name) {
  model_sf <- model_sf |>
    mutate(
      patch_row_id = row_number(),
      original_length_m = as.numeric(st_length(geometry))
    )
  
  exact_overlap <- suppressWarnings(
    st_intersection(
      model_sf,
      st_sf(
        geometry = st_sfc(cutoff_union_barrier_adjusted_analysis, crs = analysis_crs)
      )
    )
  ) |>
    mutate(overlap_length_m = as.numeric(st_length(geometry))) |>
    st_drop_geometry() |>
    group_by(patch_row_id) |>
    summarise(
      exact_overlap_length_m = sum(overlap_length_m, na.rm = TRUE),
      .groups = "drop"
    )
  
  buffered_overlap <- suppressWarnings(
    st_intersection(
      model_sf,
      st_sf(
        geometry = st_sfc(cutoff_buffer_10m_barrier_adjusted_analysis, crs = analysis_crs)
      )
    )
  ) |>
    mutate(overlap_length_m = as.numeric(st_length(geometry))) |>
    st_drop_geometry() |>
    group_by(patch_row_id) |>
    summarise(
      buffered10_overlap_length_m = sum(overlap_length_m, na.rm = TRUE),
      .groups = "drop"
    )
  
  model_sf |>
    st_drop_geometry() |>
    select(patch_row_id, original_length_m, everything()) |>
    left_join(exact_overlap, by = "patch_row_id") |>
    left_join(buffered_overlap, by = "patch_row_id") |>
    mutate(
      layer_name = layer_name,
      exact_overlap_length_m = coalesce(exact_overlap_length_m, 0),
      buffered10_overlap_length_m = coalesce(buffered10_overlap_length_m, 0),
      exact_prop_retained = exact_overlap_length_m / original_length_m,
      buffered10_prop_retained = buffered10_overlap_length_m / original_length_m
    )
}

model_overlap_diagnostics <- bind_rows(
  lapply(
    names(model_layers_bob),
    function(layer_name) {
      diagnose_model_overlap(
        model_layers_bob[[layer_name]],
        layer_name
      )
    }
  )
)

model_overlap_diagnostics |>
  group_by(layer_name) |>
  summarise(
    n_patches = n(),
    total_original_km = sum(original_length_m, na.rm = TRUE) / 1000,
    total_exact_overlap_km = sum(exact_overlap_length_m, na.rm = TRUE) / 1000,
    total_buffered10_overlap_km = sum(buffered10_overlap_length_m, na.rm = TRUE) / 1000,
    mean_exact_prop_retained = mean(exact_prop_retained, na.rm = TRUE),
    mean_buffered10_prop_retained = mean(buffered10_prop_retained, na.rm = TRUE),
    max_exact_prop_retained = max(exact_prop_retained, na.rm = TRUE),
    max_buffered10_prop_retained = max(buffered10_prop_retained, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(width = Inf)
# -----------------------------------------------------------------------------

## Map patches going into Table 2 ---------
stopifnot(
  exists("model_layers_bob"),
  exists("cutoff_buffer_10m_barrier_adjusted_analysis"),
  exists("huc12_bob"),
  all(c("mod23", "ext23") %in% names(model_layers_bob))
)

table2_models <- c(
  mod23 = "2040",
  ext23 = "2080"
)

cutoff_buffer_sf <- st_sf(
  geometry = st_sfc(cutoff_buffer_10m_barrier_adjusted_analysis, crs = st_crs(huc12_bob))
)

make_table2_current_patch_map_layers <- function(model_sf, model_layer, model_label) {
  model_sf <- model_sf |>
    st_make_valid() |>
    mutate(
      patch_row_id = row_number(),
      Model = model_label
    )
  
  retained <- suppressWarnings(
    st_intersection(model_sf, cutoff_buffer_sf)
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      patch_map_class = "Retained after cutoff",
      patch_length_km = as.numeric(st_length(geometry)) / 1000
    )
  
  removed <- suppressWarnings(
    st_difference(model_sf, st_union(cutoff_buffer_sf))
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      patch_map_class = "Removed by cutoff",
      patch_length_km = as.numeric(st_length(geometry)) / 1000
    )
  
  bind_rows(retained, removed) |>
    mutate(
      model_layer = model_layer,
      Model = factor(Model, levels = unname(table2_models)),
      patch_map_class = factor(
        patch_map_class,
        levels = c("Retained after cutoff", "Removed by cutoff")
      )
    )
}

table2_current_patch_map_layers <- bind_rows(
  lapply(names(table2_models), function(model_layer) {
    make_table2_current_patch_map_layers(
      model_layers_bob[[model_layer]],
      model_layer,
      table2_models[[model_layer]]
    )
  })
)

table2_original_context_layers <- bind_rows(
  lapply(names(table2_models), function(model_layer) {
    model_layers_bob[[model_layer]] |>
      st_make_valid() |>
      mutate(
        Model = factor(table2_models[[model_layer]], levels = unname(table2_models)),
        model_layer = model_layer
      )
  })
)

table2_current_patch_map <- ggplot() +
  geom_sf(data = huc12_bob, fill = "#F5F5F2", color = "white", linewidth = 0.25) +
  geom_sf(data = table2_original_context_layers, color = "#8F99A8", linewidth = 0.35, alpha = 0.45) +
  geom_sf(
    data = filter(table2_current_patch_map_layers, patch_map_class == "Removed by cutoff"),
    aes(color = patch_map_class),
    linewidth = 0.7,
    alpha = 0.75
  ) +
  geom_sf(
    data = filter(table2_current_patch_map_layers, patch_map_class == "Retained after cutoff"),
    aes(color = patch_map_class),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(data = huc12_bob, fill = NA, color = "#4D4D4D", linewidth = 0.25) +
  scale_color_manual(
    values = c(
      "Retained after cutoff" = "#0072B2",
      "Removed by cutoff" = "#D55E00"
    ),
    name = NULL
  ) +
  facet_wrap(vars(Model), ncol = 2) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Patches used in Table 2 current calculations",
    #subtitle = "Gray = original model patches; blue = retained after cutoff; orange = removed by cutoff.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

print(table2_current_patch_map)
## ---------------------------------------------------------------------------

## Patch length reductions after applying cutoffs ----------------------------
find_patch_length_column <- function(model_sf) {
  found <- intersect(
    c("PL", "LENGTH_KM"),
    names(model_sf)
  )
  
  if (length(found) == 0) {
    return(NA_character_)
  }
  
  found[1]
}

find_patch_id_column <- function(model_sf) {
  found <- intersect(
    c("Patch_ID", "PATCH_ID"),
    names(model_sf)
  )
  
  if (length(found) == 0) {
    return(NA_character_)
  }
  
  found[1]
}

quantify_patch_length_reduction <- function(model_sf, layer_name, cutoff_buffer) {
  patch_length_col <- find_patch_length_column(model_sf)
  patch_id_col <- find_patch_id_column(model_sf)
  
  model_sf <- model_sf |>
    mutate(
      patch_row_id = row_number(),
      patch_id = if (!is.na(patch_id_col)) {
        as.character(.data[[patch_id_col]])
      } else {
        as.character(patch_row_id)
      },
      original_attribute_length_km = if (!is.na(patch_length_col)) {
        as.numeric(.data[[patch_length_col]])
      } else {
        NA_real_
      },
      original_patch_length_km = as.numeric(st_length(geometry)) / 1000,
      patch_length_source = "geometry"
    )
  
  retained <- suppressWarnings(
    st_intersection(
      model_sf,
      st_sf(
        geometry = st_sfc(cutoff_buffer, crs = st_crs(model_sf))
      )
    )
  ) |>
    mutate(
      retained_geometry_length_km = as.numeric(st_length(geometry)) / 1000
    ) |>
    st_drop_geometry() |>
    group_by(patch_row_id) |>
    summarise(
      retained_patch_length_km = sum(retained_geometry_length_km, na.rm = TRUE),
      .groups = "drop"
    )
  
  model_sf |>
    st_drop_geometry() |>
    left_join(retained, by = "patch_row_id") |>
    mutate(
      model_layer = layer_name,
      retained_patch_length_km = coalesce(retained_patch_length_km, 0),
      reduced_patch_length_km = original_patch_length_km - retained_patch_length_km,
      prop_patch_length_retained = retained_patch_length_km / original_patch_length_km,
      prop_patch_length_reduced = reduced_patch_length_km / original_patch_length_km,
      retained_less_than_3km = retained_patch_length_km < 3,
      retained_nonzero_less_than_3km =
        retained_patch_length_km > 0 & retained_patch_length_km < 3
    )
}

patch_length_reduction_all <- bind_rows(
  lapply(names(model_layers_bob), function(layer_name) {
    quantify_patch_length_reduction(
      model_layers_bob[[layer_name]],
      layer_name,
      cutoff_buffer_10m_barrier_adjusted_analysis
    )
  })
)
# -----------------------------------------------------------------------------

## Focal HUC12 patch lengths before/after filters ---------------------------
target_huc12_names <- c(
  "Gorge Creek",
  "Babcock Creek",
  "Little Salmon Creek"
)

target_models <- c(
  mod17 = "2040",
  ext17 = "2080"
)

find_huc12_name_column <- function(huc12_sf, target_names) {
  candidate_cols <- names(huc12_sf)[!inherits(huc12_sf, "sfc")]
  
  matched_cols <- vapply(candidate_cols, function(col) {
    any(tolower(as.character(huc12_sf[[col]])) %in% tolower(target_names)) ||
      any(grepl(paste(target_names, collapse = "|"),
                as.character(huc12_sf[[col]]),
                ignore.case = TRUE))
  }, logical(1))
  
  if (!any(matched_cols)) {
    stop("Could not find a HUC12 name column matching target_huc12_names.")
  }
  
  candidate_cols[which(matched_cols)[1]]
}

huc12_name_col <- find_huc12_name_column(huc12_bob, target_huc12_names)

target_huc12s <- huc12_bob |>
  mutate(huc12_target_name = as.character(.data[[huc12_name_col]])) |>
  filter(huc12_target_name %in% target_huc12_names)

stopifnot(nrow(target_huc12s) == length(target_huc12_names))

get_largest_patch_in_huc12 <- function(model_layer) {
  model_sf <- model_layers_bob[[model_layer]] |>
    st_make_valid() |>
    mutate(
      patch_row_id = row_number(),
      model_layer = model_layer,
      original_total_patch_length_km = as.numeric(st_length(geometry)) / 1000
    )
  
  suppressWarnings(
    st_intersection(
      model_sf,
      target_huc12s |> select(huc12_target_name)
    )
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      original_length_inside_huc12_km = as.numeric(st_length(geometry)) / 1000
    ) |>
    st_drop_geometry() |>
    group_by(model_layer, huc12_target_name, patch_row_id) |>
    summarise(
      original_length_inside_huc12_km = sum(original_length_inside_huc12_km, na.rm = TRUE),
      original_total_patch_length_km = first(original_total_patch_length_km),
      .groups = "drop"
    ) |>
    group_by(model_layer, huc12_target_name) |>
    slice_max(original_length_inside_huc12_km, n = 1, with_ties = FALSE) |>
    ungroup()
}

largest_target_patches <- bind_rows(
  lapply(names(target_models), get_largest_patch_in_huc12)
)

target_patch_lengths_before_after <- largest_target_patches |>
  left_join(
    patch_length_reduction_all |>
      filter(model_layer %in% names(target_models)) |>
      select(
        model_layer,
        patch_row_id,
        original_patch_length_km,
        retained_patch_length_km,
        reduced_patch_length_km
      ),
    by = c("model_layer", "patch_row_id")
  ) |>
  mutate(
    Model = target_models[model_layer],
    retained_after_topographic_and_3km_km = if_else(
      retained_patch_length_km >= 3,
      retained_patch_length_km,
      0
    ),
    removed_after_topographic_and_3km_km =
      original_patch_length_km - retained_after_topographic_and_3km_km
  ) |>
  select(
    HUC12 = huc12_target_name,
    Model,
    original_patch_length_km,
    #retained_after_topographic_filters_km = retained_patch_length_km,
    retained_km = retained_after_topographic_and_3km_km,
    removed_km = removed_after_topographic_and_3km_km
  ) |>
  mutate(
    across(
      ends_with("_km"),
      ~ round(.x, 2)
    )
  ) |>
  arrange(HUC12, Model)

print(target_patch_lengths_before_after, n = Inf, width = Inf)
# -----------------------------------------------------------------------------

#### Code to make Table 2: ------------------------------------
model_comparison_table_2023 <- bind_rows(
  patch_length_reduction_all |>
    filter(model_layer %in% c("mod23", "ext23")) |>
    mutate(
      Model = case_when(
        model_layer == "mod23" ~ "2040",
        model_layer == "ext23" ~ "2080"
      ),
      Status = "Original",
      patch_length_km = original_patch_length_km
    ) |>
    group_by(Model, Status) |>
    summarise(
      `Suitable habitat (km)` = sum(patch_length_km, na.rm = TRUE),
      `Number of patches` = n(),
      `Minimum patch length (km)` = min(patch_length_km, na.rm = TRUE),
      `Median patch length (km)` = median(patch_length_km, na.rm = TRUE),
      `Maximum patch length (km)` = max(patch_length_km, na.rm = TRUE),
      .groups = "drop"
    ),
  
  patch_length_reduction_all |>
    filter(
      model_layer %in% c("mod23", "ext23"),
      retained_patch_length_km > 0
    ) |>
    mutate(
      Model = case_when(
        model_layer == "mod23" ~ "2040",
        model_layer == "ext23" ~ "2080"
      ),
      Status = "Current",
      patch_length_km = retained_patch_length_km
    ) |>
    group_by(Model, Status) |>
    summarise(
      `Suitable habitat (km)` = sum(patch_length_km, na.rm = TRUE),
      `Number of patches` = n(),
      `Minimum patch length (km)` = min(patch_length_km, na.rm = TRUE),
      `Median patch length (km)` = median(patch_length_km, na.rm = TRUE),
      `Maximum patch length (km)` = max(patch_length_km, na.rm = TRUE),
      .groups = "drop"
    )
) |>
  mutate(
    Status = factor(Status, levels = c("Original", "Current")),
    across(
      c(
        `Suitable habitat (km)`,
        `Minimum patch length (km)`,
        `Median patch length (km)`,
        `Maximum patch length (km)`
      ),
      ~ round(.x, 2)
    )
  ) |>
  arrange(Model, Status)

print(model_comparison_table_2023, n = Inf, width = Inf)
### ---------------------------------------------------------------------------

## Patch Length reduction summary: --------------------------------------------
patch_length_reduction_all |>
  dplyr::count(model_layer, patch_length_source)

patch_length_reduction_summary <- patch_length_reduction_all |>
  group_by(model_layer) |>
  summarise(
    n_patches = n(),
    n_patches_retained = sum(retained_patch_length_km > 0, na.rm = TRUE),
    total_original_patch_length_km = sum(original_patch_length_km, na.rm = TRUE),
    total_retained_patch_length_km = sum(retained_patch_length_km, na.rm = TRUE),
    total_reduced_patch_length_km = sum(reduced_patch_length_km, na.rm = TRUE),
    percent_patch_length_retained = round(
      100 * total_retained_patch_length_km / total_original_patch_length_km,
      1
    ),
    percent_patch_length_reduced = round(
      100 * total_reduced_patch_length_km / total_original_patch_length_km,
      1
    ),
    n_retained_less_than_3km = sum(retained_less_than_3km, na.rm = TRUE),
    n_retained_nonzero_less_than_3km = sum(
      retained_nonzero_less_than_3km,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print(patch_length_reduction_summary, width = Inf)
## ----------------------------------------------------------------------------

### Identify patches <3km in length: ------------------------------------------
patches_less_than_3km_after_cutoffs <- patch_length_reduction_all |>
  filter(retained_nonzero_less_than_3km) |>
  arrange(model_layer, retained_patch_length_km) |>
  select(
    model_layer,
    patch_row_id,
    patch_id,
    original_patch_length_km,
    retained_patch_length_km,
    reduced_patch_length_km,
    prop_patch_length_retained,
    prop_patch_length_reduced,
    patch_length_source
  )

patches_less_than_3km_after_cutoffs |>
  tibble::as_tibble() |>
  print(n = Inf, width = Inf)

patches_less_than_3km_summary <- patches_less_than_3km_after_cutoffs |>
  group_by(model_layer) |>
  summarise(
    n_patches_less_than_3km = n(),
    min_retained_patch_length_km = min(retained_patch_length_km, na.rm = TRUE),
    median_retained_patch_length_km = median(retained_patch_length_km, na.rm = TRUE),
    mean_retained_patch_length_km = mean(retained_patch_length_km, na.rm = TRUE),
    max_retained_patch_length_km = max(retained_patch_length_km, na.rm = TRUE),
    .groups = "drop"
  )

patches_less_than_3km_summary |>
  tibble::as_tibble() |>
  print(n = Inf, width = Inf)
# -----------------------------------------------------------------------------

## Map final retained patches after 3 km minimum ------------------------------
stopifnot(
  exists("model_layers_bob"),
  exists("patch_length_reduction_all"),
  exists("cutoff_buffer_10m_barrier_adjusted_analysis"),
  exists("huc12_bob"),
  all(c("mod23", "ext23") %in% names(model_layers_bob))
)

table2_models <- c(
  mod23 = "2040",
  ext23 = "2080"
)

cutoff_buffer_sf <- st_sf(
  geometry = st_sfc(cutoff_buffer_10m_barrier_adjusted_analysis, crs = st_crs(huc12_bob))
)

make_table2_3km_patch_map_layers <- function(model_sf, model_layer, model_label) {
  model_sf <- model_sf |>
    st_make_valid() |>
    mutate(
      patch_row_id = row_number(),
      Model = model_label
    )
  
  retained_status <- patch_length_reduction_all |>
    filter(model_layer == !!model_layer) |>
    st_drop_geometry() |>
    select(
      patch_row_id,
      retained_patch_length_km
    ) |>
    mutate(
      retained_after_3km_minimum = retained_patch_length_km >= 3
    )
  
  retained_after_cutoff <- suppressWarnings(
    st_intersection(model_sf, cutoff_buffer_sf)
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    left_join(retained_status, by = "patch_row_id") |>
    mutate(
      patch_map_class = if_else(
        retained_after_3km_minimum,
        "Retained after 3 km minimum",
        "Removed by 3 km minimum"
      ),
      patch_length_km = as.numeric(st_length(geometry)) / 1000
    )
  
  removed_by_cutoff_barrier <- suppressWarnings(
    st_difference(model_sf, st_union(cutoff_buffer_sf))
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      patch_map_class = "Removed by cutoff/barrier filters",
      patch_length_km = as.numeric(st_length(geometry)) / 1000
    )
  
  bind_rows(retained_after_cutoff, removed_by_cutoff_barrier) |>
    mutate(
      model_layer = model_layer,
      Model = factor(Model, levels = unname(table2_models)),
      patch_map_class = factor(
        patch_map_class,
        levels = c(
          "Retained after 3 km minimum",
          "Removed by 3 km minimum",
          "Removed by cutoff/barrier filters"
        )
      )
    )
}

table2_3km_patch_map_layers <- bind_rows(
  lapply(names(table2_models), function(model_layer) {
    make_table2_3km_patch_map_layers(
      model_layers_bob[[model_layer]],
      model_layer,
      table2_models[[model_layer]]
    )
  })
)

table2_3km_original_context_layers <- bind_rows(
  lapply(names(table2_models), function(model_layer) {
    model_layers_bob[[model_layer]] |>
      st_make_valid() |>
      mutate(
        Model = factor(table2_models[[model_layer]], levels = unname(table2_models)),
        model_layer = model_layer
      )
  })
)

table2_3km_patch_map <- ggplot() +
  geom_sf(data = huc12_bob, fill = "#F5F5F2", color = "white", linewidth = 0.25) +
  geom_sf(data = table2_3km_original_context_layers, color = "#8F99A8", linewidth = 0.35, alpha = 0.40) +
  geom_sf(
    data = filter(table2_3km_patch_map_layers, patch_map_class == "Removed by cutoff/barrier filters"),
    aes(color = patch_map_class),
    linewidth = 0.65,
    alpha = 0.65
  ) +
  geom_sf(
    data = filter(table2_3km_patch_map_layers, patch_map_class == "Removed by 3 km minimum"),
    aes(color = patch_map_class),
    linewidth = 0.8,
    alpha = 0.85
  ) +
  geom_sf(
    data = filter(table2_3km_patch_map_layers, patch_map_class == "Retained after 3 km minimum"),
    aes(color = patch_map_class),
    linewidth = 1.1,
    alpha = 0.95
  ) +
  geom_sf(data = huc12_bob, fill = NA, color = "#4D4D4D", linewidth = 0.25) +
  scale_color_manual(
    values = c(
      "Retained after 3 km minimum" = "#0072B2",
      "Removed by 3 km minimum" = "#CC79A7",
      "Removed by cutoff/barrier filters" = "#D55E00"
    ),
    name = NULL
  ) +
  facet_wrap(vars(Model), ncol = 2) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Patches retained after cutoff/barrier filters and 3 km minimum",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

print(table2_3km_patch_map)
# -----------------------------------------------------------------------------

## Table 2 with topo cutoffs/barriers AND 3 km minimum rule -------------------
model_comparison_table_2023_with_3km <- bind_rows(
  ## Original model patches
  patch_length_reduction_all |>
    filter(model_layer %in% c("mod23", "ext23")) |>
    mutate(
      Model = case_when(
        model_layer == "mod23" ~ "2040",
        model_layer == "ext23" ~ "2080"
      ),
      Status = "Original",
      patch_length_km = original_patch_length_km
    ) |>
    group_by(Model, Status) |>
    summarise(
      `Suitable habitat (km)` = sum(patch_length_km, na.rm = TRUE),
      `Number of patches` = n(),
      `Minimum patch length (km)` = min(patch_length_km, na.rm = TRUE),
      `Median patch length (km)` = median(patch_length_km, na.rm = TRUE),
      `Maximum patch length (km)` = max(patch_length_km, na.rm = TRUE),
      .groups = "drop"
    ),
  
  ## After elevation, drainage area, and barrier filters
  patch_length_reduction_all |>
    filter(
      model_layer %in% c("mod23", "ext23"),
      retained_patch_length_km > 0
    ) |>
    mutate(
      Model = case_when(
        model_layer == "mod23" ~ "2040",
        model_layer == "ext23" ~ "2080"
      ),
      Status = "After elevation/drainage/barrier filters",
      patch_length_km = retained_patch_length_km
    ) |>
    group_by(Model, Status) |>
    summarise(
      `Suitable habitat (km)` = sum(patch_length_km, na.rm = TRUE),
      `Number of patches` = n(),
      `Minimum patch length (km)` = min(patch_length_km, na.rm = TRUE),
      `Median patch length (km)` = median(patch_length_km, na.rm = TRUE),
      `Maximum patch length (km)` = max(patch_length_km, na.rm = TRUE),
      .groups = "drop"
    ),
  
  ## After 3 km minimum-length rule
  patch_length_reduction_all |>
    filter(
      model_layer %in% c("mod23", "ext23"),
      retained_patch_length_km >= 3
    ) |>
    mutate(
      Model = case_when(
        model_layer == "mod23" ~ "2040",
        model_layer == "ext23" ~ "2080"
      ),
      Status = "After 3 km minimum",
      patch_length_km = retained_patch_length_km
    ) |>
    group_by(Model, Status) |>
    summarise(
      `Suitable habitat (km)` = sum(patch_length_km, na.rm = TRUE),
      `Number of patches` = n(),
      `Minimum patch length (km)` = min(patch_length_km, na.rm = TRUE),
      `Median patch length (km)` = median(patch_length_km, na.rm = TRUE),
      `Maximum patch length (km)` = max(patch_length_km, na.rm = TRUE),
      .groups = "drop"
    )
) |>
  mutate(
    Status = factor(
      Status,
      levels = c(
        "Original",
        "After elevation/drainage/barrier filters",
        "After 3 km minimum"
      )
    ),
    across(
      c(
        `Suitable habitat (km)`,
        `Minimum patch length (km)`,
        `Median patch length (km)`,
        `Maximum patch length (km)`
      ),
      ~ round(.x, 2)
    )
  ) |>
  arrange(Model, Status)

print(model_comparison_table_2023_with_3km, n = Inf, width = Inf)
# -----------------------------------------------------------------------------

## Patch-length summary after applying stream cutoffs and 3 km minimum --------
patch_length_3km_summary <- patch_length_reduction_all |>
  mutate(
    retained_after_cutoffs_km = retained_patch_length_km,
    retained_after_3km_minimum_km = if_else(
      retained_patch_length_km >= 3,
      retained_patch_length_km,
      0
    ),
    eliminated_by_stream_cutoffs = retained_patch_length_km == 0,
    eliminated_by_3km_minimum = retained_patch_length_km > 0 &
      retained_patch_length_km < 3,
    retained_after_3km_minimum = retained_patch_length_km >= 3
  ) |>
  group_by(model_layer) |>
  summarise(
    n_original_patches = n(),
    n_retained_after_stream_cutoffs = sum(retained_after_cutoffs_km > 0, na.rm = TRUE),
    n_eliminated_by_stream_cutoffs = sum(eliminated_by_stream_cutoffs, na.rm = TRUE),
    n_eliminated_by_3km_minimum = sum(eliminated_by_3km_minimum, na.rm = TRUE),
    n_retained_after_3km_minimum = sum(retained_after_3km_minimum, na.rm = TRUE),
    original_patch_length_km = sum(original_patch_length_km, na.rm = TRUE),
    retained_after_stream_cutoffs_km = sum(retained_after_cutoffs_km, na.rm = TRUE),
    retained_after_3km_minimum_km = sum(retained_after_3km_minimum_km, na.rm = TRUE),
    total_reduced_after_3km_minimum_km =
      original_patch_length_km - retained_after_3km_minimum_km,
    percent_retained_after_3km_minimum = round(
      100 * retained_after_3km_minimum_km / original_patch_length_km,
      1
    ),
    percent_reduced_after_3km_minimum = round(
      100 * total_reduced_after_3km_minimum_km / original_patch_length_km,
      1
    ),
    .groups = "drop"
  )

print(patch_length_3km_summary, width = Inf)

## Create dataset with only final retained patches ----------------------------
patch_length_reduction_3km_retained <- patch_length_reduction_all |>
  filter(retained_patch_length_km >= 3) |>
  mutate(
    model_label = case_when(
      model_layer == "base17" ~ "2017: Baseline",
      model_layer == "mod17" ~ "2017: 2040 Projection",
      model_layer == "ext17" ~ "2017: 2080 Projection",
      model_layer == "mod23" ~ "2023: 2040 Projection",
      model_layer == "ext23" ~ "2023: 2080 Projection",
      TRUE ~ model_layer
    )
  )
# -----------------------------------------------------------------------------


####### Updating Figure 7 (4-panel maps) **2015 MODEL** ######################################
##  USING THE 2015 MODEL!!!
stopifnot(
  exists("model_layers_bob"),
  exists("huc12_bob"),
  all(c("mod17", "ext17") %in% names(model_layers_bob))
)

library(patchwork)

if (!exists("cutoff_buffer_sf")) {
  stopifnot(exists("cutoff_buffer_10m_barrier_adjusted_analysis"))
  cutoff_buffer_sf <- st_sf(
    geometry = st_sfc(cutoff_buffer_10m_barrier_adjusted_analysis, crs = st_crs(huc12_bob))
  )
}

model_2017_projection_labels <- c(
  mod17 = "2040 projection",
  ext17 = "2080 projection"
)

cutoff_rule_labels <- c(
  topo_only = "Topographic thresholds",
  topo_3km = "Topographic thresholds\n+ 3 km minimum length"
)

make_2017_cutoff_comparison_layers <- function(model_sf, model_layer, model_label) {
  model_sf <- model_sf |>
    st_make_valid() |>
    mutate(
      patch_row_id = row_number(),
      Model = model_label
    )
  
  retained_topo <- suppressWarnings(
    st_intersection(model_sf, cutoff_buffer_sf)
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      retained_segment_length_km = as.numeric(st_length(geometry)) / 1000
    )
  
  retained_status <- retained_topo |>
    st_drop_geometry() |>
    group_by(patch_row_id) |>
    summarise(
      retained_patch_length_km = sum(retained_segment_length_km, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(retained_after_3km_minimum = retained_patch_length_km >= 3)
  
  retained_topo_only <- retained_topo |>
    mutate(
      Rule = cutoff_rule_labels[["topo_only"]],
      patch_map_class = "Retained suitable habitat",
      patch_length_km = retained_segment_length_km
    ) |>
    select(Model, Rule, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo <- suppressWarnings(
    st_difference(model_sf, st_union(cutoff_buffer_sf))
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      patch_map_class = "Removed by topographic/barrier thresholds",
      patch_length_km = as.numeric(st_length(geometry)) / 1000
    ) |>
    select(Model, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo_only <- removed_by_topo |>
    mutate(Rule = cutoff_rule_labels[["topo_only"]])
  
  retained_after_3km <- retained_topo |>
    left_join(retained_status, by = "patch_row_id") |>
    mutate(
      Rule = cutoff_rule_labels[["topo_3km"]],
      patch_map_class = if_else(
        retained_after_3km_minimum,
        "Retained suitable habitat",
        "Removed by 3 km minimum"
      ),
      patch_length_km = retained_segment_length_km
    ) |>
    select(Model, Rule, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo_3km <- removed_by_topo |>
    mutate(Rule = cutoff_rule_labels[["topo_3km"]])
  
  bind_rows(
    retained_topo_only,
    removed_by_topo_only,
    retained_after_3km,
    removed_by_topo_3km
  ) |>
    mutate(
      model_layer = model_layer,
      Model = factor(Model, levels = unname(model_2017_projection_labels)),
      Rule = factor(Rule, levels = unname(cutoff_rule_labels)),
      patch_map_class = factor(
        patch_map_class,
        levels = c(
          "Retained suitable habitat",
          "Removed by 3 km minimum",
          "Removed by topographic/barrier thresholds"
        )
      )
    )
}

patch_comparison_2017_layers <- bind_rows(
  lapply(names(model_2017_projection_labels), function(model_layer) {
    make_2017_cutoff_comparison_layers(
      model_layers_bob[[model_layer]],
      model_layer,
      model_2017_projection_labels[[model_layer]]
    )
  })
)

patch_comparison_2017_context_layers <- bind_rows(
  lapply(names(model_2017_projection_labels), function(model_layer) {
    bind_rows(
      lapply(unname(cutoff_rule_labels), function(rule_label) {
        model_layers_bob[[model_layer]] |>
          st_make_valid() |>
          mutate(
            Model = factor(
              model_2017_projection_labels[[model_layer]],
              levels = unname(model_2017_projection_labels)
            ),
            Rule = factor(rule_label, levels = unname(cutoff_rule_labels)),
            model_layer = model_layer
          )
      })
    )
  })
)

patch_comparison_2017_summary <- patch_comparison_2017_layers |>
  st_drop_geometry() |>
  group_by(Model, Rule, patch_map_class) |>
  summarise(
    n_patch_segments = n(),
    mapped_length_km = round(sum(patch_length_km, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(patch_comparison_2017_summary, n = Inf, width = Inf)

## letters for 4 panels, a b c d -------------------------
panel_label_data <- tidyr::expand_grid(
  Model = factor(
    unname(model_2017_projection_labels),
    levels = unname(model_2017_projection_labels)
  ),
  Rule = factor(
    unname(cutoff_rule_labels),
    levels = unname(cutoff_rule_labels)
  )
) |>
  mutate(
    panel_label = c("a", "b", "c", "d")
  )

label_x <- st_bbox(huc12_bob)[["xmin"]]
label_y <- st_bbox(huc12_bob)[["ymax"]]
## ------------------------------------------------------

### Map a. Includes legend item for 3km cutoff patches  --------
patch_comparison_2015_map <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = patch_comparison_2017_context_layers,
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.38
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_layers,
      patch_map_class == "Removed by topographic/barrier thresholds"
    ),
    aes(color = patch_map_class),
    linewidth = 0.65,
    alpha = 0.70
  ) +
  geom_sf(
    data = filter(patch_comparison_2017_layers, patch_map_class == "Removed by 3 km minimum"),
    aes(color = patch_map_class),
    linewidth = 0.80,
    alpha = 0.85
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_layers,
      patch_map_class == "Retained suitable habitat"
    ),
    aes(color = patch_map_class),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  scale_color_manual(
    values = c(
      "Removed by topographic/barrier thresholds" = "#D55E00",
      "Removed by 3 km minimum" = "darkred",
      "Retained suitable habitat" = "#0072B2"
    ),
    breaks = c(
      "Removed by topographic/barrier thresholds",
      "Removed by 3 km minimum",
      "Retained suitable habitat"
    ),
    labels = c(
      "Removed by topographic thresholds",
      "Removed by 3 km minimum",
      "Retained suitable habitat"
    ),
    name = NULL,
    #guide = "none"
    guide = guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  ) +
  #guides(color = "none") +  ## comment out to bring back legend
  facet_grid(rows = vars(Model), cols = vars(Rule), switch = "y") +
  coord_sf(expand = FALSE) +
  geom_text(
    data = panel_label_data,
    aes(x = label_x, y = label_y, label = panel_label),
    inherit.aes = FALSE,
    hjust = -0.35,
    vjust = 1.4,
    fontface = "bold",
    size = 5
  ) +
  labs(
    title = "Projected suitable habitat for 2040 & 2080",
    #subtitle = "Blue = retained habitat; orange = patches removed by topographic/barrier thresholds",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #legend.position = "none",
    legend.position = "bottom",
    legend.box = "vertical",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", angle = 90)
)


print(patch_comparison_2015_map)

### Map b. without the 3km eliminations highlighted -------------
patch_comparison_2015_map_no_red <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = patch_comparison_2017_context_layers,
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.38
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_layers,
      patch_map_class %in% c(
        "Removed by topographic/barrier thresholds",
        "Removed by 3 km minimum"
      )
    ),
    aes(color = "Suitable habitat removed"),
    linewidth = 0.65,
    alpha = 0.70
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_layers,
      patch_map_class == "Retained suitable habitat"
    ),
    aes(color = patch_map_class),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  scale_color_manual(
    values = c(
      "Suitable habitat removed" = "#D55E00",
      "Retained suitable habitat" = "#0072B2"
    ),
    breaks = c(
      "Suitable habitat removed",
      "Retained suitable habitat"
    ),
    labels = c(
      "Suitable habitat removed",
      "Retained suitable habitat"
    ),
    name = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.2)
    )
  ) +
  facet_grid(rows = vars(Model), cols = vars(Rule), switch = "y") +
  coord_sf(expand = FALSE) +
  geom_text(
    data = panel_label_data,
    aes(x = label_x, y = label_y, label = panel_label),
    inherit.aes = FALSE,
    hjust = -0.35,
    vjust = 1.4,
    fontface = "bold",
    size = 5
  ) +
  labs(
    title = "Projected suitable habitat for 2040 & 2080",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", angle = 90)
  )

print(patch_comparison_2015_map_no_red)
# -----------------------------------------------------------------------------

####### Updating Figure 7 (4-panel maps) **2022 MODEL** ######################################
##  USING THE 2022 MODEL!!!
stopifnot(
  exists("model_layers_bob"),
  exists("huc12_bob"),
  all(c("mod23", "ext23") %in% names(model_layers_bob))
)

library(patchwork)

if (!exists("cutoff_buffer_sf")) {
  stopifnot(exists("cutoff_buffer_10m_barrier_adjusted_analysis"))
  cutoff_buffer_sf <- st_sf(
    geometry = st_sfc(cutoff_buffer_10m_barrier_adjusted_analysis, crs = st_crs(huc12_bob))
  )
}

model_2023_projection_labels <- c(
  mod23 = "Current suitable habitat",
  ext23 = "Future suitable habitat"
)

cutoff_rule_labels <- c(
  topo_only = "Topographic thresholds",
  topo_3km = "Topographic thresholds\n+ 3 km minimum length"
)

## HUC12s to highlight ---------------------------------------------------------
highlight_huc12_terms <- c(
  "Babcock Creek",
  "Little Salmon Creek"
)

huc12_attribute_text <- huc12_bob |>
  st_drop_geometry() |>
  mutate(
    huc12_search_text = apply(
      across(everything(), as.character),
      1,
      paste,
      collapse = " | "
    )
  ) |>
  select(huc12_search_text)

highlight_huc12_bob <- huc12_bob |>
  bind_cols(huc12_attribute_text) |>
  filter(
    grepl(
      paste(highlight_huc12_terms, collapse = "|"),
      huc12_search_text,
      ignore.case = TRUE
    )
  ) |>
  st_make_valid()

stopifnot(nrow(highlight_huc12_bob) > 0)

highlight_huc12_bob |>
  st_drop_geometry() |>
  select(huc12_search_text) |>
  tibble::as_tibble() |>
  print(n = Inf, width = Inf)
## ----------------------------------------------------------------------------

make_2023_cutoff_comparison_layers <- function(model_sf, model_layer, model_label) {
  model_sf <- model_sf |>
    st_make_valid() |>
    mutate(
      patch_row_id = row_number(),
      Model = model_label
    )
  
  retained_topo <- suppressWarnings(
    st_intersection(model_sf, cutoff_buffer_sf)
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      retained_segment_length_km = as.numeric(st_length(geometry)) / 1000
    )
  
  retained_status <- retained_topo |>
    st_drop_geometry() |>
    group_by(patch_row_id) |>
    summarise(
      retained_patch_length_km = sum(retained_segment_length_km, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(retained_after_3km_minimum = retained_patch_length_km >= 3)
  
  retained_topo_only <- retained_topo |>
    mutate(
      Rule = cutoff_rule_labels[["topo_only"]],
      patch_map_class = "Retained suitable habitat",
      patch_length_km = retained_segment_length_km
    ) |>
    select(Model, Rule, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo <- suppressWarnings(
    st_difference(model_sf, st_union(cutoff_buffer_sf))
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      patch_map_class = "Removed by topographic/barrier thresholds",
      patch_length_km = as.numeric(st_length(geometry)) / 1000
    ) |>
    select(Model, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo_only <- removed_by_topo |>
    mutate(Rule = cutoff_rule_labels[["topo_only"]])
  
  retained_after_3km <- retained_topo |>
    left_join(retained_status, by = "patch_row_id") |>
    mutate(
      Rule = cutoff_rule_labels[["topo_3km"]],
      patch_map_class = if_else(
        retained_after_3km_minimum,
        "Retained suitable habitat",
        "Removed by 3 km minimum"
      ),
      patch_length_km = retained_segment_length_km
    ) |>
    select(Model, Rule, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo_3km <- removed_by_topo |>
    mutate(Rule = cutoff_rule_labels[["topo_3km"]])
  
  bind_rows(
    retained_topo_only,
    removed_by_topo_only,
    retained_after_3km,
    removed_by_topo_3km
  ) |>
    mutate(
      model_layer = model_layer,
      Model = factor(Model, levels = unname(model_2023_projection_labels)),
      Rule = factor(Rule, levels = unname(cutoff_rule_labels)),
      patch_map_class = factor(
        patch_map_class,
        levels = c(
          "Retained suitable habitat",
          "Removed by 3 km minimum",
          "Removed by topographic/barrier thresholds"
        )
      )
    )
}

patch_comparison_2023_layers <- bind_rows(
  lapply(names(model_2023_projection_labels), function(model_layer) {
    make_2023_cutoff_comparison_layers(
      model_layers_bob[[model_layer]],
      model_layer,
      model_2023_projection_labels[[model_layer]]
    )
  })
)

patch_comparison_2023_context_layers <- bind_rows(
  lapply(names(model_2023_projection_labels), function(model_layer) {
    bind_rows(
      lapply(unname(cutoff_rule_labels), function(rule_label) {
        model_layers_bob[[model_layer]] |>
          st_make_valid() |>
          mutate(
            Model = factor(
              model_2023_projection_labels[[model_layer]],
              levels = unname(model_2023_projection_labels)
            ),
            Rule = factor(rule_label, levels = unname(cutoff_rule_labels)),
            model_layer = model_layer
          )
      })
    )
  })
)

patch_comparison_2023_summary <- patch_comparison_2023_layers |>
  st_drop_geometry() |>
  group_by(Model, Rule, patch_map_class) |>
  summarise(
    n_patch_segments = n(),
    mapped_length_km = round(sum(patch_length_km, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(patch_comparison_2023_summary, n = Inf, width = Inf)

## letters for 4 panels, a b c d -------------------------
panel_label_data <- tidyr::expand_grid(
  Model = factor(
    unname(model_2023_projection_labels),
    levels = unname(model_2023_projection_labels)
  ),
  Rule = factor(
    unname(cutoff_rule_labels),
    levels = unname(cutoff_rule_labels)
  )
) |>
  mutate(
    panel_label = c("a", "b", "c", "d")
  )

label_x <- st_bbox(huc12_bob)[["xmin"]]
label_y <- st_bbox(huc12_bob)[["ymax"]]
## ------------------------------------------------------

### Map a. Includes legend item for 3km cutoff patches  --------
patch_comparison_2023_map <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = highlight_huc12_bob,
    fill = "#D9F0D3",
    color = NA,
    alpha = 0.75
  ) +
  geom_sf(
    data = patch_comparison_2023_context_layers,
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.38
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2023_layers,
      patch_map_class == "Removed by topographic/barrier thresholds"
    ),
    aes(color = patch_map_class),
    linewidth = 0.65,
    alpha = 0.70
  ) +
  geom_sf(
    data = filter(patch_comparison_2023_layers, patch_map_class == "Removed by 3 km minimum"),
    aes(color = patch_map_class),
    linewidth = 0.80,
    alpha = 0.85
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2023_layers,
      patch_map_class == "Retained suitable habitat"
    ),
    aes(color = patch_map_class),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  scale_color_manual(
    values = c(
      "Removed by topographic/barrier thresholds" = "#D55E00",
      "Removed by 3 km minimum" = "darkred",
      "Retained suitable habitat" = "#0072B2"
    ),
    breaks = c(
      "Removed by topographic/barrier thresholds",
      "Removed by 3 km minimum",
      "Retained suitable habitat"
    ),
    labels = c(
      "Removed by topographic thresholds",
      "Removed by 3 km minimum",
      "Retained suitable habitat"
    ),
    name = NULL,
    #guide = "none"
    guide = guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  ) +
  #guides(color = "none") +  ## comment out to bring back legend
  facet_grid(rows = vars(Model), cols = vars(Rule), switch = "y") +
  coord_sf(expand = FALSE) +
  geom_text(
    data = panel_label_data,
    aes(x = label_x, y = label_y, label = panel_label),
    inherit.aes = FALSE,
    hjust = -0.35,
    vjust = 1.4,
    fontface = "bold",
    size = 5
  ) +
  labs(
    title = "Projected suitable habitat for 2040 & 2080",
    #subtitle = "Blue = retained habitat; orange = patches removed by topographic/barrier thresholds",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #legend.position = "none",
    legend.position = "bottom",
    legend.box = "vertical",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", angle = 90)
  )


print(patch_comparison_2023_map)

### Map b. without the 3km eliminations highlighted -------------
patch_comparison_2023_map_no_red <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = highlight_huc12_bob,
    fill = "#D9F0D3",
    color = NA,
    alpha = 0.75
  ) +
  geom_sf(
    data = patch_comparison_2023_context_layers,
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.38
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2023_layers,
      patch_map_class %in% c(
        "Removed by topographic/barrier thresholds",
        "Removed by 3 km minimum"
      )
    ),
    aes(color = "Suitable habitat removed"),
    linewidth = 0.65,
    alpha = 0.70
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2023_layers,
      patch_map_class == "Retained suitable habitat"
    ),
    aes(color = patch_map_class),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  scale_color_manual(
    values = c(
      "Suitable habitat removed" = "#D55E00",
      "Retained suitable habitat" = "#0072B2"
    ),
    breaks = c(
      "Suitable habitat removed",
      "Retained suitable habitat"
    ),
    labels = c(
      "Suitable habitat removed",
      "Retained suitable habitat"
    ),
    name = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.2)
    )
  ) +
  facet_grid(rows = vars(Model), cols = vars(Rule), switch = "y") +
  coord_sf(expand = FALSE) +
  geom_text(
    data = panel_label_data,
    aes(x = label_x, y = label_y, label = panel_label),
    inherit.aes = FALSE,
    hjust = -0.35,
    vjust = 1.4,
    fontface = "bold",
    size = 5
  ) +
  labs(
    title = "Projected current and future suitable habitat",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", angle = 90)
  )

print(patch_comparison_2023_map_no_red)
# -----------------------------------------------------------------------------


### FIGURE 7 VERSIONS FOR PRESENTATION: 
## Single-panel Figure 7 maps: topographic thresholds only  -------------------
make_single_topo_threshold_map_2023 <- function(model_label, plot_title) {
  single_context <- patch_comparison_2023_context_layers |>
    filter(
      Model == model_label,
      Rule == cutoff_rule_labels[["topo_only"]]
    )
  
  single_layers <- patch_comparison_2023_layers |>
    filter(
      Model == model_label,
      Rule == cutoff_rule_labels[["topo_only"]]
    )
  
  ggplot() +
    geom_sf(
      data = huc12_bob,
      fill = "#F5F5F2",
      color = "white",
      linewidth = 0.25
    ) +
    geom_sf(
      data = single_context,
      color = "#8F99A8",
      linewidth = 0.35,
      alpha = 0.38
    ) +
    geom_sf(
      data = filter(
        single_layers,
        patch_map_class == "Removed by topographic/barrier thresholds"
      ),
      aes(color = patch_map_class),
      linewidth = 0.65,
      alpha = 0.70
    ) +
    geom_sf(
      data = filter(
        single_layers,
        patch_map_class == "Retained suitable habitat"
      ),
      aes(color = patch_map_class),
      linewidth = 1.05,
      alpha = 0.95
    ) +
    geom_sf(
      data = huc12_bob,
      fill = NA,
      color = "#4D4D4D",
      linewidth = 0.25
    ) +
    scale_color_manual(
      values = c(
        "Removed by topographic/barrier thresholds" = "#D55E00",
        "Retained suitable habitat" = "#0072B2"
      ),
      breaks = c(
        "Removed by topographic/barrier thresholds",
        "Retained suitable habitat"
      ),
      labels = c(
        "Removed by topographic thresholds",
        "Retained suitable habitat"
      ),
      name = NULL
    ) +
    guides(
      color = guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(linewidth = 1.2)
      )
    ) +
    coord_sf(expand = FALSE) +
    labs(
      title = plot_title,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 15) +
    theme(
      panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.justification = "center",
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

Figure7_2040_topo_only_map <- make_single_topo_threshold_map_2023(
  model_label = "2040 projection",
  plot_title = "Projected suitable habitat for 2040"
)

Figure7_2080_topo_only_map <- make_single_topo_threshold_map_2023(
  model_label = "2080 projection",
  plot_title = "Projected suitable habitat for 2080"
)

print(Figure7_2040_topo_only_map)
print(Figure7_2080_topo_only_map)

### 2080 version with green subwatersheds: ------------------------------------
Figure7_2080_topo_only_map_green_huc12 <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = highlight_huc12_bob,
    fill = "#D9F0D3",
    color = NA,
    alpha = 0.75
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2023_context_layers,
      Model == "2080 projection",
      Rule == cutoff_rule_labels[["topo_only"]]
    ),
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.38
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2023_layers,
      Model == "2080 projection",
      Rule == cutoff_rule_labels[["topo_only"]],
      patch_map_class == "Removed by topographic/barrier thresholds"
    ),
    aes(color = patch_map_class),
    linewidth = 0.65,
    alpha = 0.70
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2023_layers,
      Model == "2080 projection",
      Rule == cutoff_rule_labels[["topo_only"]],
      patch_map_class == "Retained suitable habitat"
    ),
    aes(color = patch_map_class),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  scale_color_manual(
    values = c(
      "Removed by topographic/barrier thresholds" = "#D55E00",
      "Retained suitable habitat" = "#0072B2"
    ),
    breaks = c(
      "Removed by topographic/barrier thresholds",
      "Retained suitable habitat"
    ),
    labels = c(
      "Removed by topographic thresholds",
      "Retained suitable habitat"
    ),
    name = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.2)
    )
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Projected suitable habitat for 2080",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(Figure7_2080_topo_only_map_green_huc12)
# -----------------------------------------------------------------------------



####### Figure S1 **2015 MODEL: 2080 ONLY** ############################
##  USING THE 2015 MODEL!!!
stopifnot(
  exists("model_layers_bob"),
  exists("huc12_bob"),
  all(c("ext17") %in% names(model_layers_bob))
)

library(patchwork)

if (!exists("cutoff_buffer_sf")) {
  stopifnot(exists("cutoff_buffer_10m_barrier_adjusted_analysis"))
  cutoff_buffer_sf <- st_sf(
    geometry = st_sfc(cutoff_buffer_10m_barrier_adjusted_analysis, crs = st_crs(huc12_bob))
  )
}

model_2017_projection_labels <- c(
  ext17 = "2080 projection"
)

cutoff_rule_labels <- c(
  topo_only = "Topographic thresholds",
  topo_3km = "Topographic thresholds\n+ 3 km minimum length"
)

## HUC12s to highlight  -------------------------------------------------------
highlight_huc12_terms <- c(
  "Gorge Creek",
  "Spotted Bear",
  #"Silvertip Creek,"
  #"Upper Gordon Creek",
  "Wall Creek"
)

huc12_attribute_text <- huc12_bob |>
  st_drop_geometry() |>
  mutate(
    huc12_search_text = apply(
      across(everything(), as.character),
      1,
      paste,
      collapse = " | "
    )
  ) |>
  select(huc12_search_text)

highlight_huc12_bob <- huc12_bob |>
  bind_cols(huc12_attribute_text) |>
  filter(
    grepl(
      paste(highlight_huc12_terms, collapse = "|"),
      huc12_search_text,
      ignore.case = TRUE
    )
  ) |>
  st_make_valid()

stopifnot(nrow(highlight_huc12_bob) > 0)

highlight_huc12_bob |>
  st_drop_geometry() |>
  select(huc12_search_text) |>
  tibble::as_tibble() |>
  print(n = Inf, width = Inf)

## Flowlines and barriers to add to maps ---------------------------------------
stopifnot(
  exists("streams_map"),
  inherits(streams_map, "sf")
)

flowlines_bob <- streams_map |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(st_crs(huc12_bob)) |>
  st_make_valid()

## Use whichever barrier point object exists in the workspace
barrier_object_candidates <- c(
  "barriers_sf",
  "barrier_points",
  "barrier_points_sf",
  "passage_barriers",
  "barriers_map"
)

barrier_object_name <- barrier_object_candidates[
  barrier_object_candidates %in% ls()
][1]

stopifnot(!is.na(barrier_object_name))

barriers_bob <- get(barrier_object_name) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(st_crs(huc12_bob)) |>
  st_make_valid()
## ----------------------------------------------------------------------------

## --- Make cutoff comparison layers --------
make_2017_cutoff_comparison_layers <- function(model_sf, model_layer, model_label) {
  model_sf <- model_sf |>
    st_make_valid() |>
    mutate(
      patch_row_id = row_number(),
      Model = model_label
    )
  
  retained_topo <- suppressWarnings(
    st_intersection(model_sf, cutoff_buffer_sf)
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      retained_segment_length_km = as.numeric(st_length(geometry)) / 1000
    )
  
  retained_status <- retained_topo |>
    st_drop_geometry() |>
    group_by(patch_row_id) |>
    summarise(
      retained_patch_length_km = sum(retained_segment_length_km, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(retained_after_3km_minimum = retained_patch_length_km >= 3)
  
  retained_topo_only <- retained_topo |>
    mutate(
      Rule = cutoff_rule_labels[["topo_only"]],
      patch_map_class = "Retained suitable habitat",
      patch_length_km = retained_segment_length_km
    ) |>
    select(Model, Rule, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo <- suppressWarnings(
    st_difference(model_sf, st_union(cutoff_buffer_sf))
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      patch_map_class = "Removed by topographic/barrier thresholds",
      patch_length_km = as.numeric(st_length(geometry)) / 1000
    ) |>
    select(Model, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo_only <- removed_by_topo |>
    mutate(Rule = cutoff_rule_labels[["topo_only"]])
  
  retained_after_3km <- retained_topo |>
    left_join(retained_status, by = "patch_row_id") |>
    mutate(
      Rule = cutoff_rule_labels[["topo_3km"]],
      patch_map_class = if_else(
        retained_after_3km_minimum,
        "Retained suitable habitat",
        "Removed by 3 km minimum"
      ),
      patch_length_km = retained_segment_length_km
    ) |>
    select(Model, Rule, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo_3km <- removed_by_topo |>
    mutate(Rule = cutoff_rule_labels[["topo_3km"]])
  
  bind_rows(
    retained_topo_only,
    removed_by_topo_only,
    retained_after_3km,
    removed_by_topo_3km
  ) |>
    mutate(
      model_layer = model_layer,
      Model = factor(Model, levels = unname(model_2017_projection_labels)),
      Rule = factor(Rule, levels = unname(cutoff_rule_labels)),
      patch_map_class = factor(
        patch_map_class,
        levels = c(
          "Retained suitable habitat",
          "Removed by 3 km minimum",
          "Removed by topographic/barrier thresholds"
        )
      )
    )
}

patch_comparison_2017_2080_layers <- bind_rows(
  lapply(names(model_2017_projection_labels), function(model_layer) {
    make_2017_cutoff_comparison_layers(
      model_layers_bob[[model_layer]],
      model_layer,
      model_2017_projection_labels[[model_layer]]
    )
  })
)

patch_comparison_2017_2080_context_layers <- bind_rows(
  lapply(names(model_2017_projection_labels), function(model_layer) {
    bind_rows(
      lapply(unname(cutoff_rule_labels), function(rule_label) {
        model_layers_bob[[model_layer]] |>
          st_make_valid() |>
          mutate(
            Model = factor(
              model_2017_projection_labels[[model_layer]],
              levels = unname(model_2017_projection_labels)
            ),
            Rule = factor(rule_label, levels = unname(cutoff_rule_labels)),
            model_layer = model_layer
          )
      })
    )
  })
)

patch_comparison_2017_2080_summary <- patch_comparison_2017_2080_layers |>
  st_drop_geometry() |>
  group_by(Model, Rule, patch_map_class) |>
  summarise(
    n_patch_segments = n(),
    mapped_length_km = round(sum(patch_length_km, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(patch_comparison_2017_2080_summary, n = Inf, width = Inf)

## letters for 2 panels, a b -------------------------
panel_label_data <- tidyr::expand_grid(
  Model = factor(
    unname(model_2017_projection_labels),
    levels = unname(model_2017_projection_labels)
  ),
  Rule = factor(
    unname(cutoff_rule_labels),
    levels = unname(cutoff_rule_labels)
  )
) |>
  mutate(
    panel_label = c("a", "b")
  )

label_x <- st_bbox(huc12_bob)[["xmin"]]
label_y <- st_bbox(huc12_bob)[["ymax"]]
## ------------------------------------------------------

### Map a. Includes legend item for 3km cutoff patches  --------
patch_comparison_2017_2080_map <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = patch_comparison_2017_2080_context_layers,
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.38
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_2080_layers,
      patch_map_class == "Removed by topographic/barrier thresholds"
    ),
    aes(color = patch_map_class),
    linewidth = 0.65,
    alpha = 0.70
  ) +
  geom_sf(
    data = filter(patch_comparison_2017_2080_layers, patch_map_class == "Removed by 3 km minimum"),
    aes(color = patch_map_class),
    linewidth = 0.80,
    alpha = 0.85
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_2080_layers,
      patch_map_class == "Retained suitable habitat"
    ),
    aes(color = patch_map_class),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  geom_sf(
    data = flowlines_bob,
    color = "gray70",
    linewidth = 0.25,
    alpha = 0.70
  ) +
  geom_sf(
    data = highlight_huc12_bob,
    fill = "#D9F0D3",
    color = NA,
    alpha = 0.75
  ) +
  geom_sf(
    data = barriers_bob,
    shape = 4,
    color = "darkred",
    size = 2.8,
    stroke = 1.2
  ) +
  scale_color_manual(
    values = c(
      "Removed by topographic/barrier thresholds" = "#D55E00",
      "Removed by 3 km minimum" = "darkred",
      "Retained suitable habitat" = "#0072B2"
    ),
    breaks = c(
      "Removed by topographic/barrier thresholds",
      "Removed by 3 km minimum",
      "Retained suitable habitat"
    ),
    labels = c(
      "Removed by topographic thresholds",
      "Removed by 3 km minimum",
      "Retained suitable habitat"
    ),
    name = NULL,
    #guide = "none"
    guide = guide_legend(
      nrow = 3,
      byrow = TRUE
    )
  ) +
  #guides(color = "none") +  ## comment out to bring back legend
  facet_grid(rows = vars(Model), cols = vars(Rule), switch = "y") +
  coord_sf(expand = FALSE) +
  geom_text(
    data = panel_label_data,
    aes(x = label_x, y = label_y, label = panel_label),
    inherit.aes = FALSE,
    hjust = -0.35,
    vjust = 1.4,
    fontface = "bold",
    size = 5
  ) +
  labs(
    title = "Projected suitable habitat for 2080",
    #subtitle = "Blue = retained habitat; orange = patches removed by topographic/barrier thresholds",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    #legend.position = "none",
    legend.position = "bottom",
    legend.box = "vertical",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", angle = 90)
  )


print(patch_comparison_2017_2080_map)

### Map b. without the 3km eliminations highlighted -------------
patch_comparison_2017_2080_map_no_red <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = highlight_huc12_bob,
    fill = "#D9F0D3",
    color = NA,
    alpha = 0.75
  ) +
  geom_sf(
    data = patch_comparison_2017_2080_context_layers,
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.38
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_2080_layers,
      patch_map_class %in% c(
        "Removed by topographic/barrier thresholds",
        "Removed by 3 km minimum"
      )
    ),
    aes(color = "Suitable habitat removed"),
    linewidth = 0.65,
    alpha = 0.70
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_2080_layers,
      patch_map_class == "Retained suitable habitat"
    ),
    aes(color = patch_map_class),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  geom_sf(
    data = flowlines_bob,
    color = "gray70",
    linewidth = 0.25,
    alpha = 0.70
  ) +
  geom_sf(
    data = barriers_bob,
    shape = 4,
    color = "darkred",
    size = 2.8,
    stroke = 1.2
  ) +
  scale_color_manual(
    values = c(
      "Suitable habitat removed" = "#D55E00",
      "Retained suitable habitat" = "#0072B2"
    ),
    breaks = c(
      "Suitable habitat removed",
      "Retained suitable habitat"
    ),
    labels = c(
      "Suitable habitat removed",
      "Retained suitable habitat"
    ),
    name = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.2)
    )
  ) +
  facet_grid(rows = vars(Model), cols = vars(Rule), switch = "y") +
  coord_sf(expand = FALSE) +
  geom_text(
    data = panel_label_data,
    aes(x = label_x, y = label_y, label = panel_label),
    inherit.aes = FALSE,
    hjust = -0.35,
    vjust = 1.4,
    fontface = "bold",
    size = 5
  ) +
  labs(
    title = "Projected suitable habitat for 2080",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", angle = 90)
  )

print(patch_comparison_2017_2080_map_no_red)

#### 2080 retained patch lengths in Figure S1 highlighted HUC12s ---------------
## After topographic/barrier thresholds + 3 km minimum length
library(dplyr)
library(sf)
library(tidyr)

stopifnot(
  exists("model_layers_bob"),
  exists("huc12_bob"),
  "ext17" %in% names(model_layers_bob)
)

if (!exists("cutoff_buffer_sf")) {
  stopifnot(exists("cutoff_buffer_10m_barrier_adjusted_analysis"))
  cutoff_buffer_sf <- st_sf(
    geometry = st_sfc(
      cutoff_buffer_10m_barrier_adjusted_analysis,
      crs = st_crs(huc12_bob)
    )
  )
}

highlight_huc12_terms <- c(
  "Gorge Creek",
  "Spotted Bear",
  "Wall Creek",
  "Upper Gordon Creek",
  "Upper Big Salmon Creek",
  "Little Salmon Creek",
  "Babcock Creek"
)

find_huc12_name_column <- function(huc_sf, target_names) {
  target_names_clean <- tolower(trimws(target_names))
  attribute_cols <- names(st_drop_geometry(huc_sf))
  
  matched_cols <- vapply(
    attribute_cols,
    function(col) {
      values <- tolower(trimws(as.character(huc_sf[[col]])))
      any(values %in% target_names_clean, na.rm = TRUE) ||
        any(grepl(paste(target_names, collapse = "|"), values, ignore.case = TRUE))
    },
    logical(1)
  )
  
  if (!any(matched_cols)) {
    stop("Could not find the HUC12 name column. Check names(huc12_bob).")
  }
  
  attribute_cols[which(matched_cols)[1]]
}

huc12_name_col <- find_huc12_name_column(huc12_bob, highlight_huc12_terms)

highlight_huc12_bob <- huc12_bob |>
  mutate(huc12_name = as.character(.data[[huc12_name_col]])) |>
  filter(
    grepl(
      paste(highlight_huc12_terms, collapse = "|"),
      huc12_name,
      ignore.case = TRUE
    )
  ) |>
  st_make_valid()

stopifnot(nrow(highlight_huc12_bob) > 0)

patch_id_col <- c("Patch_ID", "PATCH_ID", "patch_id", "PATCHID")[
  c("Patch_ID", "PATCH_ID", "patch_id", "PATCHID") %in% names(model_layers_bob[["ext17"]])
][1]

model_2080 <- model_layers_bob[["ext17"]] |>
  st_make_valid() |>
  mutate(
    patch_row_id = row_number(),
    patch_id = if (!is.na(patch_id_col)) {
      as.character(.data[[patch_id_col]])
    } else {
      as.character(patch_row_id)
    }
  )

cutoff_buffer_2080 <- cutoff_buffer_sf |>
  st_transform(st_crs(model_2080)) |>
  st_make_valid()

highlight_huc12_2080 <- highlight_huc12_bob |>
  st_transform(st_crs(model_2080)) |>
  st_make_valid()

# Apply topo/barrier thresholds
retained_2080_topo_segments <- suppressWarnings(
  st_intersection(model_2080, cutoff_buffer_2080)
) |>
  filter(!st_is_empty(geometry)) |>
  st_collection_extract("LINESTRING", warn = FALSE) |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    retained_segment_length_km = as.numeric(st_length(geometry)) / 1000
  )

# Apply 3 km minimum using total retained length per original patch
retained_2080_patch_lengths <- retained_2080_topo_segments |>
  st_drop_geometry() |>
  group_by(patch_row_id, patch_id) |>
  summarise(
    total_retained_patch_length_km = sum(retained_segment_length_km, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(total_retained_patch_length_km >= 3)

retained_2080_patches_final <- retained_2080_topo_segments |>
  semi_join(retained_2080_patch_lengths, by = c("patch_row_id", "patch_id")) |>
  group_by(patch_row_id, patch_id) |>
  summarise(
    geometry = st_union(geometry),
    .groups = "drop"
  ) |>
  left_join(
    retained_2080_patch_lengths,
    by = c("patch_row_id", "patch_id")
  )

# Simple table: retained 2080 patch lengths within Figure S1 highlighted HUC12s
retained_2080_highlight_huc12_table <- suppressWarnings(
  st_intersection(
    retained_2080_patches_final,
    highlight_huc12_2080 |> select(huc12_name)
  )
) |>
  filter(!st_is_empty(geometry)) |>
  st_collection_extract("LINESTRING", warn = FALSE) |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    retained_length_in_huc12_km = as.numeric(st_length(geometry)) / 1000
  ) |>
  st_drop_geometry() |>
  group_by(
    huc12_name,
    patch_row_id,
    patch_id,
    total_retained_patch_length_km
  ) |>
  summarise(
    retained_length_in_huc12_km = sum(retained_length_in_huc12_km, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    total_retained_patch_length_km = round(total_retained_patch_length_km, 2),
    retained_length_in_huc12_km = round(retained_length_in_huc12_km, 2)
  ) |>
  arrange(huc12_name, desc(retained_length_in_huc12_km), patch_row_id)

print(retained_2080_highlight_huc12_table, n = Inf, width = Inf)
# -----------------------------------------------------------------------------


####### Figure S1 **2015 MODEL: 2080 ONLY** ############################
##  USING THE 2015 MODEL!!! ##################################
stopifnot(
  exists("model_layers_bob"),
  exists("huc12_bob"),
  all(c("ext17") %in% names(model_layers_bob))
)

library(patchwork)

if (!exists("cutoff_buffer_sf")) {
  stopifnot(exists("cutoff_buffer_10m_barrier_adjusted_analysis"))
  cutoff_buffer_sf <- st_sf(
    geometry = st_sfc(cutoff_buffer_10m_barrier_adjusted_analysis, crs = st_crs(huc12_bob))
  )
}

model_2017_projection_labels <- c(
  ext17 = "2080 projection"
)

cutoff_rule_labels <- c(
  topo_only = "Topographic thresholds",
  topo_3km = "Topographic thresholds\n+ 3 km minimum length"
)

## HUC12s to highlight  -------------------------------------------------------
highlight_huc12_terms <- c(
  "Gorge Creek",
  "Spotted Bear",
  #"Silvertip Creek,"
  #"Upper Gordon Creek",
  "Wall Creek"
)

huc12_attribute_text <- huc12_bob |>
  st_drop_geometry() |>
  mutate(
    huc12_search_text = apply(
      across(everything(), as.character),
      1,
      paste,
      collapse = " | "
    )
  ) |>
  select(huc12_search_text)

highlight_huc12_bob <- huc12_bob |>
  bind_cols(huc12_attribute_text) |>
  filter(
    grepl(
      paste(highlight_huc12_terms, collapse = "|"),
      huc12_search_text,
      ignore.case = TRUE
    )
  ) |>
  st_make_valid()

stopifnot(nrow(highlight_huc12_bob) > 0)

highlight_huc12_bob |>
  st_drop_geometry() |>
  select(huc12_search_text) |>
  tibble::as_tibble() |>
  print(n = Inf, width = Inf)

## Flowlines and barriers to add to maps ---------------------------------------
stopifnot(
  exists("streams_map"),
  inherits(streams_map, "sf")
)

flowlines_bob <- streams_map |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(st_crs(huc12_bob)) |>
  st_make_valid()

## Use whichever barrier point object exists in the workspace
barrier_object_candidates <- c(
  "barriers_sf",
  "barrier_points",
  "barrier_points_sf",
  "passage_barriers",
  "barriers_map"
)

barrier_object_name <- barrier_object_candidates[
  barrier_object_candidates %in% ls()
][1]

stopifnot(!is.na(barrier_object_name))

barriers_bob <- get(barrier_object_name) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_transform(st_crs(huc12_bob)) |>
  st_make_valid()
## ----------------------------------------------------------------------------
## --- Make cutoff comparison layers --------
make_2017_cutoff_comparison_layers <- function(model_sf, model_layer, model_label) {
  model_sf <- model_sf |>
    st_make_valid() |>
    mutate(
      patch_row_id = row_number(),
      Model = model_label
    )
  
  retained_topo <- suppressWarnings(
    st_intersection(model_sf, cutoff_buffer_sf)
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      retained_segment_length_km = as.numeric(st_length(geometry)) / 1000
    )
  
  retained_status <- retained_topo |>
    st_drop_geometry() |>
    group_by(patch_row_id) |>
    summarise(
      retained_patch_length_km = sum(retained_segment_length_km, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(retained_after_3km_minimum = retained_patch_length_km >= 3)
  
  retained_topo_only <- retained_topo |>
    mutate(
      Rule = cutoff_rule_labels[["topo_only"]],
      patch_map_class = "Retained suitable habitat",
      patch_length_km = retained_segment_length_km
    ) |>
    select(Model, Rule, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo <- suppressWarnings(
    st_difference(model_sf, st_union(cutoff_buffer_sf))
  ) |>
    filter(!st_is_empty(geometry)) |>
    st_collection_extract("LINESTRING", warn = FALSE) |>
    filter(!st_is_empty(geometry)) |>
    mutate(
      patch_map_class = "Removed by topographic/barrier thresholds",
      patch_length_km = as.numeric(st_length(geometry)) / 1000
    ) |>
    select(Model, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo_only <- removed_by_topo |>
    mutate(Rule = cutoff_rule_labels[["topo_only"]])
  
  retained_after_3km <- retained_topo |>
    left_join(retained_status, by = "patch_row_id") |>
    mutate(
      Rule = cutoff_rule_labels[["topo_3km"]],
      patch_map_class = if_else(
        retained_after_3km_minimum,
        "Retained suitable habitat",
        "Removed by 3 km minimum"
      ),
      patch_length_km = retained_segment_length_km
    ) |>
    select(Model, Rule, patch_row_id, patch_map_class, patch_length_km)
  
  removed_by_topo_3km <- removed_by_topo |>
    mutate(Rule = cutoff_rule_labels[["topo_3km"]])
  
  bind_rows(
    retained_topo_only,
    removed_by_topo_only,
    retained_after_3km,
    removed_by_topo_3km
  ) |>
    mutate(
      model_layer = model_layer,
      Model = factor(Model, levels = unname(model_2017_projection_labels)),
      Rule = factor(Rule, levels = unname(cutoff_rule_labels)),
      patch_map_class = factor(
        patch_map_class,
        levels = c(
          "Retained suitable habitat",
          "Removed by 3 km minimum",
          "Removed by topographic/barrier thresholds"
        )
      )
    )
}

patch_comparison_2017_2080_layers <- bind_rows(
  lapply(names(model_2017_projection_labels), function(model_layer) {
    make_2017_cutoff_comparison_layers(
      model_layers_bob[[model_layer]],
      model_layer,
      model_2017_projection_labels[[model_layer]]
    )
  })
)

patch_comparison_2017_2080_context_layers <- bind_rows(
  lapply(names(model_2017_projection_labels), function(model_layer) {
    bind_rows(
      lapply(unname(cutoff_rule_labels), function(rule_label) {
        model_layers_bob[[model_layer]] |>
          st_make_valid() |>
          mutate(
            Model = factor(
              model_2017_projection_labels[[model_layer]],
              levels = unname(model_2017_projection_labels)
            ),
            Rule = factor(rule_label, levels = unname(cutoff_rule_labels)),
            model_layer = model_layer
          )
      })
    )
  })
)

patch_comparison_2017_2080_summary <- patch_comparison_2017_2080_layers |>
  st_drop_geometry() |>
  group_by(Model, Rule, patch_map_class) |>
  summarise(
    n_patch_segments = n(),
    mapped_length_km = round(sum(patch_length_km, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(patch_comparison_2017_2080_summary, n = Inf, width = Inf)

## Freeze Figure S1 highlighted watersheds for map and inset 
figure_s1_highlight_huc12_bob <- highlight_huc12_bob

## Zoom extent around highlighted HUC12s ---------------------------------------
highlight_bbox <- st_bbox(figure_s1_highlight_huc12_bob)

x_pad <- as.numeric(highlight_bbox[["xmax"]] - highlight_bbox[["xmin"]]) * 0.18
y_pad <- as.numeric(highlight_bbox[["ymax"]] - highlight_bbox[["ymin"]]) * 0.18

zoom_xlim <- c(
  highlight_bbox[["xmin"]] - x_pad,
  highlight_bbox[["xmax"]] + x_pad
)

zoom_ylim <- c(
  highlight_bbox[["ymin"]] - y_pad,
  highlight_bbox[["ymax"]] + y_pad
)

zoom_extent_sf <- st_as_sfc(
  st_bbox(
    c(
      xmin = zoom_xlim[1],
      xmax = zoom_xlim[2],
      ymin = zoom_ylim[1],
      ymax = zoom_ylim[2]
    ),
    crs = st_crs(huc12_bob)
  )
) |>
  st_sf()
## ----------------------------------------------------------------------------
## letters for stacked map panels -------------------------
panel_label_data <- tibble::tibble(
  Rule = factor(
    unname(cutoff_rule_labels),
    levels = unname(cutoff_rule_labels)
  ),
  panel_label = c("a", "b"),
  x = zoom_xlim[1],
  y = zoom_ylim[2]
)
## ------------------------------------------------------
### Figure S1 cropped map -------------
patch_comparison_2017_2080_map_no_red <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = figure_s1_highlight_huc12_bob,
    fill = "#D9F0D3",
    color = NA,
    alpha = 0.75
  ) +
  geom_sf(
    data = flowlines_bob,
    color = "gray70",
    linewidth = 0.25,
    alpha = 0.70
  ) +
  geom_sf(
    data = patch_comparison_2017_2080_context_layers,
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.38
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_2080_layers,
      patch_map_class %in% c(
        "Removed by topographic/barrier thresholds",
        "Removed by 3 km minimum"
      )
    ),
    aes(color = "Suitable habitat removed"),
    linewidth = 0.65,
    alpha = 0.70
  ) +
  geom_sf(
    data = filter(
      patch_comparison_2017_2080_layers,
      patch_map_class == "Retained suitable habitat"
    ),
    aes(color = patch_map_class),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  geom_sf(
    data = barriers_bob,
    shape = 4,
    color = "darkred",
    size = 2.8,
    stroke = 1.2
  ) +
  scale_color_manual(
    values = c(
      "Suitable habitat removed" = "#D55E00",
      "Retained suitable habitat" = "#0072B2"
    ),
    breaks = c(
      "Suitable habitat removed",
      "Retained suitable habitat"
    ),
    labels = c(
      "Suitable habitat removed",
      "Retained suitable habitat"
    ),
    name = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.2)
    )
  ) +
  facet_grid(rows = vars(Rule), switch = "y") +
  coord_sf(
    xlim = zoom_xlim,
    ylim = zoom_ylim,
    expand = FALSE
  ) +
  geom_text(
    data = panel_label_data,
    aes(x = x, y = y, label = panel_label),
    inherit.aes = FALSE,
    hjust = -0.35,
    vjust = 1.4,
    fontface = "bold",
    size = 5
  ) +
  labs(
    title = "Projected future suitable habitat",
    subtitle = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5),
    strip.text = element_text(face = "bold"),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.y.left = element_text(face = "bold", angle = 90)
  )

print(patch_comparison_2017_2080_map_no_red)

## Inset map showing zoom area within the Bob ----------------------------------
figure_s1_inset_overview <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.18
  ) +
  geom_sf(
    data = figure_s1_highlight_huc12_bob,
    fill = "#D9F0D3",
    color = NA,
    alpha = 0.85
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.18
  ) +
  geom_sf(
    data = zoom_extent_sf,
    fill = NA,
    color = "black",
    linewidth = 0.55
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "black", linewidth = 0.35),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(3, 3, 3, 3)
  )

print(figure_s1_inset_overview)
# -----------------------------------------------------------------------------

###############################################################################
## Figure S1 version: 2080 topographic constraints only + inset
###############################################################################
figure_s1_2080_topo_only_layers <- patch_comparison_2017_2080_layers |>
  filter(
    Model == "2080 projection",
    Rule == cutoff_rule_labels[["topo_only"]]
  )

figure_s1_2080_topo_only_context <- patch_comparison_2017_2080_context_layers |>
  filter(
    Model == "2080 projection",
    Rule == cutoff_rule_labels[["topo_only"]]
  )

figure_s1_2080_topo_only_map <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = figure_s1_highlight_huc12_bob,
    fill = "#D9F0D3",
    color = NA,
    alpha = 0.75
  ) +
  geom_sf(
    data = flowlines_bob,
    color = "gray70",
    linewidth = 0.25,
    alpha = 0.70
  ) +
  geom_sf(
    data = figure_s1_2080_topo_only_context,
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.38
  ) +
  geom_sf(
    data = filter(
      figure_s1_2080_topo_only_layers,
      patch_map_class == "Removed by topographic/barrier thresholds"
    ),
    aes(color = "Suitable habitat removed"),
    linewidth = 0.65,
    alpha = 0.70
  ) +
  geom_sf(
    data = filter(
      figure_s1_2080_topo_only_layers,
      patch_map_class == "Retained suitable habitat"
    ),
    aes(color = "Retained suitable habitat"),
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  geom_sf(
    data = barriers_bob,
    shape = 4,
    color = "darkred",
    size = 2.8,
    stroke = 1.2
  ) +
  scale_color_manual(
    values = c(
      "Suitable habitat removed" = "#D55E00",
      "Retained suitable habitat" = "#0072B2"
    ),
    breaks = c(
      "Suitable habitat removed",
      "Retained suitable habitat"
    ),
    name = NULL
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.2)
    )
  ) +
  coord_sf(
    xlim = zoom_xlim,
    ylim = zoom_ylim,
    expand = FALSE
  ) +
  labs(
    title = "Projected future suitable habitat",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

figure_s1_2080_topo_only_with_inset <-
  figure_s1_2080_topo_only_map +
  inset_element(
    figure_s1_inset_overview,
    left = 0.02,
    bottom = 0.62,
    right = 0.34,
    top = 0.98,
    align_to = "plot"
  )

print(figure_s1_2080_topo_only_with_inset)
# -----------------------------------------------------------------------------



################################################################################
#######  ===   EVALUATING FIRE  =========== ####################################
################################################################################

## Read in fire shapefiles. ----------------------------------------
library(sf)
library(dplyr)
library(tibble)

fire_atlas_path <- file.path(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project",
  "ArcGIS/BMWC_Fire_Data/fire_atlas/fire_atlas_Jul2026/fire_perimeters_cce_1972_2022.shp"
)

cannon_2023_path <- file.path(
  "C:/Users/leahs/OneDrive/Documents/U_of_M/Masters_Project",
  "ArcGIS/BMWC_Fire_Data/fire_atlas/Cannon_2023_perimeter_extracted",
  "mt4771611356020220807_20220801_20230722_burn_bndy.shp"
)

stopifnot(
  file.exists(fire_atlas_path),
  file.exists(cannon_2023_path),
  exists("analysis_crs")
)

fire_atlas <- st_read(fire_atlas_path, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_make_valid() |>
  st_transform(analysis_crs) |>
  mutate(fire_source = "fire_atlas")

cannon_2023_fire <- st_read(cannon_2023_path, quiet = TRUE) |>
  st_zm(drop = TRUE, what = "ZM") |>
  st_make_valid() |>
  st_transform(analysis_crs) |>
  mutate(fire_source = "cannon_2023")
# -------------------------------------------------------------

## Combine/dissolve, align layers -----------------------------
## Combined fire layer for analysis.
fire_polygons <- bind_rows(
  fire_atlas,
  cannon_2023_fire
) |>
  st_make_valid()

## Dissolved fire footprint (one single polygon)
fire_footprint_bob <- fire_polygons |>
  st_union() |>
  st_make_valid()

## QA: confirm spatial alignment and feature counts.
fire_layer_summary <- tibble(
  layer = c("fire_atlas_bob", "cannon_2023_fire_bob", "fire_polygons_bob"),
  n_features = c(nrow(fire_atlas), nrow(cannon_2023_fire), nrow(fire_polygons)),
  crs_epsg = c(
    st_crs(fire_atlas)$epsg,
    st_crs(cannon_2023_fire)$epsg,
    st_crs(fire_polygons)$epsg
  ),
  geometry_types = c(
    paste(unique(st_geometry_type(fire_atlas)), collapse = ", "),
    paste(unique(st_geometry_type(cannon_2023_fire)), collapse = ", "),
    paste(unique(st_geometry_type(fire_polygons)), collapse = ", ")
  )
)

print(fire_layer_summary, n = Inf)
# --------------------------------------------------------------

## 2040 suitable habitat burned before cutoffs ---------------------------------
stopifnot(
  exists("model_layers_bob"),
  "mod17" %in% names(model_layers_bob),
  exists("fire_polygons")
)

# Dissolve all fire polygons so overlapping fires are not double-counted.
fire_footprint_bob <- fire_polygons |>
  st_make_valid() |>
  st_union() |>
  st_make_valid()

# 2040 suitable habitat before topographic cutoffs or 3 km rule.
habitat_2040_original <- model_layers_bob[["mod17"]] |>
  st_make_valid() |>
  st_transform(st_crs(fire_polygons))

# Basic intersection between original 2040 habitat and dissolved fire footprint.
habitat_2040_burned <- suppressWarnings(
  st_intersection(
    habitat_2040_original,
    st_sf(geometry = st_sfc(fire_footprint_bob, crs = st_crs(habitat_2040_original)))
  )
) |>
  filter(!st_is_empty(geometry)) |>
  st_collection_extract("LINESTRING", warn = FALSE) |>
  filter(!st_is_empty(geometry)) |>
  mutate(burned_length_km = as.numeric(st_length(geometry)) / 1000)

habitat_2040_burned_summary <- habitat_2040_burned |>
  st_drop_geometry() |>
  summarise(
    burned_habitat_km = round(sum(burned_length_km, na.rm = TRUE), 2),
    n_burned_segments = n()
  )

print(habitat_2040_burned_summary)
# -----------------------------------------------------------------------

## Simple map of burned 2040 suitable habitat ----------------------------------
stopifnot(
  exists("huc12_bob"),
  exists("habitat_2040_original"),
  exists("habitat_2040_burned"),
  exists("fire_polygons")
)

map_crs <- st_crs(huc12_bob)

fire_polygons_bob_map <- suppressWarnings(
  st_intersection(
    fire_polygons |> st_transform(map_crs) |> st_make_valid(),
    huc12_bob |> st_union() |> st_as_sf() |> st_make_valid()
  )
) |>
  filter(!st_is_empty(geometry)) |>
  st_collection_extract("POLYGON", warn = FALSE)

habitat_2040_original_map <- habitat_2040_original |>
  st_transform(map_crs) |>
  st_make_valid()

habitat_2040_burned_map_layer <- habitat_2040_burned |>
  st_transform(map_crs) |>
  st_make_valid()

habitat_2040_burned_map <- ggplot() +
  geom_sf(
    data = huc12_bob,
    fill = "#F5F5F2",
    color = "white",
    linewidth = 0.25
  ) +
  geom_sf(
    data = fire_polygons_bob_map,
    fill = "#F4A582",
    color = NA,
    alpha = 0.35
  ) +
  geom_sf(
    data = habitat_2040_original_map,
    color = "#8F99A8",
    linewidth = 0.35,
    alpha = 0.55
  ) +
  geom_sf(
    data = habitat_2040_burned_map_layer,
    color = "red3",
    linewidth = 1.05,
    alpha = 0.95
  ) +
  geom_sf(
    data = huc12_bob,
    fill = NA,
    color = "#4D4D4D",
    linewidth = 0.25
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "2040 suitable habitat intersecting fire polygons",
    subtitle = "HUC12 backdrop; gray = original 2040 habitat; red = burned habitat.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "#D8D8D8", linewidth = 0.18),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "italic", hjust = 0.5)
  )

print(habitat_2040_burned_map)
# -----------------------------------------------------------------------

## Number of patches that burned? (2040 projection, 2015 model) ---------
## Count unique patch IDs contributing to burned 2040 habitat 
patch_id_col_2040 <- find_patch_id_column(habitat_2040_burned)

burned_2040_unique_patch_ids <- habitat_2040_burned |>
  st_drop_geometry() |>
  mutate(
    burned_patch_id = if (!is.na(patch_id_col_2040)) {
      as.character(.data[[patch_id_col_2040]])
    } else {
      as.character(patch_row_id)
    }
  ) |>
  distinct(burned_patch_id)

burned_2040_unique_patch_count <- burned_2040_unique_patch_ids |>
  summarise(n_unique_burned_patches = n())

print(burned_2040_unique_patch_count)
print(burned_2040_unique_patch_ids)
############################################################################


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
