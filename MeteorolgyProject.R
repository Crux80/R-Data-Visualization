# ==============================================================================
# R Project: Analyzing Monthly Air Temperature Changes
# Author: Xavier R.
# Date: 2025-05-01
#
# Description: Reads cleaned CSV data, calculates monthly temperature stats
# (1961-2000), compares early (1961-80) vs. late (1981-2000) periods,
# generates plot, and performs statistical tests.
# ==============================================================================

# === 1. Setup ===

# File Paths
data_file <- "C:/Users/xrcru/OneDrive/Documents/Spring25/R Analysis/Final Project/Data/uni.tompo.24671.csv"
output_dir <- "C:/Users/xrcru/OneDrive/Documents/Spring25/R Analysis/Final Project/Output"

# Create Output Directory if needed
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Output directory created at: ", output_dir)
} else {
  message("Output directory already exists: ", output_dir)
}

# Load Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# === 2. Read Data ===

message("Reading cleaned CSV file: ", data_file)
data <- readr::read_csv(
  file = data_file,
  na = c("NA", "", " ", "999.9", "999.99", "99"), # Define potential NA values
  guess_max = 1000,
  show_col_types = FALSE
)

message(paste("Read", nrow(data), "rows and", ncol(data), "columns."))
message("Column names: ", paste(names(data), collapse=", "))

# Define primary column names (assuming headers match these exactly)
year_col <- "Year"
month_col <- "Month"
temp_col <- "Temperature" # Using the column containing °C values

# Verify required columns exist
required_cols <- c(year_col, month_col, temp_col)
if (!all(required_cols %in% names(data))) {
  stop(paste("Error: Required columns not found:", paste(required_cols[!required_cols %in% names(data)], collapse=", ")))
}

# === 3. Data Preparation ===

# Ensure correct data types (Year, Month, Temperature should be numeric)
message("Ensuring essential columns are numeric...")
data <- data %>%
  mutate(
    across(all_of(required_cols), ~ suppressWarnings(as.numeric(.)))
  )

# Check Year range
year_range <- range(data[[year_col]], na.rm = TRUE)
message(paste0("Data Year range found: ", year_range[1], " to ", year_range[2]))

# Use Temperature column directly as Temperature_C
data <- data %>%
  mutate(Temperature_C = .data[[temp_col]])

message("Filtering data...")
# Filter for valid temperature, missing codes, and target year range
missing_val_filter_threshold <- 999 
target_start_year <- 1961
target_end_year <- 2000

clean_data <- data %>%
  filter(!is.na(Temperature_C)) %>%
  filter(Temperature_C < missing_val_filter_threshold) %>%
  filter(!is.na(.data[[year_col]]) &
           .data[[year_col]] >= target_start_year &
           .data[[year_col]] <= target_end_year)

message(paste("Rows remaining after filtering:", nrow(clean_data)))

# Stop if no data remains
if (nrow(clean_data) == 0) {
  stop("Stopping: No data remaining after filtering. Check Year range or missing value filter.")
}

# Convert Month to factor
clean_data <- clean_data %>%
  mutate(Month = factor(.data[[month_col]], levels = 1:12, labels = month.abb))

# === 4. Calculate Monthly Statistics ===

message("Calculating monthly statistics...")
# Overall (1961-2000)
monthly_stats_all <- clean_data %>%
  group_by(Month) %>%
  summarise(
    mean_all_1961_2000 = mean(Temperature_C, na.rm = TRUE),
    median_all_1961_2000 = median(Temperature_C, na.rm = TRUE),
    sd_all_1961_2000 = sd(Temperature_C, na.rm = TRUE),
    .groups = "drop"
  )

# Early Period (1961-1980)
monthly_stats_early <- clean_data %>%
  filter(.data[[year_col]] >= 1961 & .data[[year_col]] <= 1980) %>%
  group_by(Month) %>%
  summarise(
    mean_early_1961_1980 = mean(Temperature_C, na.rm = TRUE),
    median_early_1961_1980 = median(Temperature_C, na.rm = TRUE),
    .groups = "drop"
  )

# Late Period (1981-2000)
monthly_stats_late <- clean_data %>%
  filter(.data[[year_col]] >= 1981 & .data[[year_col]] <= 2000) %>%
  group_by(Month) %>%
  summarise(
    mean_late_1981_2000 = mean(Temperature_C, na.rm = TRUE),
    median_late_1981_2000 = median(Temperature_C, na.rm = TRUE),
    .groups = "drop"
  )

# === 5. Generate Summary Table (Deliverable 2) ===

if (nrow(monthly_stats_all) > 0) {
  final_summary_table <- monthly_stats_all %>%
    left_join(monthly_stats_early, by = "Month") %>%
    left_join(monthly_stats_late, by = "Month") %>%
    arrange(match(Month, levels(clean_data$Month))) %>%
    
    select(Month,
           mean_all_1961_2000, median_all_1961_2000,
           mean_early_1961_1980, median_early_1961_1980,
           mean_late_1981_2000, median_late_1981_2000,
           sd_all_1961_2000 # Keep SD for Plot A
    )
  
  print("Final Summary Table:")
  print(final_summary_table)
  table_filename <- file.path(output_dir, "monthly_temperature_summary_table.csv")
  write.csv(final_summary_table, table_filename, row.names = FALSE)
  message("Summary table saved to: ", table_filename)
} else {
  message("Skipping summary table generation/saving as no monthly stats were calculated.")
  final_summary_table <- tibble() # Ensure it exists but is empty
  table_filename <- file.path(output_dir, "_table_skipped.csv")
}

# === 6. Generate Plots (Deliverable 3) ===

if (nrow(final_summary_table) > 0) {
  message("Generating plots...")
  plot_dir <- output_dir # Save plots in the main output directory
  
  # Define common caption text
  plot_caption <- "Station: Tompo | Source: National Snow and Ice Data Center (NSIDC)"
  
  # Plot A: Mean Temp with SD (1961–2000)
  plot_A <- ggplot(final_summary_table, aes(x = Month, y = mean_all_1961_2000)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    geom_errorbar(aes(ymin = mean_all_1961_2000 - sd_all_1961_2000,
                      ymax = mean_all_1961_2000 + sd_all_1961_2000),
                  width = 0.25, color = "darkgrey") +
    scale_y_continuous(n.breaks = 10) + # Added for better y-axis readability
    labs(
      title = "Plot A: Monthly Mean Air Temperature (1961–2000)",
      subtitle = "Error bars represent ±1 Standard Deviation",
      x = "Month",
      y = "Mean Air Temperature (°C)",
      caption = plot_caption  # <<< ADDED CAPTION
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.caption = element_text(hjust = 0, face = "italic")) # Align caption left
  
  plot_A_filename <- file.path(plot_dir, "plot_A_mean_with_sd_1961_2000.png")
  ggsave(plot_A_filename, plot = plot_A, width = 8, height = 6); message("Plot A saved.")
  
  # Plot B: Mean vs Median Temp (1961–2000)
  plot_B_data <- final_summary_table %>%
    select(Month, mean_all_1961_2000, median_all_1961_2000) %>%
    pivot_longer(cols = -Month, names_to = "Statistic", values_to = "Temperature") %>%
    mutate(Statistic = recode(Statistic, "mean_all_1961_2000" = "Mean", "median_all_1961_2000" = "Median"))
  
  plot_B <- ggplot(plot_B_data, aes(x = Month, y = Temperature, fill = Statistic)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual(values = c("Mean" = "coral", "Median" = "lightgreen")) +
    scale_y_continuous(n.breaks = 10) + # Added for better y-axis readability
    labs(
      title = "Plot B: Monthly Mean vs. Median Air Temperature (1961–2000)",
      x = "Month",
      y = "Air Temperature (°C)",
      fill = "Statistic",
      caption = plot_caption # <<< ADDED CAPTION
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          plot.caption = element_text(hjust = 0, face = "italic")) # Align caption left
  
  plot_B_filename <- file.path(plot_dir, "plot_B_mean_vs_median_1961_2000.png")
  ggsave(plot_B_filename, plot = plot_B, width = 8, height = 6); message("Plot B saved.")
  
  # Plot C: Mean Temp Comparison (Early vs. Late)
  plot_C_data <- final_summary_table %>%
    select(Month, mean_early_1961_1980, mean_late_1981_2000) %>%
    pivot_longer(cols = -Month, names_to = "Period", values_to = "Mean_Temperature") %>%
    mutate(Period = recode(Period, "mean_early_1961_1980" = "1961-1980", "mean_late_1981_2000" = "1981-2000"))
  
  plot_C <- ggplot(plot_C_data, aes(x = Month, y = Mean_Temperature, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual(values = c("1961-1980" = "lightblue", "1981-2000" = "salmon")) +
    scale_y_continuous(n.breaks = 10) + # Added for better y-axis readability
    labs(
      title = "Plot C: Comparison of Monthly Mean Air Temperature",
      subtitle = "Early Period (1961–1980) vs. Late Period (1981–2000)",
      x = "Month",
      y = "Mean Air Temperature (°C)",
      fill = "Time Period",
      caption = plot_caption # <<< ADDED CAPTION
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          plot.caption = element_text(hjust = 0, face = "italic")) # Align caption left
  
  plot_C_filename <- file.path(plot_dir, "plot_C_early_vs_late_means.png")
  ggsave(plot_C_filename, plot = plot_C, width = 8, height = 6); message("Plot C saved.")
  
} else {
  message("Skipping Plot generation as final_summary_table is empty.")
  plot_A_filename <- file.path(output_dir,"_skipped_A.png")
  plot_B_filename <- file.path(output_dir,"_skipped_B.png")
  plot_C_filename <- file.path(output_dir,"_skipped_C.png")
}


# === 7. Statistical Test (Deliverable 4) ===

# Check if we have exactly 12 months of valid data for both periods
if (exists("final_summary_table") && nrow(final_summary_table) == 12 &&
    all(c("mean_early_1961_1980", "mean_late_1981_2000") %in% names(final_summary_table)) &&
    !any(is.na(final_summary_table$mean_early_1961_1980)) &&
    !any(is.na(final_summary_table$mean_late_1981_2000))) {
  
  message("Performing statistical tests...")
  early_means <- final_summary_table$mean_early_1961_1980
  late_means <- final_summary_table$mean_late_1981_2000
  alpha <- 0.05 # Significance level
  
  # Mann-Whitney U test (non-parametric comparison of distributions)
  mw_test_result <- wilcox.test(late_means, early_means, alternative = "two.sided", exact = FALSE)
  print("--- Mann-Whitney U Test Results ---")
  print(mw_test_result)
  message("\n--- Interpretation Guidance (Mann-Whitney U) ---")
  message("H0: Distributions of monthly means are the same for both periods.")
  message(paste0("alpha = ", alpha, ", p-value = ", round(mw_test_result$p.value, 4)))
  if (mw_test_result$p.value < alpha) { message("Conclusion: REJECT H0 (Significant difference).") } else { message("Conclusion: FAIL TO REJECT H0 (No significant difference).") }
  message("-----------------------------------------------------")
  
  # Paired t-test (parametric comparison of mean difference across paired months)
  message("\nPerforming alternative test (Paired t-test)...")
  paired_t_test_result <- t.test(late_means, early_means, paired = TRUE, alternative = "two.sided")
  print("--- Paired t-test Results ---")
  print(paired_t_test_result)
  message("\n--- Interpretation Guidance (Paired t-test) ---")
  message("H0: Mean difference between paired months (Late - Early) is zero.")
  message(paste0("alpha = ", alpha, ", p-value = ", round(paired_t_test_result$p.value, 4)))
  if (paired_t_test_result$p.value < alpha) {
    message("Conclusion: REJECT H0 (Significant mean difference).")
    message(paste0("Mean difference (Late - Early): ", round(paired_t_test_result$estimate, 3), " °C"))
  } else { message("Conclusion: FAIL TO REJECT H0 (No significant mean difference).") }
  message("---------------------------------------------------")
  
} else {
  message("\nSkipping statistical tests: Conditions not met (e.g., final table empty or incomplete).")
  if(exists("final_summary_table")) { print(final_summary_table)} # Show table if tests skipped
}

# === 8. Script Completion Message ===
message("\n=====================================")
message("Analysis script completed.")
message("Review console output for messages, warnings, and results.")
message("Deliverables (if successful) saved in: ", output_dir)
message("=====================================")
