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
}

# Load Libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

# === 2. Read Data ===

data <- readr::read_csv(
  file = data_file,
  na = c("NA", "", " ", "999.9", "999.99", "99"),
  guess_max = 1000,
  show_col_types = FALSE
)

year_col <- "Year"
month_col <- "Month"
temp_col <- "Temperature"

required_cols <- c(year_col, month_col, temp_col)
if (!all(required_cols %in% names(data))) {
  stop(paste("Error: Required columns not found:", paste(required_cols[!required_cols %in% names(data)], collapse=", ")))
}

# === 3. Data Preparation ===

data <- data %>%
  mutate(across(all_of(required_cols), ~ suppressWarnings(as.numeric(.)))) %>%
  mutate(Temperature_C = .data[[temp_col]])

missing_val_filter_threshold <- 999 
target_start_year <- 1961
target_end_year <- 2000

clean_data <- data %>%
  filter(!is.na(Temperature_C)) %>%
  filter(Temperature_C < missing_val_filter_threshold) %>%
  filter(!is.na(.data[[year_col]]) & 
           .data[[year_col]] >= target_start_year & 
           .data[[year_col]] <= target_end_year)

if (nrow(clean_data) == 0) {
  stop("Stopping: No data remaining after filtering.")
}

clean_data <- clean_data %>%
  mutate(Month = factor(.data[[month_col]], levels = 1:12, labels = month.abb))

# === 4. Calculate Monthly Statistics ===

monthly_stats_all <- clean_data %>%
  group_by(Month) %>%
  summarise(
    mean_all_1961_2000 = mean(Temperature_C, na.rm = TRUE),
    median_all_1961_2000 = median(Temperature_C, na.rm = TRUE),
    sd_all_1961_2000 = sd(Temperature_C, na.rm = TRUE),
    .groups = "drop"
  )

monthly_stats_early <- clean_data %>%
  filter(.data[[year_col]] >= 1961 & .data[[year_col]] <= 1980) %>%
  group_by(Month) %>%
  summarise(
    mean_early_1961_1980 = mean(Temperature_C, na.rm = TRUE),
    median_early_1961_1980 = median(Temperature_C, na.rm = TRUE),
    .groups = "drop"
  )

monthly_stats_late <- clean_data %>%
  filter(.data[[year_col]] >= 1981 & .data[[year_col]] <= 2000) %>%
  group_by(Month) %>%
  summarise(
    mean_late_1981_2000 = mean(Temperature_C, na.rm = TRUE),
    median_late_1981_2000 = median(Temperature_C, na.rm = TRUE),
    .groups = "drop"
  )

# === 5. Generate Summary Table ===

if (nrow(monthly_stats_all) > 0) {
  final_summary_table <- monthly_stats_all %>%
    left_join(monthly_stats_early, by = "Month") %>%
    left_join(monthly_stats_late, by = "Month") %>%
    arrange(match(Month, levels(clean_data$Month))) %>%
    select(Month,
           mean_all_1961_2000, median_all_1961_2000,
           mean_early_1961_1980, median_early_1961_1980,
           mean_late_1981_2000, median_late_1981_2000,
           sd_all_1961_2000)
  
  table_filename <- file.path(output_dir, "monthly_temperature_summary_table.csv")
  write.csv(final_summary_table, table_filename, row.names = FALSE)
} else {
  final_summary_table <- tibble()
  table_filename <- file.path(output_dir, "_table_skipped.csv")
}

# === 6. Generate Plots ===

if (nrow(final_summary_table) > 0) {
  plot_dir <- output_dir
  plot_caption <- "Station: Tompo | Source: National Snow and Ice Data Center (NSIDC)"
  
  # Plot A
  plot_A <- ggplot(final_summary_table, aes(x = Month, y = mean_all_1961_2000)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    geom_errorbar(aes(ymin = mean_all_1961_2000 - sd_all_1961_2000,
                      ymax = mean_all_1961_2000 + sd_all_1961_2000),
                  width = 0.25, color = "darkgrey") +
    scale_y_continuous(n.breaks = 10) +
    labs(
      title = "Plot A: Monthly Mean Air Temperature (1961–2000)",
      subtitle = "Error bars represent ±1 Standard Deviation",
      x = "Month",
      y = "Mean Air Temperature (°C)",
      caption = plot_caption
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.caption = element_text(hjust = 0, face = "italic"))
  
  ggsave(file.path(plot_dir, "plot_A_mean_with_sd_1961_2000.png"), plot = plot_A, width = 8, height = 6)
  
  # Plot B
  plot_B_data <- final_summary_table %>%
    select(Month, mean_all_1961_2000, median_all_1961_2000) %>%
    pivot_longer(cols = -Month, names_to = "Statistic", values_to = "Temperature") %>%
    mutate(Statistic = recode(Statistic, "mean_all_1961_2000" = "Mean", "median_all_1961_2000" = "Median"))
  
  plot_B <- ggplot(plot_B_data, aes(x = Month, y = Temperature, fill = Statistic)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual(values = c("Mean" = "coral", "Median" = "lightgreen")) +
    scale_y_continuous(n.breaks = 10) +
    labs(
      title = "Plot B: Monthly Mean vs. Median Air Temperature (1961–2000)",
      x = "Month",
      y = "Air Temperature (°C)",
      fill = "Statistic",
      caption = plot_caption
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          plot.caption = element_text(hjust = 0, face = "italic"))
  
  ggsave(file.path(plot_dir, "plot_B_mean_vs_median_1961_2000.png"), plot = plot_B, width = 8, height = 6)
  
  # Plot C
  plot_C_data <- final_summary_table %>%
    select(Month, mean_early_1961_1980, mean_late_1981_2000) %>%
    pivot_longer(cols = -Month, names_to = "Period", values_to = "Mean_Temperature") %>%
    mutate(Period = recode(Period, "mean_early_1961_1980" = "1961-1980", "mean_late_1981_2000" = "1981-2000"))
  
  plot_C <- ggplot(plot_C_data, aes(x = Month, y = Mean_Temperature, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    scale_fill_manual(values = c("1961-1980" = "lightblue", "1981-2000" = "salmon")) +
    scale_y_continuous(n.breaks = 10) +
    labs(
      title = "Plot C: Comparison of Monthly Mean Air Temperature",
      subtitle = "Early Period (1961–1980) vs. Late Period (1981–2000)",
      x = "Month",
      y = "Mean Air Temperature (°C)",
      fill = "Time Period",
      caption = plot_caption
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          plot.caption = element_text(hjust = 0, face = "italic"))
  
  ggsave(file.path(plot_dir, "plot_C_early_vs_late_means.png"), plot = plot_C, width = 8, height = 6)
  
} else {
  plot_A_filename <- file.path(output_dir,"_skipped_A.png")
  plot_B_filename <- file.path(output_dir,"_skipped_B.png")
  plot_C_filename <- file.path(output_dir,"_skipped_C.png")
}

# === 7. Statistical Test ===

if (exists("final_summary_table") && nrow(final_summary_table) == 12 &&
    all(c("mean_early_1961_1980", "mean_late_1981_2000") %in% names(final_summary_table)) &&
    !any(is.na(final_summary_table$mean_early_1961_1980)) &&
    !any(is.na(final_summary_table$mean_late_1981_2000))) {
  
  early_means <- final_summary_table$mean_early_1961_1980
  late_means <- final_summary_table$mean_late_1981_2000
  alpha <- 0.05
  
  mw_test_result <- wilcox.test(late_means, early_means, alternative = "two.sided", exact = FALSE)
  paired_t_test_result <- t.test(late_means, early_means, paired = TRUE, alternative = "two.sided")
}

