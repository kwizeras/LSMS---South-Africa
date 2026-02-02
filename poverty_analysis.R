# =============================================================================
# MEASURING POVERTY IN SOUTH AFRICA: AN ANALYSIS USING LSMS HOUSEHOLD SURVEY DATA
# =============================================================================
# Author: Sotaire Kwizera
# Date: February 2026
# 
# Description:
#   This script analyzes poverty rates in South Africa using the World Bank's
#   Living Standards Measurement Study (LSMS) household survey data. It computes
#   multiple poverty measures following the Foster-Greer-Thorbecke (FGT) class
#   of poverty indices and examines disparities across demographic groups.
#
# Data Source:
#   World Bank LSMS - South Africa
#   https://microdata.worldbank.org/index.php/catalog/297
#
# Methodology:
#   The FGT class of poverty indices (Foster, Greer & Thorbecke, 1984):
#     P_α = (1/n) × Σ[(z - y_i)/z]^α
#   Where:
#     - P0 (α=0): Headcount ratio - proportion below poverty line
#     - P1 (α=1): Poverty gap - average depth of poverty
#     - P2 (α=2): Poverty severity - squared gap, sensitive to inequality among poor
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. SETUP AND CONFIGURATION
# -----------------------------------------------------------------------------

# Load required packages
library(tidyverse)  # Data manipulation and visualization
library(haven)      # Reading Stata .dta files
library(scales)     # Formatting numbers in plots

# Set working directory (modify path as needed)
# setwd("path/to/data/directory")

# Define poverty line (150 Rand per person per month)
POVERTY_LINE <- 150

# Set theme for all plots
theme_set(theme_minimal(base_size = 12))


# -----------------------------------------------------------------------------
# 2. DATA LOADING
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("LOADING LSMS DATA FILES\n")
cat(strrep("=", 60), "\n\n")

# Load household income data
HH_income <- read_dta("HHINCTL.dta")
cat("✓ Loaded household income data:", nrow(HH_income), "households\n")

# Load household roster (individual-level demographics)
HH_roster <- read_dta("M8_HROST.dta")
cat("✓ Loaded household roster:", nrow(HH_roster), "individuals\n")

# Load stratification variables (race, location)
HH_strata <- read_dta("STRATA2.dta")
cat("✓ Loaded stratification data:", nrow(HH_strata), "observations\n")


# -----------------------------------------------------------------------------
# 3. DATA PREPARATION
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("PREPARING ANALYSIS DATASET\n")
cat(strrep("=", 60), "\n\n")

# Calculate household size from roster
household_size <- HH_roster %>%
  group_by(hhid) %>%
  summarise(
    hh_size = n(),
    n_children = sum(age < 16, na.rm = TRUE),
    n_adults = sum(age >= 16, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate adult equivalent size
# Using equivalence scale where children (age < 16) count as 0.5 adults
# This accounts for lower consumption needs of children
adult_equivalents <- HH_roster %>%
  mutate(adult_equiv_weight = ifelse(age >= 16, 1, 0.5)) %>%
  group_by(hhid) %>%
  summarise(
    adult_equiv_size = sum(adult_equiv_weight, na.rm = TRUE),
    .groups = "drop"
  )

# Merge all data sources
poverty_data <- HH_income %>%
  select(hhid, totminc) %>%
  left_join(household_size, by = "hhid") %>%
  left_join(adult_equivalents, by = "hhid") %>%
  left_join(
    HH_strata %>% select(hhid, race, metro),
    by = "hhid"
  ) %>%
  filter(!is.na(totminc), !is.na(hh_size))

# Compute welfare measures and poverty indicators
poverty_data <- poverty_data %>%
  mutate(
    # Per capita income (simple division)
    income_per_capita = totminc / hh_size,
    
    # Adult-equivalent income (adjusted for household composition)
    income_adult_equiv = totminc / adult_equiv_size,
    
    # Binary poverty indicators (1 = poor, 0 = not poor)
    is_poor_pc = as.integer(income_per_capita < POVERTY_LINE),
    is_poor_ae = as.integer(income_adult_equiv < POVERTY_LINE),
    
    # Poverty gaps (normalized distance below poverty line, 0 for non-poor)
    gap_pc = pmax(0, (POVERTY_LINE - income_per_capita) / POVERTY_LINE),
    gap_ae = pmax(0, (POVERTY_LINE - income_adult_equiv) / POVERTY_LINE),
    
    # Add descriptive labels
    race_label = factor(
      race,
      levels = 1:5,
      labels = c("African", "Coloured", "Indian", "White", "Other")
    ),
    location_label = factor(
      metro,
      levels = 1:3,
      labels = c("Rural", "Urban", "Metropolitan")
    )
  )

cat("✓ Final dataset:", nrow(poverty_data), "households with complete data\n")


# -----------------------------------------------------------------------------
# 4. FGT POVERTY MEASURES - NATIONAL LEVEL
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("FGT POVERTY MEASURES\n")
cat(strrep("=", 60), "\n\n")

# Function to compute FGT indices
compute_fgt <- function(data, is_poor_col, gap_col) {
  data %>%
    summarise(
      P0_headcount = mean({{ is_poor_col }}) * 100,
      P1_gap = mean({{ gap_col }}) * 100,
      P2_severity = mean({{ gap_col }}^2) * 100,
      n = n()
    )
}

# Per capita income measures
fgt_pc <- compute_fgt(poverty_data, is_poor_pc, gap_pc)
cat("FGT Measures (Per Capita Income):\n")
cat(sprintf("  P0 (Headcount):  %.2f%%\n", fgt_pc$P0_headcount))
cat(sprintf("  P1 (Poverty Gap): %.2f%%\n", fgt_pc$P1_gap))
cat(sprintf("  P2 (Severity):    %.2f%%\n", fgt_pc$P2_severity))
cat(sprintf("  Sample size:      %s households\n", format(fgt_pc$n, big.mark = ",")))

# Adult equivalent income measures
fgt_ae <- compute_fgt(poverty_data, is_poor_ae, gap_ae)
cat("\nFGT Measures (Adult Equivalent Income):\n")
cat(sprintf("  P0 (Headcount):  %.2f%%\n", fgt_ae$P0_headcount))
cat(sprintf("  P1 (Poverty Gap): %.2f%%\n", fgt_ae$P1_gap))
cat(sprintf("  P2 (Severity):    %.2f%%\n", fgt_ae$P2_severity))
cat(sprintf("  Sample size:      %s households\n", format(fgt_ae$n, big.mark = ",")))


# -----------------------------------------------------------------------------
# 5. DESCRIPTIVE STATISTICS
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("DESCRIPTIVE STATISTICS\n")
cat(strrep("=", 60), "\n\n")

# Income distribution statistics
income_stats <- poverty_data %>%
  summarise(
    mean = mean(income_per_capita),
    sd = sd(income_per_capita),
    median = median(income_per_capita),
    p25 = quantile(income_per_capita, 0.25),
    p75 = quantile(income_per_capita, 0.75),
    min = min(income_per_capita),
    max = max(income_per_capita)
  )

cat("Per Capita Income Distribution (Rand/month):\n")
cat(sprintf("  Mean:            %.2f\n", income_stats$mean))
cat(sprintf("  Std. Dev:        %.2f\n", income_stats$sd))
cat(sprintf("  Median:          %.2f\n", income_stats$median))
cat(sprintf("  25th Percentile: %.2f\n", income_stats$p25))
cat(sprintf("  75th Percentile: %.2f\n", income_stats$p75))

# Household size statistics
size_stats <- poverty_data %>%
  summarise(
    mean = mean(hh_size),
    sd = sd(hh_size),
    median = median(hh_size),
    p25 = quantile(hh_size, 0.25),
    p75 = quantile(hh_size, 0.75)
  )

cat("\nHousehold Size Distribution:\n")
cat(sprintf("  Mean:   %.2f persons\n", size_stats$mean))
cat(sprintf("  Median: %.0f persons\n", size_stats$median))


# -----------------------------------------------------------------------------
# 6. DISAGGREGATED ANALYSIS - POVERTY BY RACE
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("POVERTY BY RACE (Adult Equivalent Income)\n")
cat(strrep("=", 60), "\n\n")

poverty_by_race <- poverty_data %>%
  filter(!is.na(race_label)) %>%
  group_by(race_label) %>%
  summarise(
    poverty_rate = mean(is_poor_ae) * 100,
    poverty_gap = mean(gap_ae) * 100,
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(poverty_rate))

# Print results
for (i in 1:nrow(poverty_by_race)) {
  cat(sprintf("  %-10s: %.2f%% poverty rate (n = %s)\n",
              poverty_by_race$race_label[i],
              poverty_by_race$poverty_rate[i],
              format(poverty_by_race$n[i], big.mark = ",")))
}


# -----------------------------------------------------------------------------
# 7. DISAGGREGATED ANALYSIS - POVERTY BY LOCATION
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("POVERTY BY LOCATION (Adult Equivalent Income)\n")
cat(strrep("=", 60), "\n\n")

poverty_by_location <- poverty_data %>%
  filter(!is.na(location_label)) %>%
  group_by(location_label) %>%
  summarise(
    poverty_rate = mean(is_poor_ae) * 100,
    poverty_gap = mean(gap_ae) * 100,
    n = n(),
    .groups = "drop"
  )

# Print results
for (i in 1:nrow(poverty_by_location)) {
  cat(sprintf("  %-15s: %.2f%% poverty rate (n = %s)\n",
              poverty_by_location$location_label[i],
              poverty_by_location$poverty_rate[i],
              format(poverty_by_location$n[i], big.mark = ",")))
}


# -----------------------------------------------------------------------------
# 8. VISUALIZATIONS
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("GENERATING VISUALIZATIONS\n")
cat(strrep("=", 60), "\n\n")

# Figure 1: FGT Measures Comparison
comparison_data <- tibble(
  measure = rep(c("P0 (Headcount)", "P1 (Gap)", "P2 (Severity)"), 2),
  method = rep(c("Per Capita", "Adult Equivalent"), each = 3),
  value = c(
    fgt_pc$P0_headcount, fgt_pc$P1_gap, fgt_pc$P2_severity,
    fgt_ae$P0_headcount, fgt_ae$P1_gap, fgt_ae$P2_severity
  )
)

p1 <- ggplot(comparison_data, aes(x = measure, y = value, fill = method)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%", value)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_manual(values = c("Per Capita" = "#2C3E50", "Adult Equivalent" = "#E74C3C")) +
  labs(
    title = "FGT Poverty Measures: Per Capita vs. Adult Equivalent",
    subtitle = "Poverty line: 150 Rand/person/month",
    x = NULL,
    y = "Percentage (%)",
    fill = "Welfare Measure"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "top",
    panel.grid.major.x = element_blank()
  ) +
  ylim(0, max(comparison_data$value) * 1.15)

print(p1)
cat("✓ Generated Figure 1: FGT Measures Comparison\n")


# Figure 2: Income Distribution
p2 <- ggplot(poverty_data, aes(x = income_per_capita)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 50,
    fill = "#3498DB",
    color = "white",
    alpha = 0.8
  ) +
  geom_density(color = "#E74C3C", linewidth = 1) +
  geom_vline(
    xintercept = POVERTY_LINE,
    linetype = "dashed",
    color = "#C0392B",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = POVERTY_LINE + 50,
    y = Inf,
    label = paste("Poverty Line =", POVERTY_LINE, "Rand"),
    hjust = 0,
    vjust = 2,
    color = "#C0392B",
    fontface = "bold"
  ) +
  scale_x_continuous(
    limits = c(0, quantile(poverty_data$income_per_capita, 0.95)),
    labels = comma
  ) +
  labs(
    title = "Distribution of Per Capita Income",
    subtitle = "Truncated at 95th percentile for visibility",
    x = "Per Capita Monthly Income (Rand)",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(p2)
cat("✓ Generated Figure 2: Income Distribution\n")


# Figure 3: Poverty by Race
p3 <- ggplot(poverty_by_race, aes(x = reorder(race_label, poverty_rate), y = poverty_rate)) +
  geom_col(fill = "#2C3E50", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%", poverty_rate)),
    hjust = -0.2,
    size = 4
  ) +
  geom_hline(
    yintercept = fgt_ae$P0_headcount,
    linetype = "dashed",
    color = "#E74C3C"
  ) +
  coord_flip() +
  labs(
    title = "Poverty Headcount by Race",
    subtitle = "Dashed line shows national average",
    x = NULL,
    y = "Poverty Rate (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p3)
cat("✓ Generated Figure 3: Poverty by Race\n")


# Figure 4: Poverty by Location
p4 <- ggplot(poverty_by_location, aes(x = location_label, y = poverty_rate)) +
  geom_col(aes(fill = location_label), width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.1f%%", poverty_rate)),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(values = c("Rural" = "#27AE60", "Urban" = "#3498DB", "Metropolitan" = "#9B59B6")) +
  labs(
    title = "Poverty Headcount by Location Type",
    x = NULL,
    y = "Poverty Rate (%)"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(p4)
cat("✓ Generated Figure 4: Poverty by Location\n")


# Figure 5: Lorenz Curve
lorenz_data <- poverty_data %>%
  arrange(income_per_capita) %>%
  mutate(
    cum_pop = row_number() / n(),
    cum_income = cumsum(income_per_capita) / sum(income_per_capita)
  )

# Calculate Gini coefficient
gini <- 1 - 2 * sum(lorenz_data$cum_income) / nrow(lorenz_data)

p5 <- ggplot(lorenz_data, aes(x = cum_pop, y = cum_income)) +
  geom_line(color = "#2C3E50", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = cum_income, ymax = cum_pop), fill = "#E74C3C", alpha = 0.2) +
  annotate(
    "text",
    x = 0.7,
    y = 0.3,
    label = paste("Gini =", round(gini, 3)),
    size = 5,
    fontface = "bold"
  ) +
  labs(
    title = "Lorenz Curve: Income Inequality",
    x = "Cumulative Share of Population",
    y = "Cumulative Share of Income"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +
  coord_fixed()

print(p5)
cat("✓ Generated Figure 5: Lorenz Curve\n")
cat(sprintf("  Gini Coefficient: %.3f\n", gini))

