# ============================================================
# FCE Monte Carlo Simulation
# Step 4: Board-Ready Visualizations
# Author: Brian Connolly
# Date: 2026
# ============================================================

library(tidyverse)
library(scales)
library(gridExtra)

# FCE color palette — matches existing dashboard
fce_green  <- "#2D6A4F"
fce_mid    <- "#52B788"
fce_light  <- "#95D5B2"
fce_orange <- "#F4A261"
fce_red    <- "#C1121F"
fce_dark   <- "#1B4332"

# Shared theme for all charts
theme_fce_board <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face  = "bold",
                                      size  = 15,
                                      color = fce_dark),
      plot.subtitle    = element_text(size  = 11,
                                      color = "gray40"),
      plot.caption     = element_text(size  = 9,
                                      color = "gray50"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      axis.title       = element_text(size  = 11,
                                      color = "gray30"),
      legend.position  = "bottom",
      plot.background  = element_rect(fill  = "white",
                                      color = NA),
      plot.margin      = margin(15, 15, 15, 15)
    )
}

# ============================================================
# Chart 1 — Loss Distribution Histogram
# ============================================================

# What this shows:
# The full distribution of 10,000 simulated portfolio losses
# The shape tells the Board how concentrated or spread out
# the risk is — a fat right tail means extreme loss scenarios
# are more likely than a normal distribution would suggest
#
# Key markers:
# Dark green dashed = Current CECL reserve
# Green solid       = Expected loss (mean)
# Red solid         = 95th percentile (VaR)

el_mean      <- mean(sim_results$total_loss)
var_75       <- quantile(sim_results$total_loss, 0.75)
var_95       <- quantile(sim_results$total_loss, 0.95)
var_99       <- quantile(sim_results$total_loss, 0.99)
cecl_reserve <- 8100000

# Pre-calculate 95th percentile cutoff BEFORE ggplot
# Required because after_stat() cannot reference
# original dataframe columns — only computed stat values
var_95_cutoff <- quantile(sim_results$total_loss / 1e6, 0.95)

chart1 <- ggplot(sim_results,
                 aes(x = total_loss / 1e6)) +
  geom_histogram(
    aes(fill = after_stat(x) > var_95_cutoff),
    bins      = 80,
    color     = "white",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = c("FALSE" = fce_light,
               "TRUE"  = fce_red),
    guide  = "none"
  ) +
  
  # Current CECL reserve line
  geom_vline(xintercept = cecl_reserve / 1e6,
             color      = fce_dark,
             linetype   = "dashed",
             linewidth  = 1.2) +
  
  # Expected loss line
  geom_vline(xintercept = el_mean / 1e6,
             color      = fce_green,
             linetype   = "solid",
             linewidth  = 1.2) +
  
  # 95th percentile VaR line
  geom_vline(xintercept = var_95 / 1e6,
             color      = fce_red,
             linetype   = "solid",
             linewidth  = 1.2) +
  
  # Current reserve label
  annotate("text",
           x        = (cecl_reserve / 1e6) - 0.8,
           y        = 500,
           label    = paste0("Current Reserve\n$",
                             round(cecl_reserve / 1e6, 1), "M"),
           color    = fce_dark,
           size     = 3.5,
           hjust    = 1,
           fontface = "bold") +
  
  # Expected loss label
  annotate("text",
           x        = (el_mean / 1e6) + 0.8,
           y        = 500,
           label    = paste0("Expected Loss\n$",
                             round(el_mean / 1e6, 1), "M"),
           color    = fce_green,
           size     = 3.5,
           hjust    = 0,
           fontface = "bold") +
  
  # VaR label
  annotate("text",
           x        = (var_95 / 1e6) + 0.8,
           y        = 500,
           label    = paste0("95th Pct VaR\n$",
                             round(var_95 / 1e6, 1), "M"),
           color    = fce_red,
           size     = 3.5,
           hjust    = 0,
           fontface = "bold") +
  
  scale_x_continuous(
    labels = dollar_format(suffix = "M", accuracy = 1)
  ) +
  scale_y_continuous(
    labels = comma_format()
  ) +
  labs(
    title    = "Portfolio Loss Distribution — 10,000 Monte Carlo Scenarios",
    subtitle = paste0("Red shading = worst 5% of scenarios | ",
                      "Current reserve covers ",
                      percent(mean(sim_results$total_loss <=
                                     cecl_reserve),
                              accuracy = 0.1),
                      " of simulations"),
    x        = "Total Portfolio Loss ($M)",
    y        = "Number of Scenarios",
    caption  = "Source: FCE Monte Carlo Simulation | FRED macro inputs | Logistic Regression PD model"
  ) +
  theme_fce_board()

print(chart1)

# ============================================================
# Chart 2 — Reserve Adequacy by Confidence Level
# ============================================================

# What this shows:
# How much capital FCE needs at each confidence level
# The gap between the bar and the dashed line
# is the capital shortfall at each threshold
#
# This is your Board recommendation chart —
# it directly answers "how much should we reserve?"

reserve_levels <- tibble(
  percentile = c(50, 60, 70, 75, 80,
                 85, 90, 95, 99),
  loss       = quantile(sim_results$total_loss,
                        c(0.50, 0.60, 0.70, 0.75,
                          0.80, 0.85, 0.90, 0.95, 0.99))
) %>%
  mutate(
    label     = paste0(percentile, "th"),
    shortfall = pmax(loss - cecl_reserve, 0),
    adequate  = loss <= cecl_reserve
  )

chart2 <- ggplot(reserve_levels,
                 aes(x    = reorder(label, percentile),
                     y    = loss / 1e6,
                     fill = adequate)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = cecl_reserve / 1e6,
             color      = fce_dark,
             linetype   = "dashed",
             linewidth  = 1.2) +
  geom_text(aes(label = dollar(loss / 1e6,
                               accuracy = 0.1,
                               suffix   = "M")),
            vjust    = -0.5,
            size     = 3.5,
            fontface = "bold") +
  annotate("text",
           x        = 1,
           y        = (cecl_reserve / 1e6) + 0.5,
           label    = paste0("Current CECL Reserve: $",
                             round(cecl_reserve / 1e6, 1), "M"),
           hjust    = 0,
           size     = 3.5,
           color    = fce_dark,
           fontface = "bold") +
  scale_fill_manual(
    values = c("TRUE"  = fce_green,
               "FALSE" = fce_red),
    labels = c("TRUE"  = "Reserve Adequate",
               "FALSE" = "Reserve Insufficient"),
    name   = NULL
  ) +
  scale_y_continuous(
    labels = dollar_format(suffix = "M", accuracy = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Required Capital Reserve by Confidence Level",
    subtitle = "Green = current reserve sufficient | Red = additional capital required",
    x        = "Loss Percentile",
    y        = "Required Reserve ($M)",
    caption  = "Source: FCE Monte Carlo Simulation | 10,000 scenarios"
  ) +
  theme_fce_board() +
  theme(legend.position = "top")

print(chart2)

# ============================================================
# Chart 3 — Segment Loss Distribution (Box Plot)
# ============================================================

# What this shows:
# The range of losses for each segment across all simulations
# Box = middle 50% of outcomes (25th to 75th percentile)
# Line = median loss
# Whiskers = 5th to 95th percentile range
# Dots = extreme outlier scenarios
#
# Wide boxes = high uncertainty in that segment
# Narrow boxes = more predictable segment losses
# This directly identifies which segments need most attention

segment_losses_long <- sim_results %>%
  select(sim_id,
         Dairy           = dairy_loss,
         `Row Crop`      = row_crop_loss,
         Livestock       = livestock_loss,
         `Agri-Business` = agribiz_loss) %>%
  pivot_longer(-sim_id,
               names_to  = "segment",
               values_to = "loss")

chart3 <- ggplot(
  segment_losses_long,
  aes(x    = reorder(segment, loss, median),
      y    = loss / 1e6,
      fill = segment)
) +
  geom_boxplot(
    outlier.shape = 16,
    outlier.size  = 0.5,
    outlier.alpha = 0.3,
    width         = 0.6
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Dairy"         = fce_green,
      "Row Crop"      = fce_orange,
      "Livestock"     = fce_mid,
      "Agri-Business" = fce_light
    ),
    guide = "none"
  ) +
  scale_y_continuous(
    labels = dollar_format(suffix = "M", accuracy = 0.1)
  ) +
  labs(
    title    = "Segment Loss Distribution — Monte Carlo Scenarios",
    subtitle = "Box = 25th–75th percentile | Line = median | Whiskers = 5th–95th percentile",
    x        = NULL,
    y        = "Segment Loss ($M)",
    caption  = "Source: FCE Monte Carlo Simulation | 10,000 scenarios"
  ) +
  theme_fce_board()

print(chart3)

# ============================================================
# Chart 4 — Macro Scenario vs Loss Scatter
# ============================================================

# What this shows:
# The relationship between each macro variable
# and total portfolio loss across all simulations
# Steeper slope = that variable drives losses more
# This answers "what macro condition should worry us most?"

scatter_rate <- ggplot(
  sim_results,
  aes(x = rate_scenario,
      y = total_loss / 1e6)
) +
  geom_point(alpha = 0.05,
             color = fce_green,
             size  = 0.8) +
  geom_smooth(method = "lm",
              color  = fce_red,
              se     = TRUE) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    labels = dollar_format(suffix = "M", accuracy = 1)
  ) +
  labs(
    title    = "Interest Rate vs Portfolio Loss",
    subtitle = "Each dot = one simulated scenario",
    x        = "Fed Funds Rate (%)",
    y        = "Total Loss ($M)"
  ) +
  theme_fce_board()

scatter_corn <- ggplot(
  sim_results,
  aes(x = corn_scenario * 100,
      y = total_loss / 1e6)
) +
  geom_point(alpha = 0.05,
             color = fce_orange,
             size  = 0.8) +
  geom_smooth(method = "lm",
              color  = fce_red,
              se     = TRUE) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    labels = dollar_format(suffix = "M", accuracy = 1)
  ) +
  labs(
    title    = "Corn Price Change vs Portfolio Loss",
    subtitle = "Negative = price decline | Positive = price increase",
    x        = "Monthly Corn Price Change (%)",
    y        = "Total Loss ($M)"
  ) +
  theme_fce_board()

scatter_milk <- ggplot(
  sim_results,
  aes(x = milk_scenario * 100,
      y = total_loss / 1e6)
) +
  geom_point(alpha = 0.05,
             color = "#6B4C9A",
             size  = 0.8) +
  geom_smooth(method = "lm",
              color  = fce_red,
              se     = TRUE) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(
    labels = dollar_format(suffix = "M", accuracy = 1)
  ) +
  labs(
    title    = "Milk Price Change vs Portfolio Loss",
    subtitle = "Dairy segment drives largest absolute losses",
    x        = "Monthly Milk Price Change (%)",
    y        = "Total Loss ($M)"
  ) +
  theme_fce_board()

print(scatter_rate)
print(scatter_corn)
print(scatter_milk)

# ============================================================
# Chart 5 — Convergence Check
# ============================================================

# What this shows:
# How the expected loss estimate stabilizes
# as we run more simulations
# If the line flattens by simulation 5,000-10,000
# it means 10,000 simulations was enough
# A line that keeps moving means we need more simulations
# This is called convergence and validates our simulation size

convergence <- sim_results %>%
  mutate(
    cumulative_mean = cummean(total_loss)
  )

chart5 <- ggplot(convergence,
                 aes(x = sim_id,
                     y = cumulative_mean / 1e6)) +
  geom_line(color     = fce_green,
            linewidth = 1.0) +
  geom_hline(yintercept = mean(sim_results$total_loss) / 1e6,
             color      = fce_red,
             linetype   = "dashed",
             linewidth  = 0.8) +
  scale_x_continuous(labels = comma_format()) +
  scale_y_continuous(
    labels = dollar_format(suffix = "M", accuracy = 0.1)
  ) +
  labs(
    title    = "Monte Carlo Convergence Check",
    subtitle = "Expected loss estimate stabilizes — confirms 10,000 simulations is sufficient",
    x        = "Number of Simulations",
    y        = "Cumulative Mean Loss ($M)",
    caption  = "Dashed line = final expected loss estimate"
  ) +
  theme_fce_board()

print(chart5)

# ============================================================
# Executive Summary Table
# ============================================================

cat("\n")
cat("╔══════════════════════════════════════════════╗\n")
cat("║     FCE PORTFOLIO RISK — EXECUTIVE SUMMARY   ║\n")
cat("║         Monte Carlo Simulation Results       ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

cat("LOSS DISTRIBUTION (10,000 Scenarios)\n")
cat("─────────────────────────────────────\n")
cat(sprintf("%-35s %s\n", "Expected Loss (Mean):",
            dollar(mean(sim_results$total_loss))))
cat(sprintf("%-35s %s\n", "Median Loss (50th Pct):",
            dollar(median(sim_results$total_loss))))
cat(sprintf("%-35s %s\n", "Elevated Loss (75th Pct):",
            dollar(quantile(sim_results$total_loss, 0.75))))
cat(sprintf("%-35s %s\n", "Value at Risk (95th Pct):",
            dollar(quantile(sim_results$total_loss, 0.95))))
cat(sprintf("%-35s %s\n", "Severe Stress (99th Pct):",
            dollar(quantile(sim_results$total_loss, 0.99))))
cat(sprintf("%-35s %s\n", "Expected Shortfall (CVaR):",
            dollar(mean(sim_results$total_loss[
              sim_results$total_loss >=
                quantile(sim_results$total_loss, 0.95)
            ]))))

cat("\nCAPITAL ADEQUACY\n")
cat("─────────────────────────────────────\n")
cat(sprintf("%-35s %s\n",
            "Current CECL Reserve:", "$8,100,000"))
cat(sprintf("%-35s %s\n",
            "Reserve Covers Expected Loss:",
            ifelse(mean(sim_results$total_loss) <= 8100000,
                   "YES", "NO")))
cat(sprintf("%-35s %s\n",
            "Probability Reserve Sufficient:",
            percent(mean(sim_results$total_loss <= 8100000),
                    accuracy = 0.1)))
cat(sprintf("%-35s %s\n",
            "Additional Capital at 95th Pct:",
            dollar(pmax(quantile(sim_results$total_loss,
                                 0.95) - 8100000, 0))))
cat(sprintf("%-35s %s\n",
            "Additional Capital at 99th Pct:",
            dollar(pmax(quantile(sim_results$total_loss,
                                 0.99) - 8100000, 0))))

cat("\nSEGMENT RISK CONCENTRATION\n")
cat("─────────────────────────────────────\n")
cat(sprintf("%-35s %s\n", "Dairy Avg Loss:",
            dollar(mean(sim_results$dairy_loss))))
cat(sprintf("%-35s %s\n", "Row Crop Avg Loss:",
            dollar(mean(sim_results$row_crop_loss))))
cat(sprintf("%-35s %s\n", "Livestock Avg Loss:",
            dollar(mean(sim_results$livestock_loss))))
cat(sprintf("%-35s %s\n", "Agri-Business Avg Loss:",
            dollar(mean(sim_results$agribiz_loss))))

cat("\nMODEL INPUTS\n")
cat("─────────────────────────────────────\n")
cat(sprintf("%-35s %s\n",
            "Simulations Run:", comma(n_sims)))
cat(sprintf("%-35s %s\n",
            "Loans in Portfolio:", comma(nrow(portfolio))))
cat(sprintf("%-35s %s\n",
            "PD Model:",
            "Logistic Regression (AUC 0.70)"))
cat(sprintf("%-35s %s\n",
            "Macro Inputs:",
            "FRED Historical Simulation"))
cat(sprintf("%-35s %s\n",
            "Data Period:", "2000-2025"))
cat(sprintf("%-35s %s\n",
            "USDA ERS Validation:",
            "Net Farm Income 2000-2024"))

cat("\n========================================\n")
cat("  STEP 4 COMPLETE — VISUALIZATIONS DONE \n")
cat("========================================\n")
cat("Ready for Step 5 — Dashboard Build\n")