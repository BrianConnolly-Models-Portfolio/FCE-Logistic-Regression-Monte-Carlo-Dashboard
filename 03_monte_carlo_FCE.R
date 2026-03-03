# ============================================================
# FCE Monte Carlo Simulation
# Step 3: Monte Carlo Loss Distribution
# Author: Brian Connolly
# Date: 2026
# ============================================================

# Note: Requires 01_data_load.R and 02_logistic_regression.R
# to have been run first in the same session

library(tidyverse)
library(scales)

# ============================================================
# Step 3A — Build FCE Agricultural Portfolio
# ============================================================

# What we're doing:
# Creating our synthetic FCE portfolio
# Each loan gets a base PD from our logistic regression model
# scaled by 0.069 to translate from consumer lending
# to agricultural lending credit risk
#
# Scaling factor derivation:
# FRED Ag Delinquency Rate (DRALACBS): 1.48%
# Lending Club Observed Default Rate:  21.4%
# Factor = 1.48 / 21.4 = 0.069
# Both inputs sourced directly from Step 1 data pulls

ag_scaling_factor <- 1.48 / 21.4   # = 0.06916

cat("=== Domain Calibration Factor ===\n")
cat("FRED Ag Delinquency Rate:  1.48%\n")
cat("Lending Club Default Rate: 21.4%\n")
cat("Scaling Factor:           ",
    round(ag_scaling_factor, 4), "\n")
cat("Source: FRED DRALACBS / Lending Club Step 1 output\n\n")

set.seed(42)

n_loans <- 500

portfolio <- tibble(
  loan_id = 1:n_loans,
  
  # Segment assignment — matches FCE concentration
  segment = sample(
    c("Dairy", "Livestock", "Row Crop",
      "Agri-Business", "Greenhouse/Nursery",
      "Fishing/Aquaculture", "Timber/Forest",
      "Other Ag"),
    size    = n_loans,
    replace = TRUE,
    prob    = c(0.28, 0.18, 0.15, 0.14,
                0.10, 0.07, 0.05, 0.03)
  ),
  
  # Loan size by segment
  gross_exposure = case_when(
    segment == "Dairy"               ~
      rnorm(n_loans, 900000, 200000),
    segment == "Livestock"           ~
      rnorm(n_loans, 750000, 150000),
    segment == "Row Crop"            ~
      rnorm(n_loans, 700000, 175000),
    segment == "Agri-Business"       ~
      rnorm(n_loans, 850000, 250000),
    segment == "Greenhouse/Nursery"  ~
      rnorm(n_loans, 500000, 100000),
    segment == "Fishing/Aquaculture" ~
      rnorm(n_loans, 450000, 125000),
    segment == "Timber/Forest"       ~
      rnorm(n_loans, 400000, 100000),
    TRUE                             ~
      rnorm(n_loans, 350000, 75000)
  ),
  
  # Risk grade distribution
  risk_grade = sample(
    c("A", "B", "C", "D", "E", "F", "G"),
    size    = n_loans,
    replace = TRUE,
    prob    = c(0.15, 0.25, 0.28, 0.18,
                0.08, 0.04, 0.02)
  )
) %>%
  
  mutate(
    gross_exposure = pmax(gross_exposure, 100000),
    
    # Base PD from logistic regression × scaling factor
    # Raw logistic PD reflects consumer lending default rates
    # Scaling factor of 0.069 translates to ag lending reality
    # Derived from: FRED ag delinquency / LC default rate
    base_pd = case_when(
      risk_grade == "A" ~ 0.092 * ag_scaling_factor,
      risk_grade == "B" ~ 0.146 * ag_scaling_factor,
      risk_grade == "C" ~ 0.219 * ag_scaling_factor,
      risk_grade == "D" ~ 0.309 * ag_scaling_factor,
      risk_grade == "E" ~ 0.421 * ag_scaling_factor,
      risk_grade == "F" ~ 0.534 * ag_scaling_factor,
      risk_grade == "G" ~ 0.616 * ag_scaling_factor,
      TRUE              ~ 0.219 * ag_scaling_factor
    ),
    
    # LGD by segment
    lgd = case_when(
      segment == "Dairy"               ~ 0.30,
      segment == "Livestock"           ~ 0.40,
      segment == "Row Crop"            ~ 0.35,
      segment == "Agri-Business"       ~ 0.35,
      segment == "Greenhouse/Nursery"  ~ 0.45,
      segment == "Fishing/Aquaculture" ~ 0.45,
      segment == "Timber/Forest"       ~ 0.25,
      TRUE                             ~ 0.40
    ),
    
    # Rate sensitivity by segment
    rate_sensitivity = case_when(
      segment == "Dairy"               ~ 1.40,
      segment == "Livestock"           ~ 1.25,
      segment == "Row Crop"            ~ 1.10,
      segment == "Agri-Business"       ~ 1.20,
      segment == "Greenhouse/Nursery"  ~ 1.30,
      segment == "Fishing/Aquaculture" ~ 1.15,
      segment == "Timber/Forest"       ~ 1.20,
      TRUE                             ~ 1.10
    ),
    
    # Corn price sensitivity by segment
    corn_sensitivity = case_when(
      segment == "Row Crop"            ~ 1.80,
      segment == "Livestock"           ~ 1.30,
      segment == "Agri-Business"       ~ 1.20,
      segment == "Dairy"               ~ 1.10,
      TRUE                             ~ 1.05
    ),
    
    # Milk price sensitivity by segment
    milk_sensitivity = case_when(
      segment == "Dairy"               ~ 1.80,
      segment == "Agri-Business"       ~ 1.20,
      segment == "Livestock"           ~ 1.10,
      TRUE                             ~ 1.05
    )
  )

cat("=== Step 3A — FCE Portfolio Built ===\n")
cat("Total loans:    ", nrow(portfolio), "\n")
cat("Total exposure: ",
    dollar(sum(portfolio$gross_exposure)), "\n")
cat("Avg base PD:    ",
    percent(mean(portfolio$base_pd), accuracy = 0.01), "\n")

# Show scaled PD table — key validation step
cat("\n=== Scaled PD by Risk Grade ===\n")
cat("Logistic Regression PD × 0.069 scaling factor\n")
portfolio %>%
  group_by(risk_grade) %>%
  summarise(
    n_loans    = n(),
    raw_pd     = percent(mean(base_pd / ag_scaling_factor),
                         accuracy = 0.1),
    scaled_pd  = percent(mean(base_pd), accuracy = 0.01),
    .groups    = "drop"
  ) %>%
  arrange(risk_grade) %>%
  print()

# ============================================================
# Step 3B — Calibrate Macro Distributions from FRED
# ============================================================

# What we're doing:
# Using real historical FRED data to define the probability
# distributions the Monte Carlo samples from
# 25 years of observed data defines the simulation space
# Every scenario drawn is historically plausible

fred_changes <- tibble(
  rate     = fed_funds$value,
  corn_chg = c(NA, diff(log(corn_prices$value))),
  milk_chg = c(NA, diff(log(milk_prices$value)))
) %>%
  drop_na()

cat("\n=== Step 3B — Macro Input Distributions ===\n")
cat("Sampling space defined by FRED 2000-2025\n\n")

cat("Fed Funds Rate:\n")
cat("  Mean:  ", round(mean(fred_changes$rate), 2), "%\n")
cat("  StDev: ", round(sd(fred_changes$rate),   2), "%\n")
cat("  Min:   ", round(min(fred_changes$rate),   2), "%\n")
cat("  Max:   ", round(max(fred_changes$rate),   2), "%\n")

cat("\nCorn Price Monthly Change:\n")
cat("  Mean:  ",
    percent(mean(fred_changes$corn_chg), accuracy = 0.01), "\n")
cat("  StDev: ",
    percent(sd(fred_changes$corn_chg),   accuracy = 0.01), "\n")
cat("  Worst: ",
    percent(min(fred_changes$corn_chg),  accuracy = 0.01), "\n")

cat("\nMilk Price Monthly Change:\n")
cat("  Mean:  ",
    percent(mean(fred_changes$milk_chg), accuracy = 0.01), "\n")
cat("  StDev: ",
    percent(sd(fred_changes$milk_chg),   accuracy = 0.01), "\n")
cat("  Worst: ",
    percent(min(fred_changes$milk_chg),  accuracy = 0.01), "\n")

# ============================================================
# Step 3C — Run Monte Carlo Simulation
# ============================================================

# What we're doing:
# 10,000 independent economic scenarios
# Each scenario:
#   1. Draws random macro conditions from FRED distributions
#   2. Calculates stress multiplier per segment
#   3. Adjusts scaled PD based on macro conditions
#   4. Simulates defaults loan by loan via Bernoulli draw
#   5. Calculates total portfolio loss
#
# The scaling factor is baked into base_pd already
# Stress multipliers then move PD up or down
# relative to the calibrated agricultural baseline

n_sims <- 10000

cat("\n=== Step 3C — Running Monte Carlo ===\n")
cat("Simulations:    ", n_sims, "\n")
cat("Loans per sim:  ", nrow(portfolio), "\n")
cat("Scaling factor: ", round(ag_scaling_factor, 4), "\n")
cat("Running...\n")

set.seed(42)

sim_results <- tibble(
  sim_id         = 1:n_sims,
  total_loss     = NA_real_,
  loss_rate      = NA_real_,
  n_defaults     = NA_integer_,
  rate_scenario  = NA_real_,
  corn_scenario  = NA_real_,
  milk_scenario  = NA_real_,
  dairy_loss     = NA_real_,
  row_crop_loss  = NA_real_,
  livestock_loss = NA_real_,
  agribiz_loss   = NA_real_
)

for(i in 1:n_sims) {
  
  # Step 1 — Draw random macro scenario
  # Bootstrap sample from historical FRED observations
  scenario_idx <- sample(nrow(fred_changes), 1)
  
  rate_draw <- fred_changes$rate[scenario_idx]
  corn_draw <- fred_changes$corn_chg[scenario_idx]
  milk_draw <- fred_changes$milk_chg[scenario_idx]
  
  # Step 2 — Calculate macro stress multipliers
  # Rate stress: each 1% above current 3.72% adds 8% to PD
  # Corn stress: negative price shock amplifies Row Crop PD
  # Milk stress: negative price shock amplifies Dairy PD
  rate_stress <- 1 + pmax(rate_draw - 3.72, 0) * 0.08
  corn_stress <- 1 + pmax(-corn_draw, 0) * 2.0
  milk_stress <- 1 + pmax(-milk_draw, 0) * 2.0
  
  # Step 3 — Apply stress to calibrated agricultural PDs
  sim_portfolio <- portfolio %>%
    mutate(
      
      # Stressed PD = Scaled base PD × macro multipliers
      # Already calibrated to ag lending via 0.069 factor
      # Stress moves PD up from the ag-realistic baseline
      stressed_pd = pmin(
        base_pd *
          (rate_sensitivity * rate_stress) *
          (corn_sensitivity * corn_stress) *
          (milk_sensitivity * milk_stress),
        0.95
      ),
      
      # Bernoulli draw — did this loan default?
      defaulted = rbinom(n(), 1, stressed_pd),
      
      # Loss = EAD × LGD if defaulted
      loss = defaulted * gross_exposure * lgd
    )
  
  # Store results
  sim_results$total_loss[i]    <- sum(sim_portfolio$loss)
  sim_results$loss_rate[i]     <- sum(sim_portfolio$loss) /
    sum(sim_portfolio$gross_exposure)
  sim_results$n_defaults[i]    <- sum(sim_portfolio$defaulted)
  sim_results$rate_scenario[i] <- rate_draw
  sim_results$corn_scenario[i] <- corn_draw
  sim_results$milk_scenario[i] <- milk_draw
  
  seg_losses <- sim_portfolio %>%
    group_by(segment) %>%
    summarise(loss = sum(loss), .groups = "drop")
  
  sim_results$dairy_loss[i] <- seg_losses %>%
    filter(segment == "Dairy") %>%
    pull(loss) %>%
    {if(length(.) == 0) 0 else .}
  
  sim_results$row_crop_loss[i] <- seg_losses %>%
    filter(segment == "Row Crop") %>%
    pull(loss) %>%
    {if(length(.) == 0) 0 else .}
  
  sim_results$livestock_loss[i] <- seg_losses %>%
    filter(segment == "Livestock") %>%
    pull(loss) %>%
    {if(length(.) == 0) 0 else .}
  
  sim_results$agribiz_loss[i] <- seg_losses %>%
    filter(segment == "Agri-Business") %>%
    pull(loss) %>%
    {if(length(.) == 0) 0 else .}
}

cat("Simulation complete\n")

# ============================================================
# Step 3D — Results Summary
# ============================================================

cat("\n=== Step 3D — Monte Carlo Results ===\n")
cat("Based on", n_sims,
    "simulated economic scenarios\n")
cat("PDs calibrated via 0.069 ag scaling factor\n\n")

cat("--- Portfolio Loss Distribution ---\n")
cat("Expected Loss (mean):      ",
    dollar(mean(sim_results$total_loss)), "\n")
cat("Median Loss (50th pct):    ",
    dollar(median(sim_results$total_loss)), "\n")
cat("75th Percentile:           ",
    dollar(quantile(sim_results$total_loss, 0.75)), "\n")
cat("95th Percentile (VaR):     ",
    dollar(quantile(sim_results$total_loss, 0.95)), "\n")
cat("99th Percentile:           ",
    dollar(quantile(sim_results$total_loss, 0.99)), "\n")
cat("Expected Shortfall (CVaR): ",
    dollar(mean(sim_results$total_loss[
      sim_results$total_loss >=
        quantile(sim_results$total_loss, 0.95)
    ])), "\n")

cat("\n--- Capital Adequacy ---\n")
cat("Current CECL Reserve:      $8,100,000\n")
cat("Monte Carlo Expected Loss: ",
    dollar(mean(sim_results$total_loss)), "\n")
cat("95th Pct Capital Needed:   ",
    dollar(quantile(sim_results$total_loss, 0.95)), "\n")
cat("Capital Shortfall at 95th: ",
    dollar(pmax(quantile(sim_results$total_loss,
                         0.95) - 8100000, 0)), "\n")

prob_sufficient <- mean(sim_results$total_loss <= 8100000)
cat("Probability $8.1M reserve sufficient: ",
    percent(prob_sufficient, accuracy = 0.1), "\n")

cat("\n--- Average Segment Losses ---\n")
tibble(
  Segment  = c("Dairy", "Row Crop",
               "Livestock", "Agri-Business"),
  Avg_Loss = c(mean(sim_results$dairy_loss),
               mean(sim_results$row_crop_loss),
               mean(sim_results$livestock_loss),
               mean(sim_results$agribiz_loss)),
  VaR_95   = c(quantile(sim_results$dairy_loss,    0.95),
               quantile(sim_results$row_crop_loss,  0.95),
               quantile(sim_results$livestock_loss, 0.95),
               quantile(sim_results$agribiz_loss,   0.95))
) %>%
  mutate(
    Avg_Loss = dollar(Avg_Loss),
    VaR_95   = dollar(VaR_95)
  ) %>%
  print()

cat("\n========================================\n")
cat("  STEP 3 COMPLETE — MONTE CARLO DONE    \n")
cat("========================================\n")
cat("Ready for Step 4 — Visualizations\n")
