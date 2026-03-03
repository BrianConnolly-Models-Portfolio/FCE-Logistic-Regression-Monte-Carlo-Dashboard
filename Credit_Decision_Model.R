
library(fredr)
fredr_set_key("a871fe44d9bd0f19f66bd5006b6b2cca")
# Test it
fredr(series_id = "FEDFUNDS", 
      observation_start = as.Date("2000-01-01"))
# ============================================================
# FCE Monte Carlo Simulation
# Step 1: Complete Data Load and Exploration
# Author: Brian Connolly
# Date: 2026
# ============================================================

rm(list = ls())
gc()

options(scipen = 999, digits = 4)
set.seed(42)

library(tidyverse)
library(scales)
library(fredr)

# ============================================================
# Step 1A — FRED API Connection
# ============================================================

# What we're doing:
# Connecting to the Federal Reserve's data API
# and pulling 5 macro series from 2000-2025
# These become the input distributions for our Monte Carlo

fredr_set_key(Sys.getenv("FRED_API_KEY"))

# Fed Funds Rate
# What it is: Overnight lending rate set by the Federal Reserve
# Why it matters: Drives variable rate farm loan costs directly
# When rates rise — debt service increases — DSCR falls — PD rises
fed_funds <- fredr(
  series_id         = "FEDFUNDS",
  observation_start = as.Date("2000-01-01"),
  observation_end   = as.Date("2025-12-31")
)

# Corn Prices
# What it is: Producer Price Index for corn and corn products
# Why it matters: Primary revenue driver for Row Crop borrowers
# When corn prices fall — farm income falls — default risk rises
corn_prices <- fredr(
  series_id         = "WPU012",
  observation_start = as.Date("2000-01-01"),
  observation_end   = as.Date("2025-12-31")
)

# Farmland Values
# What it is: National average home/property sale price index
# Used as proxy for farmland collateral values
# Why it matters: Collateral value determines LTV and LGD
farmland_values <- fredr(
  series_id         = "ASPNHSUS",
  observation_start = as.Date("2000-01-01"),
  observation_end   = as.Date("2025-12-31")
)

# Agricultural Loan Delinquency Rate
# What it is: % of ag loans at commercial banks 30+ days past due
# Why it matters: Real world ground truth for Monte Carlo validation
# Our simulated loss distributions should center near this value
ag_delinquency <- fredr(
  series_id         = "DRALACBS",
  observation_start = as.Date("2000-01-01"),
  observation_end   = as.Date("2025-12-31")
)

# Milk Prices
# What it is: Producer Price Index for dairy products
# Why it matters: FCE's largest segment is Dairy at 27.6%
# Milk price volatility directly drives dairy farmer DSCR
milk_prices <- fredr(
  series_id         = "WPU0231",
  observation_start = as.Date("2000-01-01"),
  observation_end   = as.Date("2025-12-31")
)

cat("=== FRED Series Loaded ===\n")
cat("Fed Funds Rate:      ", nrow(fed_funds),
    "monthly observations\n")
cat("Corn Prices:         ", nrow(corn_prices),
    "monthly observations\n")
cat("Farmland Values:     ", nrow(farmland_values),
    "monthly observations\n")
cat("Ag Delinquency Rate: ", nrow(ag_delinquency),
    "quarterly observations\n")
cat("Milk Prices:         ", nrow(milk_prices),
    "monthly observations\n")

cat("\n=== Current Values — Most Recent Observation ===\n")
cat("Fed Funds Rate:      ",
    tail(fed_funds$value, 1), "%\n")
cat("Corn Price Index:    ",
    tail(corn_prices$value, 1), "\n")
cat("Ag Delinquency Rate: ",
    tail(ag_delinquency$value, 1), "%\n")
cat("Milk Price Index:    ",
    tail(milk_prices$value, 1), "\n")

# ============================================================
# Step 1B — Load Lending Club Data
# ============================================================

# What we're doing:
# Loading 2.26M real US loans with observed default outcomes
# This becomes the training data for our logistic regression
# Each row is one loan — we know if it was repaid or defaulted

loans_raw <- read_csv(
  "/Users/briguy/Brian Career Supplements/FCE_Monte_Carlo_Simulation/loan.csv"
)

cat("\n=== Lending Club Dataset ===\n")
cat("Rows:   ", nrow(loans_raw), "\n")
cat("Columns:", ncol(loans_raw), "\n")

# Loan status distribution — our target variable
# This shows how many loans defaulted vs were repaid
cat("\n=== Loan Status Distribution ===\n")
cat("This becomes our binary 0/1 target variable\n")
loans_raw %>%
  count(loan_status) %>%
  mutate(pct = percent(n / sum(n), accuracy = 0.1)) %>%
  arrange(desc(n)) %>%
  print()

# Preview key columns
cat("\n=== Key Columns for Logistic Regression ===\n")
loans_raw %>%
  select(
    loan_amnt,       # Loan amount
    int_rate,        # Interest rate
    annual_inc,      # Borrower annual income
    dti,             # Debt-to-income ratio
    loan_status,     # TARGET variable
    purpose,         # Loan purpose
    addr_state,      # US state
    home_ownership,  # Collateral proxy
    grade,           # Credit grade A-G
    sub_grade,       # Granular grade A1-G5
    delinq_2yrs,     # Prior delinquencies
    inq_last_6mths,  # Recent credit inquiries
    revol_util,      # Revolving utilization
    open_acc,        # Open accounts
    pub_rec,         # Public records
    term             # Loan term
  ) %>%
  glimpse()

# Sample rows — sanity check
cat("\n=== Sample Loans — First 5 Rows ===\n")
loans_raw %>%
  select(loan_amnt, int_rate, annual_inc,
         dti, grade, loan_status,
         purpose, home_ownership,
         addr_state, term) %>%
  head(5) %>%
  print()

# Missing value check
cat("\n=== Missing Values in Key Columns ===\n")
cat("Columns with high NA% need strategy before modeling\n")
loans_raw %>%
  select(
    loan_amnt, int_rate, annual_inc, dti,
    grade, loan_status, purpose,
    home_ownership, addr_state, term,
    delinq_2yrs, inq_last_6mths,
    revol_util, open_acc, pub_rec
  ) %>%
  summarise(across(everything(),
                   ~ percent(mean(is.na(.)),
                             accuracy = 0.1))) %>%
  pivot_longer(everything(),
               names_to  = "column",
               values_to = "pct_missing") %>%
  arrange(desc(pct_missing)) %>%
  print()

# Default rate by credit grade
cat("\n=== Default Rate by Credit Grade ===\n")
cat("Validates grade as predictor — A should be lowest\n")
loans_raw %>%
  filter(loan_status %in% c("Fully Paid", "Charged Off",
                            "Late (31-120 days)",
                            "Default")) %>%
  mutate(default = as.integer(loan_status %in%
                                c("Charged Off",
                                  "Late (31-120 days)",
                                  "Default"))) %>%
  group_by(grade) %>%
  summarise(
    n_loans      = n(),
    default_rate = percent(mean(default), accuracy = 0.1),
    avg_int_rate = round(mean(int_rate), 2),
    .groups = "drop"
  ) %>%
  arrange(grade) %>%
  print()

# ============================================================
# Step 1C — FRED Visualization
# ============================================================

# What we're doing:
# Plotting all 5 macro series to visually confirm they loaded
# correctly and document the historical context
# You should see: 2008 crisis, COVID shock, 2022 rate hikes

fred_combined <- bind_rows(
  fed_funds       %>% mutate(series = "Fed Funds Rate (%)"),
  corn_prices     %>% mutate(series = "Corn Price Index"),
  farmland_values %>% mutate(series = "Farmland Value Index"),
  ag_delinquency  %>% mutate(series = "Ag Delinquency Rate (%)"),
  milk_prices     %>% mutate(series = "Milk Price Index")
)

ggplot(fred_combined,
       aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ series, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y") +
  labs(
    title    = "FRED Macro Series — Monte Carlo Inputs",
    subtitle = "Historical context for stress scenario calibration 2000-2025",
    x        = NULL,
    y        = "Value",
    caption  = "Source: Federal Reserve Economic Data (FRED)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold", size = 9),
    plot.title       = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ============================================================
# Step 1D — USDA ERS Farm Income
# ============================================================

# What we're doing:
# Loading real US farm income data from 1974-2024
# Cleaning to keep only the most recent revision per year
# Extracting Net Farm Income as our primary ag health indicator

farm_income_raw <- read_csv(
  "/Users/briguy/Brian Career Supplements/FCE_Monte_Carlo_Simulation/farm_income_data.csv"
)

# Keep only most recent revision per year
# Why: USDA revises estimates as more data comes in
# We always want the most accurate final estimate
farm_income_clean <- farm_income_raw %>%
  filter(!is.na(amount)) %>%
  group_by(year, item) %>%
  filter(release_number == max(release_number)) %>%
  ungroup() %>%
  filter(year >= 2000)

cat("\n=== Farm Income Variables Available ===\n")
farm_income_clean %>%
  distinct(item, item_description, unit_desc) %>%
  print()

# Net Farm Income trend
# What it shows: Total US farm sector profitability
# Why it matters: Validates our Monte Carlo loss assumptions
# High NFI years = low defaults | Low NFI years = high defaults
cat("\n=== Net Farm Income by Year ===\n")
cat("Unit: $1,000 | Divide by 1,000,000 for billions\n")
farm_income_clean %>%
  filter(item == "nfi") %>%
  select(year, amount) %>%
  mutate(
    amount_billions = round(amount / 1e6, 1),
    yoy_change      = percent(
      (amount - lag(amount)) / lag(amount),
      accuracy = 0.1
    )
  ) %>%
  arrange(desc(year)) %>%
  head(15) %>%
  print()

# ============================================================
# Step 1E — Complete Summary
# ============================================================

cat("\n")
cat("========================================\n")
cat("     STEP 1 COMPLETE — DATA LOADED      \n")
cat("========================================\n")
cat("Lending Club: ", nrow(loans_raw),
    "loans |", ncol(loans_raw), "columns\n")
cat("FRED Series:   5 macro series | 2000-2025\n")
cat("USDA ERS:     ", nrow(farm_income_clean),
    "cleaned observations\n")
cat("========================================\n")
cat("Ready for Step 2 — Logistic Regression\n")