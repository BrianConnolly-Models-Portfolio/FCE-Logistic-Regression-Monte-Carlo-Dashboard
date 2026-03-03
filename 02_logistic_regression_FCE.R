# ============================================================
# FCE Monte Carlo Simulation
# Step 2: Logistic Regression — Credit Default Model
# Author: Brian Connolly
# Date: 2026
# ============================================================

# Note: Run 01_data_load.R first to load loans_raw
# This script assumes loans_raw exists in environment

library(tidyverse)
library(scales)
library(pROC)
library(broom)
library(caret)

# ============================================================
# Step 2A — Create Binary Default Target Variable
# ============================================================

# What we're doing:
# Collapsing 9 loan status categories into binary 0/1
# 1 = defaulted  (loan was not repaid)
# 0 = not default (loan was fully repaid)
#
# We exclude ambiguous statuses — Current, Grace Period,
# Late 16-30 days — because we don't know their final outcome
# Including them would introduce noise into our model
#
# Training on ambiguous data = model learns the wrong signal

loans_model <- loans_raw %>%
  
  # Keep only loans with definitive known outcomes
  filter(loan_status %in% c(
    "Fully Paid",
    "Charged Off",
    "Late (31-120 days)",
    "Default",
    "Does not meet the credit policy. Status:Charged Off",
    "Does not meet the credit policy. Status:Fully Paid"
  )) %>%
  
  # Create binary default flag
  # Charged Off = lender wrote off the loan as a loss
  # Late 31-120 days = severely delinquent, very likely default
  # Default = explicitly marked as default
  mutate(
    default = as.integer(loan_status %in% c(
      "Charged Off",
      "Late (31-120 days)",
      "Default",
      "Does not meet the credit policy. Status:Charged Off"
    ))
  )

# Report what we kept and what the default rate is
cat("=== Step 2A — Binary Target Variable ===\n")
cat("Loans with definitive outcomes:", nrow(loans_model), "\n")
cat("Loans excluded (ambiguous):     ",
    nrow(loans_raw) - nrow(loans_model), "\n")
cat("Total defaults:                 ",
    sum(loans_model$default), "\n")
cat("Default rate:                   ",
    percent(mean(loans_model$default), accuracy = 0.1), "\n")

# ============================================================
# Step 2B — Feature Engineering
# ============================================================

# What we're doing:
# Selecting the variables that will predict default
# and transforming them into forms the model can use
#
# A good feature has three qualities:
# 1. Known at origination — not post-default information
# 2. Logical relationship to default risk
# 3. Sufficient variation to be informative

loans_features <- loans_model %>%
  
  select(
    default,
    loan_amnt,
    int_rate,
    annual_inc,
    dti,
    delinq_2yrs,
    inq_last_6mths,
    open_acc,
    pub_rec,
    revol_util,
    grade,
    home_ownership,
    term,
    purpose,
    addr_state
  ) %>%
  
  mutate(
    
    # Convert term from text to number
    # "36 months" → 36  |  "60 months" → 60
    # Why: 60 month loans have more time to default
    term_months = as.numeric(str_extract(term, "\\d+")),
    
    # Log transform income — right skewed distribution
    # A few very high earners distort the model
    # Log scale compresses extremes, improves model fit
    # Example: log(50,000) = 10.8  |  log(500,000) = 13.1
    # The difference shrinks from 450K to 2.3 — more proportional
    log_annual_inc = log(annual_inc + 1),
    
    # Log transform loan amount — same reason as income
    log_loan_amnt = log(loan_amnt + 1),
    
    # Cap DTI at 60% — extreme outliers distort the model
    # A DTI of 200% is not 10x more risky than 20%
    # The relationship flattens at extreme values
    dti_capped = pmin(dti, 60),
    
    # Northeast flag — FCE territory
    # 1 = borrower in FCE's 8-state territory
    # 0 = rest of US
    northeast = as.integer(addr_state %in%
                             c("ME", "NH", "VT", "MA",
                               "CT", "RI", "NY", "NJ")),
    
    # Encode credit grade as ordered numeric 1-7
    # A = 1 (best), G = 7 (worst)
    # Mirrors FCE's internal 1-7 risk rating scale
    grade_numeric = case_when(
      grade == "A" ~ 1,
      grade == "B" ~ 2,
      grade == "C" ~ 3,
      grade == "D" ~ 4,
      grade == "E" ~ 5,
      grade == "F" ~ 6,
      grade == "G" ~ 7,
      TRUE         ~ 4
    ),
    
    # Encode home ownership as collateral risk proxy
    # MORTGAGE = 1 (real estate secured — lowest risk)
    # OWN      = 2 (free and clear property)
    # RENT     = 3 (no collateral — highest risk)
    # Mirrors FCE's LTV-based collateral assessment
    home_risk = case_when(
      home_ownership == "MORTGAGE" ~ 1,
      home_ownership == "OWN"      ~ 2,
      home_ownership == "RENT"     ~ 3,
      TRUE                         ~ 2
    ),
    
    # Small business flag
    # Most relevant loan purpose for agricultural comparison
    small_biz = as.integer(purpose == "small_business"),
    
    # Default must be factor for glm classification
    default_factor = factor(default,
                            levels = c(0, 1),
                            labels = c("No", "Yes"))
  ) %>%
  
  # Remove rows with any missing values
  # We lose <0.1% of data — acceptable tradeoff
  drop_na(default, int_rate, annual_inc, dti,
          delinq_2yrs, inq_last_6mths,
          revol_util, open_acc, grade)

cat("\n=== Step 2B — Feature Dataset ===\n")
cat("Rows after feature engineering:", nrow(loans_features), "\n")
cat("Default rate:                  ",
    percent(mean(loans_features$default),
            accuracy = 0.1), "\n")

# ============================================================
# Step 2C — Train / Test Split
# ============================================================

# What we're doing:
# Splitting the data into two sets
# TRAINING SET (80%) — model learns patterns from this
# TEST SET (20%)     — we evaluate model on data it never saw
#
# Why split?
# If we test on the same data we trained on the model
# looks artificially accurate — it just memorized the answers
# Testing on unseen data tells us if it actually learned
# generalizable patterns — this is called out-of-sample validation
#
# set.seed(42) ensures this split is reproducible
# Anyone running this code gets the exact same split

set.seed(42)

# Create index of rows for training set
train_index <- createDataPartition(
  loans_features$default_factor,
  p    = 0.80,   # 80% training, 20% test
  list = FALSE
)

train_data <- loans_features[ train_index, ]
test_data  <- loans_features[-train_index, ]

cat("\n=== Step 2C — Train/Test Split ===\n")
cat("Training set: ", nrow(train_data), "loans (",
    percent(nrow(train_data)/nrow(loans_features),
            accuracy = 0.1), ")\n")
cat("Test set:     ", nrow(test_data),  "loans (",
    percent(nrow(test_data)/nrow(loans_features),
            accuracy = 0.1), ")\n")
cat("Train default rate:", 
    percent(mean(train_data$default), accuracy = 0.1), "\n")
cat("Test default rate: ",
    percent(mean(test_data$default),  accuracy = 0.1), "\n")
cat("Default rates should be nearly identical\n")
cat("caret's createDataPartition ensures balanced split\n")

# ============================================================
# Step 2D — Train Logistic Regression Model
# ============================================================

# What we're doing:
# Training a logistic regression model on the training set
# 
# Why logistic regression for PD modeling?
# - Output is naturally bounded 0-1 (a probability)
# - Coefficients are interpretable as odds ratios
# - Industry standard for credit scoring (Basel II/III)
# - Regulators understand and accept it
# - Directly produces PD — what CECL requires
#
# The formula reads:
# log(P(default) / P(not default)) = 
#   β0 + β1*grade + β2*dti + β3*int_rate + ...
#
# Each coefficient tells us:
# "Holding all else equal, a one unit increase in X
#  multiplies the odds of default by e^β"

cat("\n=== Step 2D — Training Logistic Regression ===\n")
cat("Training on", nrow(train_data), "loans...\n")
cat("This may take 30-60 seconds on 2.26M rows\n")

logit_model <- glm(
  default ~
    grade_numeric    +   # Credit grade 1-7
    dti_capped       +   # Debt-to-income ratio
    int_rate         +   # Interest rate
    log_annual_inc   +   # Log income
    log_loan_amnt    +   # Log loan size
    term_months      +   # Loan term
    delinq_2yrs      +   # Prior delinquencies
    inq_last_6mths   +   # Recent inquiries
    revol_util       +   # Revolving utilization
    home_risk        +   # Collateral proxy
    pub_rec          +   # Public records
    small_biz        +   # Small business flag
    northeast,           # FCE territory flag
  
  data   = train_data,
  family = binomial(link = "logit")  # Logistic regression
)

cat("Model trained successfully\n")

# ============================================================
# Step 2E — Model Evaluation
# ============================================================

# What we're doing:
# Assessing how well the model predicts defaults
# on the TEST set — data it never saw during training
#
# Key metrics:
# AUC — Area Under the ROC Curve
#   0.5 = random guessing (coin flip)
#   0.7 = acceptable
#   0.8 = good
#   0.9 = excellent
#   1.0 = perfect (suspicious — probably overfitting)
#   We expect ~0.70-0.75 for a credit model
#
# Confusion Matrix:
#   True Positive  = predicted default, actually defaulted
#   True Negative  = predicted ok, actually ok
#   False Positive = predicted default, actually ok (Type I)
#   False Negative = predicted ok, actually defaulted (Type II)
#   In credit risk — False Negatives are more costly

# Generate predicted probabilities on test set
test_data <- test_data %>%
  mutate(
    pd_predicted = predict(logit_model,
                           newdata = test_data,
                           type    = "response")
  )

# ROC Curve and AUC
roc_result <- roc(
  response  = test_data$default,
  predictor = test_data$pd_predicted
)

cat("\n=== Step 2E — Model Performance ===\n")
cat("AUC Score:", round(auc(roc_result), 4), "\n")
cat("Interpretation:\n")
cat("  0.50 = random | 0.70 = acceptable | 0.80 = good\n")

# Plot ROC curve
plot(roc_result,
     main = "ROC Curve — Logistic Regression PD Model",
     col  = "#2D6A4F",
     lwd  = 2,
     print.auc = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")

# Confusion matrix at 0.50 threshold
test_data <- test_data %>%
  mutate(
    default_predicted = as.integer(pd_predicted >= 0.20)
  )

cat("\n=== Confusion Matrix (threshold = 0.20) ===\n")
cat("Why 0.20 threshold?\n")
cat("Our default rate is ~20% so 0.20 is a natural cutoff\n")
conf_matrix <- table(
  Predicted = test_data$default_predicted,
  Actual    = test_data$default
)
print(conf_matrix)

# Accuracy metrics
cat("\nAccuracy: ",
    percent(mean(test_data$default_predicted ==
                   test_data$default),
            accuracy = 0.1), "\n")

# ============================================================
# Step 2F — Coefficient Interpretation
# ============================================================

# What we're doing:
# Reading the model coefficients to understand
# what drives default probability
#
# Coefficients are in LOG ODDS — hard to interpret directly
# We convert to ODDS RATIOS by exponentiating: exp(coefficient)
#
# Odds ratio interpretation:
# 1.0 = no effect on default probability
# 1.5 = 50% higher odds of default per unit increase
# 0.5 = 50% lower odds of default per unit increase
#
# This is your interview answer to:
# "What are the key drivers of default in your model?"

cat("\n=== Step 2F — Model Coefficients ===\n")
cat("Odds ratios — how each variable affects default risk\n")
cat("Values above 1.0 INCREASE default risk\n")
cat("Values below 1.0 DECREASE default risk\n")

tidy(logit_model, exponentiate = TRUE,
     conf.int = TRUE) %>%
  select(term, estimate, conf.low,
         conf.high, p.value) %>%
  mutate(
    estimate  = round(estimate,  4),
    conf.low  = round(conf.low,  4),
    conf.high = round(conf.high, 4),
    p.value   = round(p.value,   6),
    significant = ifelse(p.value < 0.05, "YES", "NO")
  ) %>%
  arrange(desc(estimate)) %>%
  print()

# ============================================================
# Step 2G — PD Distribution Summary
# ============================================================

# What we're doing:
# Looking at the distribution of predicted PDs
# across the full dataset
# This tells us if our model produces realistic PD ranges
# and validates against our FCE baseline of 6.63%

cat("\n=== Step 2G — Predicted PD Distribution ===\n")
cat("Do these ranges look realistic for credit risk?\n")
summary(test_data$pd_predicted)

cat("\n=== PD by Credit Grade ===\n")
cat("Should mirror the staircase we saw in Step 1\n")
test_data %>%
  group_by(grade) %>%
  summarise(
    n_loans     = n(),
    avg_pd      = percent(mean(pd_predicted),
                          accuracy = 0.1),
    actual_dr   = percent(mean(default),
                          accuracy = 0.1),
    .groups     = "drop"
  ) %>%
  arrange(grade) %>%
  print()

cat("\n=== PD — Northeast vs National ===\n")
cat("FCE territory comparison\n")
test_data %>%
  group_by(northeast) %>%
  summarise(
    n_loans   = n(),
    avg_pd    = percent(mean(pd_predicted),
                        accuracy = 0.1),
    actual_dr = percent(mean(default),
                        accuracy = 0.1),
    .groups   = "drop"
  ) %>%
  mutate(region = ifelse(northeast == 1,
                         "Northeast — FCE Territory",
                         "Rest of US")) %>%
  select(region, n_loans, avg_pd, actual_dr) %>%
  print()

cat("\n========================================\n")
cat("   STEP 2 COMPLETE — MODEL TRAINED      \n")
cat("========================================\n")
cat("Ready for Step 3 — Monte Carlo Simulation\n")


