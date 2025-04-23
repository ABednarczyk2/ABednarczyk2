library(tidyverse)
library(lubridate)
library(data.table)
library(plm)
library(lpirfs)
install.packages(c("lpirfs", "plm", "tidyverse", "lubridate"))
library(lpirfs)  #  local projections
library(plm)
library(dplyr)
library(broom)


# Rename columns for consistency
FEDFUNDS <- FEDFUNDS %>% 
  rename(date = observation_date, fedfunds = FEDFUNDS)
freddiemac<-copy(historicalweeklydata1)

freddiemac <- freddiemac %>% 
  rename(
    date = week,
    frm30 = `30 yr FRM`,
    fees30 = `30yr fees&point`,
    frm15 = `15yr FRM`,
    fees15 = `15yr FRM fees&points`,
    arm5 = `5/1 ARM`,
    fees5 = `5/1 ARM fees&points`,
    margin5 = `5/1 ARM margin`,
    spread5 = `5/1 ARM spread`
  )

U1bp <- U1bp %>% 
  rename(date = Time)

merged <- FEDFUNDS %>% 
  mutate(date = ceiling_date(date, "week")) %>%  # Align to week-end
  full_join(
    freddiemac %>% mutate(date = as.Date(date)),
    by = "date"
  ) %>% 
  full_join(
    U1bp %>% mutate(date = as.Date(date)),
    by = "date"
  ) %>% 
  arrange(date) %>% 
  filter(!is.na(u1))  # Keep only weeks with shocks




# Convert to panel data
pdata <- pdata.frame(merged, index = "date")
##############################################
library(dplyr)
library(lubridate)
library(dplyr)
library(lubridate)

# 1. Clean and standardize dates in each dataframe
# For U1bp (contains hours): extract just the date portion
U1bp <- U1bp %>% 
  mutate(date = as.Date(date)) %>%  # Drops the hour component
  distinct(date, .keep_all = TRUE)  # Remove potential duplicates if multiple shocks same day

# For freddiemac (Freddie Mac data) - ensure it's Date type
freddiemac <- freddiemac %>% 
  mutate(date = as.Date(date))

# For FEDFUNDS - ensure it's Date type
FEDFUNDS <- FEDFUNDS %>% 
  mutate(date = as.Date(date))

# 2. Merge data (using weekly Freddie Mac data as base)
merged_data <- freddiemac %>% 
  left_join(FEDFUNDS, by = "date") %>% 
  left_join(U1bp, by = "date")

# 3. Replace shock NAs with 0 (no shock occurred)
shock_cols <- c("u1", "u2", "u3", "u4")  # Adjust if column names differ
merged_data <- merged_data %>%
  mutate(across(any_of(shock_cols), ~replace_na(., 0)))

# 4. Filter for 1999-2021
merged_data <- merged_data %>% 
  filter(year(date) >= 1999 & year(date) <= 2021)

# 5. Verify merge
head(merged_data)
summary(merged_data)

# Check date range and NA counts
range(merged_data$date)
sapply(merged_data, function(x) sum(is.na(x)))

# See column types
str(freddiemac)

rm(freddiemac)
freddiemac<-copy(historicalweeklydata1)

library(readxl)
library(readxl)
library(dplyr)

# 1. Re-import with proper parameters
freddiemac <- read_excel(
  path = "C:/Users/2022/Downloads/historicalweeklydata1.xlsx",
  range = "A1:I2820",               # Explicit data range (skip footer)
  col_types = c("date", rep("numeric", 8)),  # Force numeric columns
  na = c("NA", "N/A", "-", "", "NULL")       # Common NA indicators
)

# 2. Clean column names (remove special characters)
colnames(freddiemac) <- gsub("[ /&]", "_", colnames(freddiemac))

# 3. Verify structure
str(freddiemac)

# 4. Final cleanup if any issues remain
freddiemac <- freddiemac %>%
  mutate(across(where(is.logical), as.numeric)) %>%    # Fix any remaining logicals
  filter(!is.na(date)) %>%                            # Remove empty rows
  distinct(date, .keep_all = TRUE)                    # Remove duplicate dates

# 5. Save cleaned version
write.csv(freddiemac, "freddiemac_cleaned.csv", row.names = FALSE)




library(dplyr)
library(lubridate)

library(dplyr)
library(lubridate)

# Trim timestamps from U1bp (keep only dates)
U1bp_clean <- U1bp %>%
  mutate(date = as.Date(date))  # Drops hours/minutes

# Ensure DFF is Date type (if not already)
DFF_clean <- DFF %>%
  mutate(date = as.Date(observation_date)) %>%
  select(-observation_date)

freddie_clean <- freddiemac %>%
  rename(date = week) %>%  # Rename 'week' column to 'date' for consistency
  mutate(date = as.Date(date))  # Ensure Date format
# 4. Merge datasets
final_data <- DFF_clean %>%          # Start with daily rates
  left_join(U1bp_clean, by = "date") %>%  # Add shocks
  left_join(freddie_clean, by = "date")   # Add weekly mortgage data

# 5. Handle missing values
final_data <- final_data %>%
  mutate(across(starts_with("shock_"), ~replace_na(., 0))) %>%  # 0 = no shock
  arrange(date) %>%
  filter(year(date) >= 1999 & year(date) <= 2021)

# 6. Forward-fill weekly mortgage data to daily
final_data <- final_data %>%
  fill(names(freddie_clean)[-1], .direction = "down")


#weeeeeeeeeeeeeeeeeeeeee
library(lpirfs)  # For local projections
library(sandwich) # For robust standard errors

colnames(final_data)

library(dplyr)
library(lpirfs)

# Select and rename the needed columns
lp_data <- final_data %>%
  select(
    date,
    FRM30 = `30_yr_FRM`,  # Mortgage rate
    shock = u1,           # Policy shock (using u1)
    FEDFUNDS = DFF        # Federal funds rate
  ) %>%
  na.omit()  # Remove any rows with missing values

# Verify the data
glimpse(lp_data)





# Set parameters
horizon <- 12  # Response over 12 weeks
lags <- 4      # Number of lags for controls

# Run LP model
lp_results <- lp_lin(
  endog_data = lp_data$FRM30,     # 30-year mortgage rate
  shock = lp_data$shock,          # Policy shock (u1)
  exog_data = lp_data["FEDFUNDS"], # Control variable (as data.frame)
  lags_endog_lin = lags,          # Lags of dependent variable
  lags_exog = lags,               # Lags of control variable
  hor = horizon,                  # Forecast horizon
  confint = 1.96                  # 95% confidence intervals
)







# Create a complete data frame in base R format
model_data <- data.frame(
  FRM30 = lp_data$FRM30,
  shock = lp_data$shock,
  FEDFUNDS = lp_data$FEDFUNDS
)

# Run LP model
lp_results <- lp_lin(
  endog_data = model_data$FRM30,
  shock = model_data$shock,
  exog_data = model_data["FEDFUNDS"], # Now properly formatted
  lags_endog_lin = 4,
  hor = 12
)




model_data <- as.data.frame(model_data)
exog_data <- data.frame(FEDFUNDS = model_data$FEDFUNDS)
library(lpirfs)

lp_results <- lp_lin(
  endog_data = model_data$FRM30,  # Dependent variable (numeric vector)
  shock = model_data$shock,       # Policy shock (numeric vector)
  exog_data = exog_data,          # Control variable (data.frame)
  lags_endog_lin = 4,             # Lags of dependent variable
  lags_exog = 4,                  # Lags of control variable
  hor = 12,                       # Forecast horizon (12 weeks)
  confint = 1.96                  # 95% confidence intervals
)


library(lpirfs)

# Ensure all inputs are base R objects (not tibbles)
model_data <- as.data.frame(model_data)
exog_data <- as.data.frame(model_data["FEDFUNDS"])  # Use subsetting with [ , ]

# Verify classes
class(model_data$FRM30)    # Should be "numeric"
class(model_data$shock)    # Should be "numeric"
class(exog_data)           # Should be "data.frame"
lp_results <- lp_lin(
  endog_data = model_data$FRM30,   # Numeric vector
  shock = model_data$shock,        # Numeric vector
  exog_data = exog_data,           # Data frame (not tibble)
  lags_endog_lin = 4,              # Lags for endogenous variable
  lags_exog = 4,                   # Lags for exogenous variable
  hor = 12,                        # Horizon (12 weeks)
  confint = 1.96                   # 95% confidence intervals
)






library(lpirfs)

# Prepare data
model_data <- data.frame(
  FRM30 = lp_data$FRM30,
  shock = lp_data$shock,
  FEDFUNDS = lp_data$FEDFUNDS
) %>% na.omit()

# Convert exogenous variable to a MATRIX
exog_matrix <- as.matrix(model_data["FEDFUNDS"])


lp_results <- lp_lin(
  endog_data = model_data$FRM30,   # Numeric vector
  shock = model_data$shock,        # Numeric vector
  exog_data = exog_matrix,         # Matrix (not data.frame)
  lags_endog_lin = 4,              # Lags for endogenous variable
  lags_exog = 4,                   # Lags for exogenous variable
  hor = 12,                        # Horizon (12 weeks)
  confint = 1.96                   # 95% confidence intervals
)
write.csv(final_data, "merged_mortgage_analysis.csv", row.names = FALSE)







library(dplyr)
library(ggplot2)

# 1. Prepare data with proper lags and leads
model_data <- final_data %>%
  select(date, FRM30 = `30_yr_FRM`, shock = u1, FEDFUNDS = DFF) %>%
  na.omit() %>%
  mutate(
    # Lags for FRM30 and FEDFUNDS
    FRM30_lag1 = lag(FRM30, 1),
    FRM30_lag2 = lag(FRM30, 2),
    FRM30_lag3 = lag(FRM30, 3),
    FRM30_lag4 = lag(FRM30, 4),
    FEDFUNDS_lag1 = lag(FEDFUNDS, 1),
    FEDFUNDS_lag2 = lag(FEDFUNDS, 2),
    FEDFUNDS_lag3 = lag(FEDFUNDS, 3),
    FEDFUNDS_lag4 = lag(FEDFUNDS, 4),
    # Leads for FRM30 (horizons h0 to h12)
    h0 = FRM30, 
    h1 = dplyr::lead(FRM30, 1),
    h2 = dplyr::lead(FRM30, 2),
    h3 = dplyr::lead(FRM30, 3),
    h4 = dplyr::lead(FRM30, 4),
    h5 = dplyr::lead(FRM30, 5),
    h6 = dplyr::lead(FRM30, 6),
    h7 = dplyr::lead(FRM30, 7),
    h8 = dplyr::lead(FRM30, 8),
    h9 = dplyr::lead(FRM30, 9),
    h10 = dplyr::lead(FRM30, 10),
    h11 = dplyr::lead(FRM30, 11),
    h12 = dplyr::lead(FRM30, 12)
  ) %>%
  na.omit()

# 2. Estimate local projections
irf <- sapply(paste0("h", 0:12), function(h) {
  formula <- as.formula(paste(h, "~ shock + FRM30_lag1 + FRM30_lag2 + FRM30_lag3 + FRM30_lag4 + FEDFUNDS_lag1 + FEDFUNDS_lag2 + FEDFUNDS_lag3 + FEDFUNDS_lag4"))
  model <- lm(formula, data = model_data)
  coef(model)["shock"]
})

# 3. Store results
results <- data.frame(
  horizon = 0:12,
  effect = irf
)

# 4. Plot the impulse response
ggplot(results, aes(x = horizon, y = effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "Impulse Response of 30-Year Mortgage Rates to Policy Shock",
    x = "Weeks After Shock",
    y = "Change in Mortgage Rate (Percentage Points)"
  ) +
  theme_minimal()














library(dplyr)
library(ggplot2)

# 1. Prepare data with proper lags and leads
model_data <- final_data %>%
  select(date, ARM = `5_1_ARM`, shock = u1, FEDFUNDS = DFF) %>%
  na.omit() %>%
  mutate(
    # Lags for FRM30 and FEDFUNDS
    ARM_lag1 = lag(ARM, 1),
    ARM_lag2 = lag(ARM, 2),
    ARM_lag3 = lag(ARM, 3),
    ARM_lag4 = lag(ARM, 4),
    FEDFUNDS_lag1 = lag(FEDFUNDS, 1),
    FEDFUNDS_lag2 = lag(FEDFUNDS, 2),
    FEDFUNDS_lag3 = lag(FEDFUNDS, 3),
    FEDFUNDS_lag4 = lag(FEDFUNDS, 4),
    # Leads for FRM30 (horizons h0 to h12)
    h0 = ARM, 
    h1 = dplyr::lead(ARM, 1),
    h2 = dplyr::lead(ARM, 2),
    h3 = dplyr::lead(ARM, 3),
    h4 = dplyr::lead(ARM, 4),
    h5 = dplyr::lead(ARM, 5),
    h6 = dplyr::lead(ARM, 6),
    h7 = dplyr::lead(ARM, 7),
    h8 = dplyr::lead(ARM, 8),
    h9 = dplyr::lead(ARM, 9),
    h10 = dplyr::lead(ARM, 10),
    h11 = dplyr::lead(ARM, 11),
    h12 = dplyr::lead(ARM, 12)
  ) %>%
  na.omit()

# 2. Estimate local projections
irf <- sapply(paste0("h", 0:12), function(h) {
  formula <- as.formula(paste(h, "~ shock + ARM_lag1 + ARM_lag2 + ARM_lag3 + ARM_lag4 + FEDFUNDS_lag1 + FEDFUNDS_lag2 + FEDFUNDS_lag3 + FEDFUNDS_lag4"))
  model <- lm(formula, data = model_data)
  coef(model)["shock"]
})

# 3. Store results
results <- data.frame(
  horizon = 0:12,
  effect = irf
)

# 4. Plot the impulse response
ggplot(results, aes(x = horizon, y = effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = "orchid", linewidth = 1) +
  geom_point(color = "orchid", size = 3) +
  labs(
    title = "Impulse Response of ARM Mortgage Rates to Policy Shock",
    x = "Weeks After Shock",
    y = "Change in Mortgage Rate (Percentage Points)"
  ) +
  theme_minimal()



