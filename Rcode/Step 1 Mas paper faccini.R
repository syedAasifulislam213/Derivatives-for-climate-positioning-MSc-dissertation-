# =========================================================
# Load packages
# =========================================================

library(readr)
library(readxl)
library(dplyr)
library(lubridate)

# =========================================================
# Load climate news data
# =========================================================

climate <- read_csv("Faccini_news_index.csv")

climate$date <- as.Date(climate$date)

# =========================================================
# Run PCA on daily climate indices
# =========================================================

climate_indices <- climate %>%
  select(
    `US climate policy`,
    `International summits`,
    `Global warming`,
    `Natural disasters`
  )

pca_result <- prcomp(
  climate_indices,
  center = TRUE,
  scale. = TRUE
)

climate_daily <- climate %>%
  mutate(
    PC1 = pca_result$x[, 1]
  )

climate_monthly <- climate_daily %>%
  mutate(date = floor_date(date, "month")) %>%
  group_by(date) %>%
  summarise(
    US_Climate_Policy = mean(`US climate policy`, na.rm = TRUE),
    International_Summits = mean(`International summits`, na.rm = TRUE),
    Global_Warming = mean(`Global warming`, na.rm = TRUE),
    Natural_Disasters = mean(`Natural disasters`, na.rm = TRUE),
    PC1 = mean(PC1, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# Load monthly coffee futures data
# =========================================================

coffee <- read_excel("KCc1 arabica monthly 2000-2025.xlsx")
#the raw file gets transformed simply to the version here by just by
#deleting the excess rows and columns and then keeping just the exchange date
#and the close cloumns in check.initially i just manually did it 
#but theres a code block in my python submissions that can get the raw data
#in the form needed for this
coffee <- coffee %>%
  rename(
    date = `Exchange Date`,
    close = Close
  ) %>%
  mutate(
    date = floor_date(date, "month")
  ) %>%
  arrange(date)

# =========================================================
# Calculate monthly coffee log returns
# =========================================================

coffee <- coffee %>%
  mutate(
    Coffee_Return = 100 * log(close / lag(close))
  )

# =========================================================
# Merge coffee returns with climate data
# =========================================================

merged_data <- coffee %>%
  select(date, close, Coffee_Return) %>%
  inner_join(climate_monthly, by = "date") %>%
  arrange(date)

# =========================================================
# Check merged dataset
# =========================================================

head(merged_data)
tail(merged_data)
summary(merged_data)
nrow(merged_data)

# =========================================================
# Save merged dataset
# =========================================================

write_csv(
  merged_data,
  "merged_coffee_climate_monthly.csv"
)

# =========================================================
# Load regression packages
# =========================================================

library(lmtest)
library(sandwich)

# =========================================================
# Standardise predictors
# =========================================================

reg_data <- merged_data %>%
  arrange(date) %>%
  mutate(
    US_Climate_Policy_z =
      as.numeric(scale(US_Climate_Policy)),
    
    International_Summits_z =
      as.numeric(scale(International_Summits)),
    
    Global_Warming_z =
      as.numeric(scale(Global_Warming)),
    
    Natural_Disasters_z =
      as.numeric(scale(Natural_Disasters)),
    
    PC1_z =
      as.numeric(scale(PC1))
  )

# =========================================================
# Create future returns
# =========================================================

reg_data <- reg_data %>%
  mutate(
    
    Return_1m =
      lead(Coffee_Return, 1),
    
    Return_6m =
      rowSums(
        cbind(
          lead(Coffee_Return, 1),
          lead(Coffee_Return, 2),
          lead(Coffee_Return, 3),
          lead(Coffee_Return, 4),
          lead(Coffee_Return, 5),
          lead(Coffee_Return, 6)
        ),
        na.rm = FALSE
      ),
    
    Return_12m =
      rowSums(
        cbind(
          lead(Coffee_Return, 1),
          lead(Coffee_Return, 2),
          lead(Coffee_Return, 3),
          lead(Coffee_Return, 4),
          lead(Coffee_Return, 5),
          lead(Coffee_Return, 6),
          lead(Coffee_Return, 7),
          lead(Coffee_Return, 8),
          lead(Coffee_Return, 9),
          lead(Coffee_Return, 10),
          lead(Coffee_Return, 11),
          lead(Coffee_Return, 12)
        ),
        na.rm = FALSE
      )
    
  )

# =========================================================
# Regression function
# =========================================================

run_predictive_regression <- function(
    data,
    return_var,
    predictor_var,
    nw_lag
) {
  
  formula <- as.formula(
    paste(return_var, "~", predictor_var)
  )
  
  model <- lm(formula, data = data)
  
  nw_results <- coeftest(
    model,
    vcov = NeweyWest(
      model,
      lag = nw_lag,
      prewhite = FALSE
    )
  )
  
  beta <- nw_results[predictor_var, "Estimate"]
  
  nw_t <- nw_results[predictor_var, "t value"]
  
  pval <- nw_results[predictor_var, "Pr(>|t|)"]
  
  r2 <- summary(model)$r.squared * 100
  
  data.frame(
    Horizon = return_var,
    Predictor = predictor_var,
    Beta = beta,
    NW_t = nw_t,
    P_value = pval,
    R2_percent = r2
  )
}

# =========================================================
# Define predictors and horizons
# =========================================================

predictors <- c(
  "PC1_z",
  "US_Climate_Policy_z",
  "International_Summits_z",
  "Global_Warming_z",
  "Natural_Disasters_z"
)

horizons <- data.frame(
  Return_Var = c(
    "Return_1m",
    "Return_6m",
    "Return_12m"
  ),
  NW_Lag = c(1, 6, 12)
)

# =========================================================
# Run regressions
# =========================================================

results <- data.frame()

for (i in 1:nrow(horizons)) {
  
  for (p in predictors) {
    
    temp <- run_predictive_regression(
      data = reg_data,
      return_var = horizons$Return_Var[i],
      predictor_var = p,
      nw_lag = horizons$NW_Lag[i]
    )
    
    results <- rbind(results, temp)
    
  }
  
}

# =========================================================
# Clean output table
# =========================================================

results_clean <- results %>%
  mutate(
    
    Horizon = case_when(
      Horizon == "Return_1m" ~ "1-month",
      Horizon == "Return_6m" ~ "6-month",
      Horizon == "Return_12m" ~ "12-month"
    ),
    
    Predictor = case_when(
      Predictor == "PC1_z" ~ "PC-CR",
      Predictor == "US_Climate_Policy_z" ~ "US Climate Policy",
      Predictor == "International_Summits_z" ~ "International Summits",
      Predictor == "Global_Warming_z" ~ "Global Warming",
      Predictor == "Natural_Disasters_z" ~ "Natural Disasters"
    )
    
  ) %>%
  mutate(
    Beta = round(Beta, 4),
    NW_t = round(NW_t, 2),
    P_value = round(P_value, 4),
    R2_percent = round(R2_percent, 2)
  )

# =========================================================
# View results
# =========================================================

results_clean

# =========================================================
# Save results
# =========================================================

write_csv(
  results_clean,
  "coffee_climate_regression_results.csv"
)

# =========================================================
# Checks
# =========================================================

summary(pca_result)

