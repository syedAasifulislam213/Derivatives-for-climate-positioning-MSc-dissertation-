# ============================================================
# ECONOMIC DATA PREPARATION
# ============================================================

library(readr)
library(dplyr)
library(lubridate)

# ------------------------------------------------------------
# 1. Load economic variables
# ------------------------------------------------------------

# DXY - US Dollar Index
DXY <- read_csv("DXY US dollar currency index.csv") %>%
  rename(
    date = `Exchange Date`,
    DXY = `Trade Price`
  ) %>%
  mutate(
    date = as.Date(date, format = "%d-%b-%Y")
  ) %>%
  arrange(date)


# CPI
CPI <- read_csv("CPIAUCSL.csv") %>%
  rename(
    date = observation_date,
    CPI = CPIAUCSL
  ) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(date)


# 3-Month Treasury Bill Rate
TBL <- read_csv("3 mo treasury bills.csv") %>%
  rename(
    date = observation_date,
    TBL = TB3MS
  ) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(date)


# 10-Year Treasury Yield
GS10 <- read_csv("10 year treasury yield.csv") %>%
  rename(
    date = observation_date,
    GS10 = GS10
  ) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(date)


# Industrial Production
IP <- read_csv("industrial production.csv") %>%
  rename(
    date = observation_date,
    IP = INDPRO
  ) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(date)


# Kilian / Global Real Economic Activity Index
KI <- read_csv("IGREA.csv") %>%
  rename(
    date = observation_date,
    KI = IGREA
  ) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(date)


# ------------------------------------------------------------
# 2. Put all economic variables on monthly dates
# ------------------------------------------------------------

DXY <- DXY %>%
  mutate(month = floor_date(date, "month"))

CPI <- CPI %>%
  mutate(month = floor_date(date, "month"))

TBL <- TBL %>%
  mutate(month = floor_date(date, "month"))

GS10 <- GS10 %>%
  mutate(month = floor_date(date, "month"))

IP <- IP %>%
  mutate(month = floor_date(date, "month"))

KI <- KI %>%
  mutate(month = floor_date(date, "month"))


# ------------------------------------------------------------
# 3. Merge economic variables
# ------------------------------------------------------------

macro_data <- DXY %>%
  select(month, DXY) %>%
  inner_join(
    CPI %>% select(month, CPI),
    by = "month"
  ) %>%
  inner_join(
    TBL %>% select(month, TBL),
    by = "month"
  ) %>%
  inner_join(
    GS10 %>% select(month, GS10),
    by = "month"
  ) %>%
  inner_join(
    IP %>% select(month, IP),
    by = "month"
  ) %>%
  inner_join(
    KI %>% select(month, KI),
    by = "month"
  ) %>%
  rename(date = month) %>%
  arrange(date)


# ------------------------------------------------------------
# 4. Ensure Step 1 regression data has monthly dates
# ------------------------------------------------------------

reg_data <- reg_data %>%
  mutate(
    date = floor_date(date, "month")
  )


# ------------------------------------------------------------
# 5. Merge economic variables with Step 1 data
# ------------------------------------------------------------

reg_data_econ <- reg_data %>%
  inner_join(
    macro_data,
    by = "date"
  ) %>%
  arrange(date)


# ------------------------------------------------------------
# 6. Create transformed economic controls
# ------------------------------------------------------------

reg_data_econ <- reg_data_econ %>%
  mutate(
    # Monthly inflation rate
    INFL = 100 * (log(CPI) - log(lag(CPI))),
    
    # Monthly industrial production growth
    IP_growth = 100 * (log(IP) - log(lag(IP)))
  )


# ------------------------------------------------------------
# 7. Standardise all economic controls
# ------------------------------------------------------------

reg_data_econ <- reg_data_econ %>%
  mutate(
    DXY_z       = as.numeric(scale(DXY)),
    INFL_z      = as.numeric(scale(INFL)),
    TBL_z       = as.numeric(scale(TBL)),
    GS10_z      = as.numeric(scale(GS10)),
    IP_growth_z = as.numeric(scale(IP_growth)),
    KI_z        = as.numeric(scale(KI))
  )


# ------------------------------------------------------------
# 8. Checks
# ------------------------------------------------------------

names(reg_data_econ)

nrow(reg_data_econ)

summary(
  reg_data_econ %>%
    select(
      PC1_z,
      Return_1m,
      Return_6m,
      Return_12m,
      DXY_z,
      INFL_z,
      TBL_z,
      GS10_z,
      IP_growth_z,
      KI_z
    )
)

# Predictive regressions with economic controls
# ============================================================

library(dplyr)
library(purrr)
library(tibble)
library(lmtest)
library(sandwich)

# ------------------------------------------------------------
# Function to estimate one predictive regression
# ------------------------------------------------------------

run_control_regression <- function(
    data,
    return_variable,
    control_variable,
    control_label,
    nw_lag
) {
  
  required_variables <- c(
    return_variable,
    "PC1_z",
    control_variable
  )
  
  # Keep only complete observations for this particular model
  model_data <- data %>%
    select(all_of(required_variables)) %>%
    filter(if_all(everything(), ~ !is.na(.x)))
  
  if (nrow(model_data) == 0) {
    stop(
      paste(
        "No complete observations are available for",
        return_variable,
        "and",
        control_variable
      )
    )
  }
  
  # Full model: PC-CR plus one economic control
  full_formula <- as.formula(
    paste(
      return_variable,
      "~ PC1_z +",
      control_variable
    )
  )
  
  full_model <- lm(
    formula = full_formula,
    data = model_data
  )
  
  # Newey-West covariance matrix
  full_vcov <- NeweyWest(
    full_model,
    lag = nw_lag,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  full_test <- coeftest(
    full_model,
    vcov. = full_vcov
  )
  
  # Economic-control-only model
  economic_formula <- as.formula(
    paste(
      return_variable,
      "~",
      control_variable
    )
  )
  
  economic_model <- lm(
    formula = economic_formula,
    data = model_data
  )
  
  # Extract PC-CR results
  beta_pc_cr <- full_test["PC1_z", "Estimate"]
  nw_t_pc_cr <- full_test["PC1_z", "t value"]
  p_pc_cr <- full_test["PC1_z", "Pr(>|t|)"]
  
  # Extract economic-control results
  beta_control <- full_test[control_variable, "Estimate"]
  nw_t_control <- full_test[control_variable, "t value"]
  p_control <- full_test[control_variable, "Pr(>|t|)"]
  
  # R-squared values expressed as percentages
  r2_full <- summary(full_model)$r.squared * 100
  r2_economic_only <- summary(economic_model)$r.squared * 100
  
  # Incremental explanatory power from adding PC-CR
  delta_r2 <- r2_full - r2_economic_only
  
  tibble(
    Control = control_label,
    Beta_PC_CR = beta_pc_cr,
    NW_t_PC_CR = nw_t_pc_cr,
    P_PC_CR = p_pc_cr,
    Beta_Control = beta_control,
    NW_t_Control = nw_t_control,
    P_Control = p_control,
    R2_percent = r2_full,
    R2_econ_only = r2_economic_only,
    Delta_R2 = delta_r2,
    Observations = nobs(full_model)
  )
}

economic_controls <- tribble(
  ~variable,       ~label,
  "TBL_z",         "TBL",
  "KI_z",          "KI",
  "IP_growth_z",   "IP",
  "INFL_z",        "INFL",
  "GS10_z",        "GS10",
  "DXY_z",         "DXY"
)


results_1m <- map2_dfr(
  economic_controls$variable,
  economic_controls$label,
  ~ run_control_regression(
    data = reg_data_econ,
    return_variable = "Return_1m",
    control_variable = .x,
    control_label = .y,
    nw_lag = 1
  )
)

results_1m


results_6m <- map2_dfr(
  economic_controls$variable,
  economic_controls$label,
  ~ run_control_regression(
    data = reg_data_econ,
    return_variable = "Return_6m",
    control_variable = .x,
    control_label = .y,
    nw_lag = 6
  )
)

results_6m


results_12m <- map2_dfr(
  economic_controls$variable,
  economic_controls$label,
  ~ run_control_regression(
    data = reg_data_econ,
    return_variable = "Return_12m",
    control_variable = .x,
    control_label = .y,
    nw_lag = 12
  )
)

results_12m

table_1m <- results_1m %>%
  transmute(
    Control,
    `Beta PC-CR` = round(Beta_PC_CR, 4),
    `NW-t PC-CR` = round(NW_t_PC_CR, 2),
    `p-value PC-CR` = round(P_PC_CR, 4),
    `Beta Control` = round(Beta_Control, 4),
    `NW-t Control` = round(NW_t_Control, 2),
    `R2 (%)` = round(R2_percent, 2),
    `Delta R2` = round(Delta_R2, 2)
  )

table_6m <- results_6m %>%
  transmute(
    Control,
    `Beta PC-CR` = round(Beta_PC_CR, 4),
    `NW-t PC-CR` = round(NW_t_PC_CR, 2),
    `p-value PC-CR` = round(P_PC_CR, 4),
    `Beta Control` = round(Beta_Control, 4),
    `NW-t Control` = round(NW_t_Control, 2),
    `R2 (%)` = round(R2_percent, 2),
    `Delta R2` = round(Delta_R2, 2)
  )

table_12m <- results_12m %>%
  transmute(
    Control,
    `Beta PC-CR` = round(Beta_PC_CR, 4),
    `NW-t PC-CR` = round(NW_t_PC_CR, 2),
    `p-value PC-CR` = round(P_PC_CR, 4),
    `Beta Control` = round(Beta_Control, 4),
    `NW-t Control` = round(NW_t_Control, 2),
    `R2 (%)` = round(R2_percent, 2),
    `Delta R2` = round(Delta_R2, 2)
  )

print(table_1m)
print(table_6m)
print(table_12m)

table_1m <- results_1m %>%
  transmute(
    `Economic control included alongside PC-CR` = Control,
    `PC-CR coefficient` = round(Beta_PC_CR, 4),
    `PC-CR NW-t` = round(NW_t_PC_CR, 2),
    `PC-CR p-value` = round(P_PC_CR, 4),
    `Control coefficient` = round(Beta_Control, 4),
    `Control NW-t` = round(NW_t_Control, 2),
    `Full-model R2 (%)` = round(R2_percent, 2),
    `Incremental R2 from PC-CR` = round(Delta_R2, 2)
  )


