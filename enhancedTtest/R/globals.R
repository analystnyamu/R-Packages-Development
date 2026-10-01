# =============================================================================
# GLOBAL VARIABLE DECLARATIONS
# File: R/globals.R
# =============================================================================
# This file declares variables used in non-standard evaluation contexts
# (e.g., inside dplyr::mutate, ggplot2::aes) to prevent R CMD check NOTEs
# about "no visible binding for global variable".

utils::globalVariables(c(
  # Common variable names in statistical computations
  ".",
  "N",
  "n",
  "Mean",
  "SD",
  "SE",
  "Median",
  "IQR",
  "Q1",
  "Q3",
  "Min",
  "Max",
  "group",
  "value",
  "variable",
  "statistic",
  "result",
  "estimate",
  "conf.low",
  "conf.high",
  "p.value",
  "parameter",
  "method",
  "alternative",
  
  # dplyr NSE variables
  "starts_with",
  "ends_with",
  "contains",
  
  # Internal column names
  "value_col",
  "group_col",
  "response_var",
  "group_var",
  
  # For the %>% operator
  "%>%"
))