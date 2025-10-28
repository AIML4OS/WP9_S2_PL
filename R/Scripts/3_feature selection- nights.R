library(readr)
library(dplyr)
library(glmnet)
library(Metrics)  # for MAE, RMSE

# Load data
features <- read_csv("synthetic data/nights_features.csv")
features$...1 <- NULL

# ----- LASSO variable selection per KKR for D8R2 -----
lasso_vars_per_kkr_d8r2 <- list()

for(kkr_val in unique(features$KKR)) {
  
  # Filter data for current KKR and remove missing target values
  df_kkr <- features %>%
    filter(KKR == kkr_val) %>%
    filter(!is.na(D8R2))
  
  y <- df_kkr$D8R2
  
  # Skip if dependent variable is constant
  if(length(unique(y)) < 2) next
  
  # Select numeric predictors only
  x <- df_kkr %>%
    select(-c(D8R2, REGON, OKRES, RO, WON, POW, GMN, KKR, date))
  numeric_vars <- sapply(x, is.numeric)
  x <- x[, numeric_vars]
  
  # Median imputation for missing predictor values
  x <- as.data.frame(lapply(x, function(col) {
    col[is.na(col)] <- median(col, na.rm = TRUE)
    return(col)
  }))
  
  # Remove predictors with zero variance
  x <- x[, apply(x, 2, var, na.rm = TRUE) != 0]
  
  # Skip if no valid predictors or too few rows
  if(ncol(x) == 0 || nrow(x) < 5) next
  
  x <- as.matrix(x)
  
  # LASSO regression with cross-validation
  lasso_model <- cv.glmnet(x, y, alpha = 1)
  coef_lasso <- coef(lasso_model, s = "lambda.min")
  
  # Extract non-zero coefficient variables
  selected_vars <- rownames(coef_lasso)[which(coef_lasso != 0)]
  selected_vars <- selected_vars[selected_vars != "(Intercept)"]
  
  # Save selected variables for this KKR
  lasso_vars_per_kkr_d8r2[[as.character(kkr_val)]] <- selected_vars
}

# ----- Build linear regression models for each KKR -----
lm_models_per_kkr_d8r2 <- list()

for(kkr_val in unique(features$KKR)) {
  
  df_kkr <- features %>%
    filter(KKR == kkr_val) %>%
    filter(!is.na(D8R2))
  
  # Skip if no selected variables
  if(!(as.character(kkr_val) %in% names(lasso_vars_per_kkr_d8r2))) next
  
  vars <- lasso_vars_per_kkr_d8r2[[as.character(kkr_val)]]
  
  # Skip if target variable is constant or no predictors
  if(length(unique(df_kkr$D8R2)) < 2 || length(vars) == 0) next
  
  # Build the linear model
  formula <- as.formula(paste("D8R2 ~", paste(vars, collapse = " + ")))
  lm_model <- lm(formula, data = df_kkr)
  
  # Save model
  lm_models_per_kkr_d8r2[[as.character(kkr_val)]] <- lm_model
}

# Example summary for KKR = 203
summary(lm_models_per_kkr_d8r2[["203"]])

# ----- Combine regression results into one data frame -----
model_results_d8r2 <- lapply(names(lm_models_per_kkr_d8r2), function(kkr_val) {
  
  model <- lm_models_per_kkr_d8r2[[kkr_val]]
  if (is.null(model)) return(NULL)
  
  s <- summary(model)
  
  df <- as.data.frame(s$coefficients)
  df$variable <- rownames(df)
  df$KKR <- kkr_val
  
  colnames(df) <- c("estimate", "std_error", "t_value", "p_value", "variable", "KKR")
  
  return(df)
})

model_results_df_d8r2 <- do.call(rbind, model_results_d8r2)

model_results_df_d8r2 <- model_results_df_d8r2 %>%
  select(KKR, variable, estimate, std_error, t_value, p_value)

# Preview results
head(model_results_df_d8r2)


# ----- Predictions and error metrics -----
prediction_errors_d8r2 <- data.frame()

for (kkr_val in names(lm_models_per_kkr_d8r2)) {
  
  model <- lm_models_per_kkr_d8r2[[kkr_val]]
  if (is.null(model)) next
  
  # Subset the data for this KKR
  df_kkr <- features %>%
    filter(KKR == as.numeric(kkr_val)) %>%
    filter(!is.na(D8R2))
  
  vars <- all.vars(formula(model))[-1]
  
  df_pred <- df_kkr[, vars, drop = FALSE]
  
  # Median imputation
  df_pred <- as.data.frame(lapply(df_pred, function(col) {
    col[is.na(col)] <- median(col, na.rm = TRUE)
    return(col)
  }))
  
  # Make predictions
  y_true <- df_kkr$D8R2
  y_pred <- predict(model, newdata = df_pred)
  
  # Compute errors
  mae_val <- mae(y_true, y_pred)
  rmse_val <- rmse(y_true, y_pred)
  r2_val <- cor(y_true, y_pred)^2
  
  prediction_errors_d8r2 <- rbind(prediction_errors_d8r2, data.frame(
    KKR = kkr_val,
    n_obs = length(y_true),
    MAE = mae_val,
    RMSE = rmse_val,
    R2_pred = r2_val
  ))
}

# View prediction performance
prediction_errors_d8r2



# Interpretation of Prediction Results for D8R2
# 
# The linear regression models built separately for each KKR to predict D8R2 show low predictive performance across all regions.
# 
# MAE (Mean Absolute Error) ranges from ~32 to ~121, indicating that, on average, predictions deviate from the true values by these amounts.
# 
# RMSE (Root Mean Squared Error) is substantially higher, ranging from ~130 to ~364, showing that larger errors occur frequently and the model cannot reliably predict extreme values.
# 
# R²_pred values are extremely low (0.007–0.19), meaning that the models explain very little of the variability in the actual D8R2 values. Most KKRs have R² near zero, and even the best-performing region (KKR 276) only explains about 19% of the variance.
# 
# Key takeaways:
#   
#   Linear models are inadequate for D8R2 prediction.
# The extremely low R² values indicate that the relationships between predictors and D8R2 are not well captured by a linear model.
# 
# Errors are large relative to typical values.
# High MAE and RMSE suggest that the models’ point predictions are often far from the true values, making them unreliable for operational forecasting.
# 
# Significant heterogeneity between regions.
# Some regions (e.g., KKR 276) show slightly better predictive performance, while others are almost random, indicating that the linear relationship differs widely across KKRs.
# 
# Potential reasons for poor performance:
#   
#   Nonlinear relationships or interactions between variables.
# 
# Seasonality or temporal trends not included in the model.
# 
# Important predictors missing (e.g., events, promotions, weather, tourism capacity).
# 
# High variability or stochasticity in D8R2 across regions.
# 
# Recommended actions:
#   
#   To improve forecasts of D8R2:
#   
#   Use nonlinear models (Random Forest, Gradient Boosting, GAMs).
# 
# Include temporal and seasonal features (months, quarters, lagged values).
# 
# Consider region-specific models or hierarchical/mixed-effect models to account for differences between KKRs.
# 
# Explore additional predictors that may strongly influence D8R2.