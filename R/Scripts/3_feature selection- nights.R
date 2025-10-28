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

