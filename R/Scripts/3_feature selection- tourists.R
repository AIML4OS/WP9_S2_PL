library(readr)
library(broom)
library(dplyr)
library(glmnet)


# Load data
features <- tourists_features <- read_csv("synthetic data/tourists_features.csv")

# Remove index column
features$...1<-NULL


# Select predictive features by excluding target, identifiers, and aggregated features
features_to_use <- features[, !names(features) %in% c("D8R1", "RO", "WON", "POW", "GMN", "KKR","D8R1_mean_gmn","D8R1_mean_pow",    
                                                      "D8R1_mean_won","D8R1_quarter_mean","mean_3m","mean_6m","mean_9m","mean_12m")]

# Target variable
target <- features$D8R1


# List to store selected LASSO variables for each KKR
lasso_vars_per_kkr <- list()

## Step 1: Variable Selection using LASSO for each KKR group
for(kkr_val in unique(features$KKR)) {
  
  # Filter data for the current KKR group
  df_kkr <- features %>% filter(KKR == kkr_val)
  
  # Remove rows where y (D8R1) is NA
  df_kkr <- df_kkr %>% filter(!is.na(D8R1))
  
  y <- df_kkr$D8R1
  
  # Skip groups where D8R1 is constant (necessary for GLM/LM)
  if(length(unique(y)) < 2) next
  
  # Select predictors, excluding identifiers and target/lag variables
  x <- df_kkr %>% select(-c(D8R1, REGON, OKRES, RO, WON, POW, GMN, KKR, date))
  # Keep only numeric variables
  numeric_vars <- sapply(x, is.numeric)
  x <- x[, numeric_vars]
  
  # Impute NAs with the median for predictors
  x <- as.data.frame(lapply(x, function(col) {
    col[is.na(col)] <- median(col, na.rm = TRUE)
    return(col)
  }))
  
  x <- as.matrix(x)
  
  # Check if data is sufficient
  if(ncol(x) == 0 || nrow(x) < 5) next
  
  # LASSO with cross-validation (alpha = 1)
  lasso_model <- cv.glmnet(x, y, alpha = 1)
  # Get coefficients for lambda.min
  coef_lasso <- coef(lasso_model, s = "lambda.min")
  
  # Get names of non-zero coefficients (selected variables)
  selected_vars <- rownames(coef_lasso)[which(coef_lasso != 0)]
  selected_vars <- selected_vars[selected_vars != "(Intercept)"]
  
  # Store selected variables
  lasso_vars_per_kkr[[as.character(kkr_val)]] <- selected_vars
}

print(lasso_vars_per_kkr)

# The second block in your original code, which was commented as 'model liniowy z regularyzacja', 
# seems to be intended for fitting OLS regression models based on the LASSO selection.

# I am combining the two separate LM blocks and fixing the variable name.

## Step 2: Fitting OLS Linear Models (lm) using LASSO selected variables
# List to store OLS linear models for each KKR
lm_models_per_kkr <- list()

# Use the list 'lasso_vars_per_kkr' generated in Step 1
for(kkr_val in unique(features$KKR)) {
  
  df_kkr <- features %>% filter(KKR == kkr_val)
  
  # Check if selected variables exist for this group in the LASSO result list
  # *** LOGICAL FIX: Changed 'selected_vars_per_kkr' to 'lasso_vars_per_kkr' ***
  if(!(as.character(kkr_val) %in% names(lasso_vars_per_kkr))) next
  vars <- lasso_vars_per_kkr[[as.character(kkr_val)]]
  
  # Remove rows where D8R1 is constant or missing
  df_kkr <- df_kkr %>% filter(!is.na(D8R1))
  if(length(unique(df_kkr$D8R1)) < 2) next
  
  # Check if there are predictive variables selected by LASSO
  if(length(vars) == 0) next
  
  # Build the linear model formula
  formula <- as.formula(paste("D8R1 ~", paste(vars, collapse = " + ")))
  # Fit the OLS linear model
  lm_model <- lm(formula, data = df_kkr)
  
  # Store the model in the list
  lm_models_per_kkr[[as.character(kkr_val)]] <- lm_model
}

## saving my results


# --- Collecting Summary Results from all Linear Models ---

# 1. Extraction of Coefficients

# List to store tidied coefficient data frames
all_coefficients_list <- list()

for (kkr_val in names(lm_models_per_kkr)) {
  model <- lm_models_per_kkr[[kkr_val]]
  
  # Use tidy() to extract coefficients, SE, t-stat, and p-values
  if (!is.null(model)) {
    tidy_df <- tidy(model)
    
    # Add the KKR column (group identifier)
    tidy_df <- tidy_df %>% 
      mutate(KKR = as.character(kkr_val), .before = term)
    
    all_coefficients_list[[kkr_val]] <- tidy_df
  }
}

# Combine all data frames into one table
coefficients_df <- bind_rows(all_coefficients_list)

# Preview results
print("Coefficients Data Frame:")
print(head(coefficients_df))


# 2. Extraction of Model Fit Metrics (Goodness-of-Fit)

# List to store tidied model metrics data frames
all_model_metrics_list <- list()

for (kkr_val in names(lm_models_per_kkr)) {
  model <- lm_models_per_kkr[[kkr_val]]
  
  # Use glance() to extract R-squared, F-statistic, etc.
  if (!is.null(model)) {
    glance_df <- glance(model)
    
    # Add the KKR column (group identifier)
    glance_df <- glance_df %>% 
      mutate(KKR = as.character(kkr_val), .before = r.squared)
    
    all_model_metrics_list[[kkr_val]] <- glance_df
  }
}

# Combine all data frames into one table
model_metrics_df <- bind_rows(all_model_metrics_list)



# Preview results
print("Model Fit Metrics Data Frame:")
print(head(model_metrics_df))
