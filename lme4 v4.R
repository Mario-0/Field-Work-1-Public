###### packages and config ######

library(lme4)
library(tidyverse) # data wrangling and visualization
library(readr)
library(tidyr)
library(dplyr)
library(mice)
library(corrplot)  # For visualization
library(caret)     # For correlation-related functions (optional)
library(car)
library(broom.mixed)
#library(rsample)
library(boot)
library(sjPlot)
library(flextable)
library(broom.mixed)
library(performance)
library(stringr)
library(pROC)
library(Metrics)   # for logLoss()

options(scipen = 999)

###### load data ######

#change to appropriate working directory
setwd("SET WORKING FOLDER HERE")
df <- read_csv("SET RATINGS+FEATURES FILE HERE")

###### weighted sampling and rating mapping ######

# Remove rows with rating 0
df <- df[df$rating != 0, ]

# Map rating -1 to 0
df$rating[df$rating == -1] <- 0

# Compute rating weight
df$rating_weight <- 1 / ave(df$p_id, df$p_id, FUN=length)

# Set desired sample size (adjust as needed)
sample_size <- 600  # Example: selecting 500 samples

# Perform weighted random sampling
set.seed(42)
df_sampled <- df[sample(1:nrow(df), size=sample_size, prob=df$rating_weight, replace=FALSE), ]

###### weighted sampling and rating mapping VERSION 2 - EQUAL 1:0 ######

# --- Remove neutral ratings ---
df <- df[df$rating != 0, ]

# --- Map rating -1 to 0 ---
df$rating[df$rating == -1] <- 0

# --- Split into negative and positive classes ---
df_neg <- df[df$rating == 0, ]
df_pos <- df[df$rating == 1, ]

# --- Calculate inverse frequency weights for each participant ---
df$rating_weight <- 1 / ave(df$p_id, df$p_id, FUN = length)

# --- Apply weights only to positive class ---
df_pos$rating_weight <- 1 / ave(df_pos$p_id, df_pos$p_id, FUN = length)

# --- Define sample size equal to number of negatives ---
sample_size <- nrow(df_neg)

# --- Sample from positives using weights ---
set.seed(42)
df_pos_sampled <- df_pos[sample(1:nrow(df_pos), size = sample_size, 
                                prob = df_pos$rating_weight, replace = FALSE), ]

# --- Remove 'rating_weight' column if present ---
df_neg$rating_weight <- NULL
df_pos_sampled$rating_weight <- NULL

# --- Combine into a balanced dataset ---
df_balanced <- rbind(df_neg, df_pos_sampled)

# --- Reset row names ---
rownames(df_balanced) <- NULL

df_sampled = df_balanced

###### map age ad gender

# Rename the column
names(df_sampled)[names(df_sampled) == "sociodem_age"] <- "sociodem_age_original"
names(df_sampled)[names(df_sampled) == "sociodem_gender"] <- "sociodem_gender_original"

# Define the mapping
age_mapping <- c(
  "<25" = 20,
  "25-35" = 30,
  "36-45" = 40,
  "46-55" = 50,
  "56-65" = 60,
  "66-75" = 70,
  "75<" = 80
)

gender_mapping <- c("Female" = 0, "Male" = 1)

# Apply the mapping to create the new column
df_sampled$sociodem_age <- age_mapping[df_sampled$sociodem_age_original]
df_sampled$sociodem_gender <- gender_mapping[df_sampled$sociodem_gender_original]

###### impute missing values ######

# Function to impute missing values with the median for all continuous variables
impute_missing_values_median <- function(df) {
  continuous_vars <- c("interval_id", "warnings_slowdown_count", "H10_hr_mean", "H10_hrv_mean", 
                       "H10_HRV_MeanNN", "E4_eda_tonic_mean", "E4_eda_phasic_mean", "E4_eda_tonic_peaks", 
                       "E4_eda_phasic_peaks", "cadence_avg", "cadence_pvr", "velocity_avg", "velocity_slope", 
                       "wind_speed", "temperature", "BMI", "pleasantnessdisposition_mapped", 
                       "FB_hinder_mapping", "FB_wegtype_mapping", "FB_wegkwal_mapping")
  
  # Loop through each continuous variable and impute missing values with the median
  for (var in continuous_vars) {
    df[[var]][is.na(df[[var]])] <- median(df[[var]], na.rm = TRUE)
  }
  
  return(df)
}

# Apply the median imputation function to your dataset
df_imputed_median <- impute_missing_values_median(df_sampled)

unique(df_imputed_median$sociodem_gender)

###### run single model ######

# gives error p_id invalid grouping factor: sociodem_fitness 
# drops random effect to 0: context_perceivedinfluence
# drops random effect to very low: weather_wind_speed +  weather_temperature +

# Define your predictors as a character vector
predictors <- c(
  "rating_totalperpid", "H10_hr_mean", "H10_hrv_mean", "H10_HRV_RMSSD", "H10_HRV_MeanNN", "H10_HRV_HF", 
  "E4_eda_phasic_mean", "E4_eda_phasic_peaks", "E4_eda_phasic_max", 
  "E4_eda_phasic_n_above_mean", "E4_eda_tonic_mean", "E4_eda_tonic_peaks", "E4_eda_tonic_n_above_mean", 
  "cadence_avg", "velocity_avg", "velocity_avg_change", "warnings_slowdown_count", "warnings_tactile_warning", 
  "warnings_audio_warning", "warnings_warning_value", "sociodem_income", "sociodem_education", 
  "context_surface_type", "context_road_quality", 
  "context_scenic_beauty", "context_hindrance", "context_road_type", "sociodem_mood", 
  "sociodem_pleasantness_disposition", "cycling_perceivedintensity"
)

# Paste predictors into a formula string, Add outcome and random effect to create full formula, Convert to formula object
model_formula <- as.formula(paste("rating ~", paste(predictors, collapse = " + "), "+ (1 | p_id)"))

# Fit the model
model <- glmer(  formula = model_formula, data = df_imputed_median, family = binomial, control = glmerControl(optimizer = "bobyqa"))

# Display summary
summary(model)

###### run model 10 fold CV, with conf intervals, without p_id RE ######

# Create 10 group-level folds using caret's groupKFold
folds <- groupKFold(group = df_imputed_median$p_id, k = 10)

# Store results
coef_list <- list()

for (i in seq_along(folds)) {
  cat("Running fold", i, "...\n")
  train_idx <- folds[[i]]
  train_data <- df_imputed_median[train_idx, ]
  
  model <- tryCatch({
    glmer(model_formula, data = train_data, family = binomial, control = glmerControl(optimizer = "bobyqa"))
  }, error = function(e) NULL)
  
  if (!is.null(model)) {
    tidy_model <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE)
    tidy_model$fold <- i
    coef_list[[i]] <- tidy_model
  }
}

# Combine all fold results
coef_df <- bind_rows(coef_list)

# Summarize: mean estimate, p-value, and confidence intervals across folds
summary_df <- coef_df %>%
  group_by(term) %>%
  summarise(
    mean_estimate = mean(estimate, na.rm = TRUE),
    mean_p_value = mean(p.value, na.rm = TRUE),
    mean_conf_low = mean(conf.low, na.rm = TRUE),
    mean_conf_high = mean(conf.high, na.rm = TRUE)
  ) %>%
  arrange(mean_p_value)

# Show results
print(summary_df)
