# Models clean 

# Models

library(ggplot2)
library(scales)
library(lme4)
library(tidyverse)

rm(list=ls())

############# PERFECT DETECTION

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 
ex_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_complete.csv", sep = ",", header = TRUE) 

# Convert categorical variables to factors
ex_data$Site.type <- as.factor(ex_data$Site.type)
cs_data$Site.type <- as.factor(cs_data$Site.type)

# For cs_data, keep only Watercontactsites with at least 20 observations
cs_data_filtered <- cs_data %>%
  group_by(Watercontactsite) %>%
  filter(n() >= 20) %>%
  ungroup()

# For ex_data, keep only Watercontactsites with at least 10 observations
ex_data_filtered <- ex_data %>%
  group_by(Watercontactsite) %>%
  filter(n() >= 10) %>%
  ungroup()

# Identify variables to scale and combine across datasets
NDVI = c(cs_data_filtered$NDVI_q90_t, ex_data_filtered$NDVI_q90_t)
TEMP = c(cs_data_filtered$temp_modis, ex_data_filtered$temp_modis)
PP   = c(cs_data_filtered$pp_oneweek, ex_data_filtered$pp_oneweek)
FA   = c(cs_data_filtered$flow_acc, ex_data_filtered$flow_acc)
ELEV = c(cs_data_filtered$elev, ex_data_filtered$elev)
SS   = c(cs_data_filtered$SS, ex_data_filtered$SS)

# Compute overall means and standard deviations
means <- c(mean(NDVI, na.rm = TRUE),
           mean(TEMP, na.rm = TRUE),
           mean(PP, na.rm = TRUE),
           mean(FA, na.rm = TRUE),
           mean(ELEV, na.rm = TRUE),
           mean(SS, na.rm = TRUE))

sds <- c(sd(NDVI, na.rm = TRUE),
         sd(TEMP, na.rm = TRUE),
         sd(PP, na.rm = TRUE),
         sd(FA, na.rm = TRUE),
         sd(ELEV, na.rm = TRUE),
         sd(SS, na.rm = TRUE))

# Create a named vector for easy access
scaling_params <- data.frame(variable = c("NDVI_q90_t", "temp_modis", "pp_oneweek", "flow_acc", "elev", "SS"),
                             mean = means,
                             sd = sds)

# Function to scale data based on precomputed mean and SD
scale_with_params <- function(data, scaling_params) {
  data %>%
    mutate(across(all_of(scaling_params$variable), 
                  ~ (. - scaling_params$mean[scaling_params$variable == cur_column()]) / 
                    scaling_params$sd[scaling_params$variable == cur_column()]))
}

# Apply scaling to each dataset
ex_data_model <- scale_with_params(ex_data_filtered, scaling_params)
cs_data_model <- scale_with_params(cs_data_filtered, scaling_params)

# GLMMs

# Convert categorical variables to factors
ex_data_model$Watercontactsite <- as.factor(ex_data_model$Watercontactsite)
cs_data_model$Watercontactsite <- as.factor(cs_data_model$Watercontactsite)

rm(list = setdiff(ls(), c('ex_data_model','cs_data_model')))

library(MuMIn)

# General model - Land

# Define the control parameters

control_params <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 5e5))

# Full formula 

full_formula <- bio_pres ~ pp_oneweek + temp_modis + Site.type * NDVI_q90_t + elev + flow_acc + SS

# Extract all fixed effect terms from the full model (without the interaction term)
fixed_effects <- c("pp_oneweek", "temp_modis", "Site.type", "NDVI_q90_t", "elev", "flow_acc", "SS")

# Function to generate all subsets of a set (without the empty set)
generate_combinations <- function(effects) {
  unlist(lapply(1:length(effects), function(k) combn(effects, k, simplify = FALSE)), recursive = FALSE)
}


# Generate all combinations of the fixed effects
combinations <- generate_combinations(fixed_effects)

# Initialize storage for AIC, R2 values, and formulas
results <- data.frame(Formula = character(length(combinations)),
                      AIC = numeric(length(combinations)),
                      R2_m = numeric(length(combinations)),
                      R2_c = numeric(length(combinations)),
                      Significant_Vars = character(length(combinations)),  # Store significant variables
                      stringsAsFactors = FALSE)

# Loop over all combinations and fit models
for (i in seq_along(combinations)) {
  
  # Create the fixed effects formula part
  fixed_formula <- paste(combinations[[i]], collapse = " + ")
  
  # If interaction term is present in the combination, include it
  if (all(c("Site.type", "NDVI_q90_t") %in% combinations[[i]])) {
    fixed_formula <- paste(fixed_formula, "+ Site.type * NDVI_q90_t")
  }
  
  # Create the full formula for the model
  formula <- as.formula(paste("bio_pres ~", fixed_formula, "+ (1 | Watercontactsite)"))
  
  # Fit the model
  model <- glmer(formula, data = ex_data_model, family = binomial, control = control_params)
  
  # Store the formula, AIC, and R2 values
  results$Formula[i] <- paste(deparse(formula), collapse = " ")
  results$AIC[i] <- AIC(model)
  
  # Calculate R2 values
  r2_values <- r.squaredGLMM(model)
  results$R2_m[i] <- r2_values[1]
  results$R2_c[i] <- r2_values[3]
  
  # Extract p-values and identify significant variables
  pvals <- summary(model)$coefficients[, "Pr(>|z|)"]  # Extract p-values for fixed effects
  significant_vars <- names(pvals)[pvals < 0.05]  # Variables with p-value < 0.05
  
  # Store significant variables as a concatenated string
  results$Significant_Vars[i] <- ifelse(length(significant_vars) > 0, paste(significant_vars, collapse = ", "), "None")
}

# Sort the results by AIC from smallest to largest
results <- results %>% arrange(AIC)

# Save expert output 

write.csv(results,'~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_expert_models_complete.csv')

# Selected model for the expert 

# Model 11

model_expert <- glmer(bio_pres ~ pp_oneweek + Site.type*NDVI_q90_t + 
                        elev + (1 | Watercontactsite),
                      data = ex_data_model, family = binomial,
                      control = control_params)

summary(model_expert)

full_model_expert = glmer(bio_pres ~ pp_oneweek + temp_modis + Site.type * NDVI_q90_t + elev + flow_acc + SS + (1 | Watercontactsite),
      data = ex_data_model, family = binomial,
      control = control_params)

# Fit models
model_with_interaction <- glmer(bio_pres ~ pp_oneweek + Site.type * NDVI_q90_t + elev + (1 | Watercontactsite), 
                                family = binomial, data = ex_data_model ,
                                control = control_params)
model_without_interaction <- glmer(bio_pres ~ pp_oneweek + Site.type + NDVI_q90_t + elev + (1 | Watercontactsite), 
                                   family = binomial, data = ex_data_model,
                                   control = control_params)

# Likelihood Ratio Test
anova(model_with_interaction, model_without_interaction)



library(gtsummary)
library(broom.mixed)

# format results into data frame with global p-values

model_expert |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      pp_oneweek ~ "Precipitation",
      Site.type ~ "Site type",
      NDVI_q90_t ~ "NDVI",
      elev ~ "Elevation"
    )
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

model_expert |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    intercept = T,
    label = list(
      pp_oneweek ~ "Precipitation",
      Site.type ~ "Site type",
      NDVI_q90_t ~ "NDVI",
      elev ~ "Elevation"
    )
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

full_model_expert |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    intercept = T,
    label = list(
      pp_oneweek ~ "Precipitation",
      Site.type ~ "Site type",
      NDVI_q90_t ~ "NDVI",
      elev ~ "Elevation"
    )
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

# Citizen scientists land models 

# Initialize storage for AIC, R2 values, and formulas
results_cs <- data.frame(Formula = character(length(combinations)),
                         AIC = numeric(length(combinations)),
                         R2_m = numeric(length(combinations)),
                         R2_c = numeric(length(combinations)),
                         Significant_Vars = character(length(combinations)),  # Store significant variables
                         stringsAsFactors = FALSE)

# Loop over all combinations and fit models
for (i in seq_along(combinations)) {
  # Create the fixed effects formula part
  fixed_formula <- paste(combinations[[i]], collapse = " + ")
  
  # If interaction term is present in the combination, include it
  if (all(c("Site.type", "NDVI_q90_t") %in% combinations[[i]])) {
    fixed_formula <- paste(fixed_formula, "+ Site.type * NDVI_q90_t")
  }
  
  # Create the full formula for the model
  formula <- as.formula(paste("bio_pres ~", fixed_formula, "+ (1 | Watercontactsite)"))
  
  # Fit the model
  model <- glmer(formula, data = cs_data_model, family = binomial, control = control_params)
  
  # Store the formula, AIC, and R2 values
  results_cs$Formula[i] <- paste(deparse(formula), collapse = " ")
  results_cs$AIC[i] <- AIC(model)
  
  # Calculate R2 values
  r2_values <- r.squaredGLMM(model)
  results_cs$R2_m[i] <- r2_values[1]
  results_cs$R2_c[i] <- r2_values[3]
  
  # Extract p-values and identify significant variables
  pvals <- summary(model)$coefficients[, "Pr(>|z|)"]  # Extract p-values for fixed effects
  significant_vars <- names(pvals)[pvals < 0.05]  # Variables with p-value < 0.05
  
  # Store significant variables as a concatenated string
  results_cs$Significant_Vars[i] <- ifelse(length(significant_vars) > 0, paste(significant_vars, collapse = ", "), "None")
}
# Sort the results by AIC from smallest to largest
results_cs <- results_cs %>% arrange(AIC)

# Selected model for the expert 

write.csv(results_cs,'~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_models_complete.csv')


model_cs <- glmer(bio_pres ~ Site.type + NDVI_q90_t + flow_acc + SS + Site.type * NDVI_q90_t+ (1 | Watercontactsite),
                  data = cs_data_model, family = binomial,
                  control = control_params)
summary(model_cs)

full_model_cs = glmer(bio_pres ~ pp_oneweek + temp_modis + Site.type * NDVI_q90_t + elev + flow_acc + SS + (1 | Watercontactsite),
                          data = cs_data_model, family = binomial,
                          control = control_params)

# format results into data frame with global p-values

model_cs |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      Site.type ~ "Site type",
      NDVI_q90_t ~ "NDVI",
      flow_acc ~ "Flow accumulation"
    )
  ) |>
  bold_p(t = 0.050) |>
  bold_labels() |>
  italicize_levels()

model_cs |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    intercept = T,
    label = list(
      Site.type ~ "Site type",
      NDVI_q90_t ~ "NDVI",
      flow_acc ~ "Flow accumulation"
    )
  ) |>
  bold_p(t = 0.050) |>
  bold_labels() |>
  italicize_levels()

full_model_cs |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    intercept = T,
    label = list(
      Site.type ~ "Site type",
      NDVI_q90_t ~ "NDVI",
      flow_acc ~ "Flow accumulation"
    )
  ) |>
  bold_p(t = 0.050) |>
  bold_labels() |>
  italicize_levels()


# Fit models
model_with_interaction <- glmer(bio_pres ~ Site.type + NDVI_q90_t + flow_acc + Site.type * NDVI_q90_t+ (1 | Watercontactsite),
                                data = cs_data_model, family = binomial,
                                control = control_params)
model_without_interaction <- glmer(bio_pres ~ flow_acc + Site.type + NDVI_q90_t+ (1 | Watercontactsite),
                                   data = cs_data_model, family = binomial,
                                   control = control_params)

summary(model_with_interaction)
# Likelihood Ratio Test
anova(model_with_interaction, model_without_interaction)

# Baseline models 

model_expert_null <- glm(bio_pres ~ 1,
                      data = ex_data_model, family = binomial)


model_cs_null <- glm(bio_pres ~ 1,
                  data = cs_data_model, family = binomial)


model_expert_null |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    intercept = T,
  ) |>
  bold_p(t = 0.050) |>
  bold_labels() |>
  italicize_levels()

model_cs_null |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    intercept = T
  ) |>
  bold_p(t = 0.050) |>
  bold_labels() |>
  italicize_levels()

############# IMPERFECT DETECTION

## Imperfect detection

# Predictions in cs data 
cs_data_model$PDe <- predict(model_expert, newdata = cs_data_model, type = "response")

cs_data_model$Dcs = cs_data_model$PDe - cs_data_model$bio_pres

# this needs to be the original dataset without scaling (normally the detection var have been not scaled)

cs_data_reverted = cs_data_model

cs_data_reverted$ID = as.factor(cs_data_reverted$ID)
# Split data into positive Dcs and negative Dcs datasets
positive_Dcs_data <- cs_data_reverted %>%
  filter(Dcs > 0)

locations_pos =  positive_Dcs_data %>%
  group_by(Watercontactsite) %>%
  summarize(
    n =  n()
  )
positive_Dcs_data <- positive_Dcs_data %>%
  inner_join(locations_pos %>% filter(n >= 20), by = "Watercontactsite")

table(positive_Dcs_data$Watercontactsite)

negative_Dcs_data <- cs_data_reverted %>%
  filter(Dcs < 0)

locations_neg =  negative_Dcs_data %>%
  group_by(Watercontactsite) %>%
  summarize(
    n =  n()
  )

negative_Dcs_data <- negative_Dcs_data %>%
  inner_join(locations_neg %>% filter(n >= 20), by = "Watercontactsite")

sd(positive_Dcs_data$time_sin)
sd(positive_Dcs_data$samp_den)

sd(negative_Dcs_data$cumulative_sampling_freq)
sd(negative_Dcs_data$samp_den)


# Visualize the distribution
ggplot(positive_Dcs_data, aes(x = Dcs)) +
  geom_histogram(binwidth = 0.1, fill = "navyblue", alpha = 0.7) +
  labs(x = "Observed probability of false negatives (FNij)", y = "Frequency") + theme_minimal()

# Visualize the distribution
ggplot(negative_Dcs_data, aes(x = Dcs)) +
  geom_histogram(binwidth = 0.1, fill = "thistle", alpha = 0.7) +
  labs(x = "Observed probability of unexpected observations (UOij)", y = "Frequency") + theme_minimal()

# Calculate the median
median_value_pos <- median(positive_Dcs_data$Dcs, na.rm = TRUE)

# Create the plot
ggplot(positive_Dcs_data, aes(x = Dcs)) +
  geom_histogram(binwidth = 0.1, fill = "#5f63ab") +
  geom_vline(xintercept = median_value_pos, linetype = "dashed", color = "darkred", size = 1) +  # Vertical line at median
  annotate("text", x = median_value_pos + 0.05, y = max(table(cut(positive_Dcs_data$Dcs, breaks = seq(0, 1, 0.1)))), 
           label = paste("Median = ", round(median_value_pos, 3)), color = "darkred", hjust = 0, size = 4.5) +  # Annotation with median value
  labs(x = "Observed probability of false negatives (FNij)", y = "Frequency") + 
  theme_minimal()

negative_Dcs_data$Dcs = negative_Dcs_data$Dcs*(-1)

# Calculate the median for negative Dcs data
median_value_neg <- median(negative_Dcs_data$Dcs, na.rm = TRUE)

# Create the plot for negative Dcs data
ggplot(negative_Dcs_data, aes(x = Dcs)) +
  geom_histogram(binwidth = 0.1, fill = "#e3d4e4") +
  geom_vline(xintercept = median_value_neg, linetype = "dashed", color = "darkred", size = 1) +  # Vertical line at median
  annotate("text", x = median_value_neg + 0.37, y = 490, 
           label = paste("Median = ", round(median_value_neg, 3)), color = "darkred", hjust = 0, size = 4.5) +  # Annotation with median value
  labs(x = "Observed probability of unexpected observations (UOij)", y = "Frequency") + 
  scale_y_continuous(breaks = seq(0, 600, by = 100)) + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.25), expand = c(0,0.07)) +   # Set y-axis breaks every 100
  theme_minimal()

## Now, scale per subsets

sd(positive_Dcs_data$time_sin)
sd(positive_Dcs_data$samp_den)

sd(negative_Dcs_data$cumulative_sampling_freq)
sd(negative_Dcs_data$samp_den)


positive_Dcs_data_sc <- positive_Dcs_data %>%
  mutate(across(c(time_sin, time_cos, cons, cumulative_sampling_freq, time_dur, samp_den), scale))

negative_Dcs_data_sc <- negative_Dcs_data %>%
  mutate(across(c(time_sin, time_cos, cons, cumulative_sampling_freq, time_dur, samp_den), scale))

# Load necessary libraries
library(glmmTMB)
library(MuMIn)  # For R2 calculation

# Extract all fixed effect terms from the full model (without the interaction term)
fixed_effects <- c("time_sin",'time_cos', "cons", "cumulative_sampling_freq", "time_dur",'ID' ,'samp_den')

#fixed_effects <- c("time_sin",'time_cos', "cons", "cumulative_sampling_freq", "time_dur", 'Site.type')

# Function to generate all subsets of a set (without the empty set)
generate_combinations <- function(effects) {
  unlist(lapply(1:length(effects), function(k) combn(effects, k, simplify = FALSE)), recursive = FALSE)
}

# Generate all combinations of the fixed effects
combinations <- generate_combinations(fixed_effects)

# Load necessary libraries
library(glmmTMB)
library(performance) # For R² calculation
library(MuMIn)       # For pseudo-R² (optional, as `performance` can also calculate R²)

# Set control parameters (optional, adjust if needed)
control_params <- glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max = 1e5))

# Set the reference level for ID
# ID 10 is Imelda and she was the cs with more sampling instances (considering that she had to sample 3 places while ID 23, 4 sites)
positive_Dcs_data_sc$ID <- factor(positive_Dcs_data_sc$ID, 
                                  levels = c("10", 
                                             setdiff(levels(positive_Dcs_data_sc$ID), "10")))
# Initialize storage for AIC, R2 values, and formulas
results_cs_ex_1 <- data.frame(Formula = character(length(combinations)),
                              AIC = numeric(length(combinations)),
                              R2_m = numeric(length(combinations)),
                              R2_c = numeric(length(combinations)),
                              stringsAsFactors = FALSE)

# Loop over all combinations and fit models
for (i in seq_along(combinations)) {
  # Create the fixed effects formula part
  fixed_formula <- paste(combinations[[i]], collapse = " + ")
  
  # Create the full formula for the model
  formula <- as.formula(paste("Dcs ~", fixed_formula, "+ (1|Watercontactsite)"))
  
  # Fit the model using Beta distribution with logit link
  model <- glmmTMB(formula, 
                   data = positive_Dcs_data_sc, 
                   family = beta_family(link = "logit"), 
                   control = control_params)
  
  # Store the formula and AIC
  results_cs_ex_1$Formula[i] <- paste(deparse(formula), collapse = " ")
  results_cs_ex_1$AIC[i] <- AIC(model)
  
  
  # Calculate pseudo-R² values using the `performance` package
  r2_values <- r2(model)
  
  # Store marginal and conditional R² values
  results_cs_ex_1$R2_m[i] <- r2_values$R2_marginal
  results_cs_ex_1$R2_c[i] <- r2_values$R2_conditional
  
  pvals <- summary(model)$coefficients$cond[, "Pr(>|z|)"]  # Extract p-values for fixed effects
  significant_vars <- names(pvals)[pvals < 0.05]  # Variables with p-value < 0.05
  
  # Store significant variables as a concatenated string
  results_cs_ex_1$Significant_Vars[i] <- ifelse(length(significant_vars) > 0, paste(significant_vars, collapse = ", "), "None")
  
}

# Print the results sorted by AIC
results_cs_ex_1 <- results_cs_ex_1 %>% arrange(AIC)

write.csv(results_cs_ex_1,'~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_false_neg_complete.csv')

model_det_pos = glmmTMB(Dcs ~ time_sin + time_cos + ID + cumulative_sampling_freq + samp_den + (1 | Watercontactsite), 
                          data = positive_Dcs_data_sc, 
                          family = beta_family(link = "logit"), 
                          control = control_params)

response_fn <- predict(model_det_pos, newdata = positive_Dcs_data_sc, type = "response")

plot(positive_Dcs_data$samp_den, response_fn)


library(gtsummary)

model_det_pos |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      time_sin ~ "Time sin",
      samp_den ~ 'Sampling density'
    )
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()



# ID 10 is Imelda and she was the cs with more sampling instances (considering that she had to sample 3 places while ID 23, 4 sites)
negative_Dcs_data_sc$ID <- factor(negative_Dcs_data_sc$ID, 
                                  levels = c("10", 
                                             setdiff(levels(negative_Dcs_data_sc$ID), "10")))

# Initialize storage for AIC, R2 values, and formulas
results_cs_ex_2 <- data.frame(Formula = character(length(combinations)),
                              AIC = numeric(length(combinations)),
                              R2_m = numeric(length(combinations)),
                              R2_c = numeric(length(combinations)),
                              stringsAsFactors = FALSE)

# Loop over all combinations and fit models
for (i in seq_along(combinations)) {
  # Create the fixed effects formula part
  fixed_formula <- paste(combinations[[i]], collapse = " + ")
  
  # Create the full formula for the model
  formula <- as.formula(paste("Dcs ~", fixed_formula, "+ (1|Watercontactsite)"))
  
  # Fit the model using Beta distribution with logit link
  model <- glmmTMB(formula, 
                   data = negative_Dcs_data_sc, 
                   family = beta_family(link = "logit"), 
                   control = control_params)
  
  # Store the formula and AIC
  results_cs_ex_2$Formula[i] <- paste(deparse(formula), collapse = " ")
  results_cs_ex_2$AIC[i] <- AIC(model)
  
  
  # Calculate pseudo-R² values using the `performance` package
  r2_values <- r2(model)
  
  # Store marginal and conditional R² values
  results_cs_ex_2$R2_m[i] <- r2_values$R2_marginal
  results_cs_ex_2$R2_c[i] <- r2_values$R2_conditional
  
  pvals <- summary(model)$coefficients$cond[, "Pr(>|z|)"]  # Extract p-values for fixed effects
  significant_vars <- names(pvals)[pvals < 0.05]  # Variables with p-value < 0.05
  
  # Store significant variables as a concatenated string
  results_cs_ex_2$Significant_Vars[i] <- ifelse(length(significant_vars) > 0, paste(significant_vars, collapse = ", "), "None")
  
}

# Print the results sorted by AIC
results_cs_ex_2 <- results_cs_ex_2 %>% arrange(AIC)

write.csv(results_cs_ex_2,'~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_unex_obs_complete.csv')

model_det_neg = glmmTMB(Dcs ~ time_sin + cumulative_sampling_freq + ID + samp_den + (1 | Watercontactsite), 
                        data = negative_Dcs_data_sc, 
                        family = beta_family(link = "logit"), 
                        control = control_params)

#response_uo <- predict(model_det_neg, newdata = negative_Dcs_data_sc, type = "response")

#plot(positive_Dcs_data$samp_den, response_fn)
#plot(negative_Dcs_data$samp_den, response_uo)

#library(ggplot2)
#library(dplyr)

response_fn <- predict(model_det_pos, newdata = positive_Dcs_data_sc, type = "response")
response_uo <- predict(model_det_neg, newdata = negative_Dcs_data_sc, type = "response")

# Create a combined dataframe with predictions
positive_Dcs_data1 <- positive_Dcs_data %>% mutate(type = "False Negatives", response = response_fn)
 negative_Dcs_data1 <- negative_Dcs_data %>% mutate(type = "Unexpected observations", response = response_uo)

 combined_data <- bind_rows(positive_Dcs_data1, negative_Dcs_data1)

# Plot both predictions as line trends
ggplot(combined_data, aes(x = cumulative_sampling_freq, y = response, color = type)) +
  geom_point(alpha = 0.3) +  # Light points to show data
  theme_minimal() +
  labs(
       x = "Cumulative sampling frequency (CSF)",
       y = "Predicted Response",
       color = "Group") +
  scale_color_manual(values = c("False Negatives" = "navyblue", "Unexpected observations" = "thistle"))  # Custom colors

# model_det_neg |>
#  tbl_regression(
#    exponentiate = TRUE,
#    pvalue_fun = label_style_pvalue(digits = 2),
#    label = list(
#      samp_den ~ "Sampling density",
#      cumulative_sampling_freq ~ 'Cumulative samp. freq.')
#  ) |>
#  bold_p(t = 0.05) |>
#  bold_labels() |>
#  italicize_levels()

#saveRDS(model_expert, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/PD_expert.RDS")
#saveRDS(model_cs, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/PD_citizens.RDS")
#saveRDS(model_det_pos, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/ID_false_neg.RDS")
#saveRDS(model_det_neg, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/ID_unex_obs.RDS")

#HEREEEE

model_expert = readRDS("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/PD_expert.RDS")
model_cs = readRDS("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/PD_citizens.RDS")

# Graph indicating frequency of predictors and significance

# Total number of models for Expert and CS groups
total_models_expert <- 15
total_models_cs <- 7


# Create dataframes for expert and CS variable frequencies
variable_frequency_ex <- data.frame(
  Variable = c('Site type', 'Elevation', 'Precipitation', 'Temperature', 'NDVI', 'Flow accumulation','Slope steepness factor', 'Site type * NDVI'),
  Frequency = c(15, 10, 7, 4, 3, 2, 2, 3),
  Model = "Expert"
)

variable_importance_ex <- data.frame(
  Variable = c('Site type', 'Elevation', 'Precipitation', 'Temperature', 'NDVI', 'Flow accumulation','Slope steepness factor', 'Site type * NDVI'),
  Frequency = c(4, 0, 0, 0, 3, 0, 0, 0),
  Model = "Expert_i"
)

variable_frequency_cs <- data.frame(
  Variable = c('Site type', 'Elevation', 'Precipitation', 'Temperature', 'NDVI', 'Flow accumulation', 'Slope steepness factor', 'Site type * NDVI'),
  Frequency = c(7, 1, 1, 1, 7, 2, 2, 7),
  Model = "CS"
)

variable_importance_cs <- data.frame(
  Variable = c('Site type', 'Elevation', 'Precipitation', 'Temperature', 'NDVI', 'Flow accumulation','Slope steepness factor', 'Site type * NDVI'),
  Frequency = c(7, 0, 0, 0, 7, 0, 0, 7),
  Model = "CS_i"
)

# Combine the two dataframes
#combined_data <- rbind(variable_frequency_ex, variable_frequency_cs,variable_importance_ex, variable_importance_cs)
# Combined data preparation remains the same
combined_data <- rbind(variable_frequency_ex, variable_frequency_cs, variable_importance_ex, variable_importance_cs) %>%
  mutate(Type = ifelse(grepl("_i", Model), "Importance", "Frequency"),
         Model = sub("_i", "", Model))  # Clean up model names

# Calculate percentages for each group
combined_data <- combined_data %>%
  group_by(Model) %>%
  mutate(Total_Models = ifelse(Model == "Expert", 15, 7),
         Percentage = (Frequency / Total_Models) * 100) %>%
  ungroup()

library(ggpattern)

p <- ggplot() +
  # Larger bars for frequency
  geom_bar(
    data = combined_data %>% filter(Type == "Frequency"),
    aes(
      y = Percentage, 
      x = reorder(Variable, Percentage), 
      fill = Model
    ),
    stat = "identity",
    position = position_dodge(width = 0.8),  # Side-by-side for models
    width = 0.8,  # Full bar width for outer bars
    alpha = 1,
    color = 'black', linewidth= 0.5# Transparency for clarity
  ) +
  # Smaller bars for importance with border and hatching
  geom_bar_pattern(
    data = combined_data %>% filter(Type == "Importance"),
    aes(
      y = Percentage, 
      x = reorder(Variable, Percentage), 
      fill = Model,
      pattern_fill = Model),
    stat = "identity",
    position = position_dodge(width = 0.8),  # Align with larger bars
    width = 0.8,  # Narrower width for smaller bars
    alpha = 1,  # Opaque bars
    pattern = "stripe",  pattern_alpha = 1 , pattern_density = 0.7, pattern_angle = 45, 
    pattern_frequency = 2, pattern_key_scale_factor = 1, pattern_size = 0.6, # Pattern type
    color = "black" , linewidth = 0.6, pattern_colour = 'white'# Border color
  ) +
  # Custom colors and patterns to match styling
  scale_fill_manual(values = c("Expert" = "#104862", "CS" = "#83CBEB")) +
  scale_pattern_fill_manual(values = c("Expert" = "#104862", "CS" = "#83CBEB")) +
  # Flip coordinates to match horizontal layout
  coord_flip() +
  # Add labels and styling
  labs(
    subtitle = "Expert: 15 models, Citizen scientists: 7 models",
    x = "Variable",
    y = "Percentage (%)",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "right"
  )

print(p)

# Best models info and plotting 

# Plot odd ratios 

library(broom.mixed)

# Tidy up the model and extract coefficients
tidy_model_ex <- broom.mixed::tidy(model_expert, conf.int = TRUE)
# Tidy up the model and extract coefficients
tidy_model_cs <- broom.mixed::tidy(model_cs, conf.int = TRUE)


# Convert log-odds to odds ratios by exponentiating the estimates
tidy_model_ex <- tidy_model_ex %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(conf.low),
         upper_ci = exp(conf.high))


# Convert log-odds to odds ratios by exponentiating the estimates
tidy_model_cs <- tidy_model_cs %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(conf.low),
         upper_ci = exp(conf.high))


# Filter out the intercept and random effects
tidy_model_ex <- tidy_model_ex %>%
  filter(effect == "fixed", term != "(Intercept)")

# Filter out the intercept and random effects
tidy_model_cs <- tidy_model_cs %>%
  filter(effect == "fixed", term != "(Intercept)")
# Combine and align terms from both models
all_terms <- unique(c(tidy_model_ex$term, tidy_model_cs$term))

# Ensure both models have the same terms (fill in missing terms with NA)
tidy_model_ex_aligned <- tidy_model_ex %>%
  complete(term = all_terms, fill = list(odds_ratio = NA, lower_ci = NA, upper_ci = NA))

tidy_model_cs_aligned <- tidy_model_cs %>%
  complete(term = all_terms, fill = list(odds_ratio = NA, lower_ci = NA, upper_ci = NA))

# Define common labels for both plots
variable_labels <- c(
  "elev" = "Elevation",
  "Site.typeWetland:NDVI_q90_t" = "Site type (Wetland) * NDVI",
  "Site.typeWetland" = "Site type (Wetland)",
  "Site.typeStream:NDVI_q90_t" = "Site type (Stream) * NDVI",
  "Site.typeStream" = "Site type (Stream)",
  "Site.typeSpring:NDVI_q90_t" = "Site type (Spring) * NDVI",
  "Site.typeSpring" = "Site type (Spring)",
  "NDVI_q90_t" = "NDVI",
  "pp_oneweek" = "Precipitation",
  "SS" = "Slope steepness",
  "flow_acc" = "Flow accumulation"
)

# Plot for the expert model
plot_expert <- ggplot(tidy_model_ex_aligned, aes(x = term, y = odds_ratio)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), color = "#104862", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +
  scale_y_log10(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  scale_x_discrete(labels = variable_labels) +
  coord_flip() +
  labs(x = "Predictor variables", y = "Odds Ratio (95% CI)") +
  theme_minimal()

# Plot for the citizen scientist model
plot_cs <- ggplot(tidy_model_cs_aligned, aes(x = term, y = odds_ratio)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), color = "#83CBEB", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +
  scale_y_log10(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  scale_x_discrete(labels = variable_labels) +
  coord_flip() +
  labs(x = "Predictor variables", y = "Odds Ratio (95% CI)") +
  theme_minimal()

# Display the plots
plot_expert
plot_cs

# Add a column to each dataset to indicate the model type
tidy_model_ex_aligned <- tidy_model_ex_aligned %>%
  mutate(model = "Expert")

tidy_model_cs_aligned <- tidy_model_cs_aligned %>%
  mutate(model = "Citizen Scientists")

# Combine both datasets
combined_data <- bind_rows(tidy_model_ex_aligned, tidy_model_cs_aligned)

# Define common labels for both plots
variable_labels <- c(
  "elev" = "Elevation",
  "Site.typeWetland:NDVI_q90_t" = "Site type (Wetland) * NDVI",
  "Site.typeWetland" = "Site type (Wetland)",
  "Site.typeStream:NDVI_q90_t" = "Site type (Stream) * NDVI",
  "Site.typeStream" = "Site type (Stream)",
  "Site.typeSpring:NDVI_q90_t" = "Site type (Spring) * NDVI",
  "Site.typeSpring" = "Site type (Spring)",
  "NDVI_q90_t" = "NDVI",
  "pp_oneweek" = "Precipitation",
  "SS" = "Slope steepness",
  "flow_acc" = "Flow accumulation"
)

# Create the combined forest plot
ggplot(combined_data, aes(x = term, y = odds_ratio, color = model)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci, shape = model), size = 1, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +
  scale_y_log10(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  scale_x_discrete(labels = variable_labels) +
  coord_flip() +
  scale_color_manual(values = c("Expert" = "#104862", "Citizen Scientists" = "#83CBEB")) +
  labs(x = "Predictor variables", y = "Odds Ratio (95% CI)", color = "Best model", shape = "Best model") +
  theme_minimal() +
  theme(legend.position = "top")  # Move the legend to the top for better clarity


## Add model citizen scientists (with reduced period)

rm(list= setdiff(ls(), c('model_det_neg', 'model_det_pos', 'tidy_model_cs', 'tidy_model_ex')))

library(broom.mixed)

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 

#	May 2022 cs model vs expert model (sensitivity) a;sp for imp/ det 
# Convert categorical variables to factors
cs_data$Site.type <- as.factor(cs_data$Site.type)

# Define the date range
start_date <- as.Date("2020-06-24")
end_date <- as.Date("2022-04-27")

# Filter cs_data and ex_data based on date range
cs_data_filtered <- cs_data %>%
  filter(date >= start_date & date <= end_date)

# For cs_data, keep only Watercontactsites with at least 20 observations
cs_data_filtered <- cs_data_filtered %>%
  group_by(Watercontactsite) %>%
  filter(n() >= 20) %>%
  ungroup()

cs_data_model <- cs_data_filtered %>%
  mutate(across(c(pp_oneweek,temp_modis,NDVI_q90_t, elev, flow_acc, SS), scale))

# GLMMs

# Convert categorical variables to factors
cs_data_model$Watercontactsite <- as.factor(cs_data_model$Watercontactsite)

rm(list = setdiff(ls(), c('model_det_neg', 'model_det_pos', 'tidy_model_cs', 'tidy_model_ex','cs_data_model')))

library(MuMIn)

# Define the control parameters

# Same as selected for the citizens in the extended dataset

control_params <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 5e5))

model_cs_short <- glmer(bio_pres ~ SS + Site.type + NDVI_q90_t + flow_acc + Site.type * NDVI_q90_t+ (1 | Watercontactsite),
                  data = cs_data_model, family = binomial,
                  control = control_params)


model_cs_short |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    intercept = T,
    label = list(
      Site.type ~ "Site type",
      NDVI_q90_t ~ "NDVI",
      flow_acc ~ "Flow accumulation"
    )
  ) |>
  bold_p(t = 0.050) |>
  bold_labels() |>
  italicize_levels()

# Tidy up the model and extract coefficients
tidy_model_short_cs <- broom.mixed::tidy(model_cs_short, conf.int = TRUE)


# Convert log-odds to odds ratios by exponentiating the estimates
tidy_model_short_cs <- tidy_model_short_cs %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(conf.low),
         upper_ci = exp(conf.high))

# Filter out the intercept and random effects
tidy_model_short_cs <- tidy_model_short_cs %>%
  filter(effect == "fixed", term != "(Intercept)")
# Combine and align terms from both models
all_terms <- unique(c(tidy_model_ex$term, tidy_model_cs$term, tidy_model_short_cs$term))

# Ensure both models have the same terms (fill in missing terms with NA)
tidy_model_ex_aligned <- tidy_model_ex %>%
  complete(term = all_terms, fill = list(odds_ratio = NA, lower_ci = NA, upper_ci = NA))

tidy_model_cs_aligned <- tidy_model_cs %>%
  complete(term = all_terms, fill = list(odds_ratio = NA, lower_ci = NA, upper_ci = NA))

tidy_model_short_cs_aligned <- tidy_model_short_cs %>%
  complete(term = all_terms, fill = list(odds_ratio = NA, lower_ci = NA, upper_ci = NA))

# Define common labels for both plots

#Add a column to each dataset to indicate the model type
tidy_model_ex_aligned <- tidy_model_ex_aligned %>%
  mutate(model = "Expert")

tidy_model_cs_aligned <- tidy_model_cs_aligned %>%
  mutate(model = "Citizen Scientists Extended")

tidy_model_short_cs_aligned <- tidy_model_short_cs_aligned %>%
  mutate(model = "Citizen Scientists Reduced")
# Combine both datasets
combined_data <- bind_rows(tidy_model_ex_aligned, tidy_model_cs_aligned,tidy_model_short_cs_aligned )

# Define common labels for both plots
variable_labels <- c(
  "elev" = "Elevation",
  "Site.typeWetland:NDVI_q90_t" = "Site type (Wetland) * NDVI",
  "Site.typeWetland" = "Site type (Wetland)",
  "Site.typeStream:NDVI_q90_t" = "Site type (Stream) * NDVI",
  "Site.typeStream" = "Site type (Stream)",
  "Site.typeSpring:NDVI_q90_t" = "Site type (Spring) * NDVI",
  "Site.typeSpring" = "Site type (Spring)",
  "NDVI_q90_t" = "NDVI",
  "pp_oneweek" = "Precipitation",
  "SS" = "Slope steepness",
  "flow_acc" = "Flow accumulation"
)

# Create the combined forest plot
ggplot(combined_data, aes(x = term, y = odds_ratio, color = model)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci, shape = model), size = 1, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +
  scale_y_log10(breaks = c(0.1, 1, 10, 100), labels = c("0.1", "1", "10", "100")) +
  scale_x_discrete(labels = variable_labels) +
  coord_flip() +
  scale_color_manual(values = c("Expert" = "#104862", "Citizen Scientists Extended" = "#83CBEB", 'Citizen Scientists Reduced' = 'darkred')) +
  labs(x = "Predictor variables", y = "Odds Ratio (95% CI)", color = "Best model", shape = "Best model") +
  theme_minimal() +
  theme(legend.position = "top")  # Move the legend to the top for better clarity


model_expert = readRDS("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/PD_expert.RDS")

#Predictions in cs data 
cs_data_model$PDe <- predict(model_expert, newdata = cs_data_model, type = "response")

cs_data_model$Dcs = cs_data_model$PDe - cs_data_model$bio_pres

cs_data_reverted = cs_data_model

cs_data_reverted$ID = as.factor(cs_data_reverted$ID)
# Split data into positive Dcs and negative Dcs datasets
positive_Dcs_data <- cs_data_reverted %>%
  filter(Dcs > 0)

locations_pos =  positive_Dcs_data %>%
  group_by(Watercontactsite) %>%
  summarize(
    n =  n()
  )

positive_Dcs_data <- positive_Dcs_data %>%
  inner_join(locations_pos %>% filter(n >= 20), by = "Watercontactsite")

table(positive_Dcs_data$Watercontactsite)

negative_Dcs_data <- cs_data_reverted %>%
  filter(Dcs < 0)

locations_neg =  negative_Dcs_data %>%
  group_by(Watercontactsite) %>%
  summarize(
    n =  n()
  )

negative_Dcs_data <- negative_Dcs_data %>%
  inner_join(locations_neg %>% filter(n >= 20), by = "Watercontactsite")

# Visualize the distribution
ggplot(positive_Dcs_data, aes(x = Dcs)) +
  geom_histogram(binwidth = 0.1, fill = "navyblue", alpha = 0.7) +
  labs(x = "Observed probability of false negatives (FNij)", y = "Frequency") + theme_minimal()

# Visualize the distribution
ggplot(negative_Dcs_data, aes(x = Dcs)) +
  geom_histogram(binwidth = 0.1, fill = "thistle", alpha = 0.7) +
  labs(x = "Observed probability of unexpected observations (UOij)", y = "Frequency") + theme_minimal()


negative_Dcs_data$Dcs = negative_Dcs_data$Dcs*(-1)
## Now, scale per subsets

positive_Dcs_data_sc <- positive_Dcs_data %>%
  mutate(across(c(time_sin, time_cos, cons, cumulative_sampling_freq, time_dur, samp_den), scale))

negative_Dcs_data_sc <- negative_Dcs_data %>%
  mutate(across(c(time_sin, time_cos, cons, cumulative_sampling_freq, time_dur, samp_den), scale))

# Load necessary libraries
library(glmmTMB)
library(MuMIn)  # For R2 calculation

# Extract all fixed effect terms from the full model (without the interaction term)
#fixed_effects <- c("time_sin",'time_cos', "cons", "cumulative_sampling_freq", "time_dur",'ID' ,'samp_den')

#fixed_effects <- c("time_sin",'time_cos', "cons", "cumulative_sampling_freq", "time_dur", 'Site.type')

# Function to generate all subsets of a set (without the empty set)
#generate_combinations <- function(effects) {
#  unlist(lapply(1:length(effects), function(k) combn(effects, k, simplify = FALSE)), recursive = FALSE)
#}

# Generate all combinations of the fixed effects
#combinations <- generate_combinations(fixed_effects)

# Load necessary libraries
#library(glmmTMB)
#library(performance) # For R² calculation
#library(MuMIn)       # For pseudo-R² (optional, as `performance` can also calculate R²)

# Set control parameters (optional, adjust if needed)
control_params <- glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max = 1e5))

# Set the reference level for ID
# ID 10 is Imelda and she was the cs with more sampling instances (considering that she had to sample 3 places while ID 23, 4 sites)
positive_Dcs_data_sc$ID <- factor(positive_Dcs_data_sc$ID, 
                                  levels = c("10", 
                                             setdiff(levels(positive_Dcs_data_sc$ID), "10")))


# Initialize storage for AIC, R2 values, and formulas
#results_cs_ex_3 <- data.frame(Formula = character(length(combinations)),
#                              AIC = numeric(length(combinations)),
#                              R2_m = numeric(length(combinations)),
#                              R2_c = numeric(length(combinations)),
#                              stringsAsFactors = FALSE)

# Loop over all combinations and fit models
#for (i in seq_along(combinations)) {
  # Create the fixed effects formula part
#  fixed_formula <- paste(combinations[[i]], collapse = " + ")
  
  # Create the full formula for the model
#  formula <- as.formula(paste("Dcs ~", fixed_formula, "+ (1|Watercontactsite)"))
  
  # Fit the model using Beta distribution with logit link
#  model <- glmmTMB(formula, 
#                   data = positive_Dcs_data_sc, 
#                   family = beta_family(link = "logit"), 
#                   control = control_params)
  
  # Store the formula and AIC
#  results_cs_ex_3$Formula[i] <- paste(deparse(formula), collapse = " ")
#  results_cs_ex_3$AIC[i] <- AIC(model)
  
  
  # Calculate pseudo-R² values using the `performance` package
#  r2_values <- r2(model)
  
  # Store marginal and conditional R² values
#  results_cs_ex_3$R2_m[i] <- r2_values$R2_marginal
#  results_cs_ex_3$R2_c[i] <- r2_values$R2_conditional
  
#  pvals <- summary(model)$coefficients$cond[, "Pr(>|z|)"]  # Extract p-values for fixed effects
#  significant_vars <- names(pvals)[pvals < 0.05]  # Variables with p-value < 0.05
  
  # Store significant variables as a concatenated string
#  results_cs_ex_3$Significant_Vars[i] <- ifelse(length(significant_vars) > 0, paste(significant_vars, collapse = ", "), "None")
  
#}

# Print the results sorted by AIC
# results_cs_ex_3 <- results_cs_ex_3 %>% arrange(AIC)

#write.csv(results_cs_ex_3,'~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_short_false_neg_complete.csv')

#model_fn_short_best = glmmTMB(Dcs ~ time_sin + cumulative_sampling_freq + ID  + (1 | Watercontactsite), 
#                          data = positive_Dcs_data_sc, 
#                          family = beta_family(link = "logit"), 
#                          control = control_params)

model_fn_short = glmmTMB(Dcs ~ time_sin + time_cos + ID + samp_den + cumulative_sampling_freq + (1 | Watercontactsite), 
                              data = positive_Dcs_data_sc, 
                              family = beta_family(link = "logit"), 
                              control = control_params)

library(gtsummary)

model_fn_short |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      time_sin ~ "Time sin",
      cumulative_sampling_freq ~ 'Cumulative samp. freq.'    )
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

#model_fn_short_same |>
#  tbl_regression(
#    exponentiate = TRUE,
#    pvalue_fun = label_style_pvalue(digits = 2),
#    label = list(
#      time_sin ~ "Time sin",
#      time_cos ~ "Time cos",
#      samp_den ~ 'Sampling density'
#    )
#  ) |>
#  bold_p(t = 0.05) |>
#  bold_labels() |>
#  italicize_levels()

# SD for model interpretation
sd(positive_Dcs_data$cumulative_sampling_freq)


# ID 10 is Imelda and she was the cs with more sampling instances (considering that she had to sample 3 places while ID 23, 4 sites)
negative_Dcs_data_sc$ID <- factor(negative_Dcs_data_sc$ID, 
                                  levels = c("10", 
                                             setdiff(levels(negative_Dcs_data_sc$ID), "10")))


# Initialize storage for AIC, R2 values, and formulas
#results_cs_ex_4 <- data.frame(Formula = character(length(combinations)),
#                              AIC = numeric(length(combinations)),
#                              R2_m = numeric(length(combinations)),
#                              R2_c = numeric(length(combinations)),
#                              stringsAsFactors = FALSE)

# Loop over all combinations and fit models
#for (i in seq_along(combinations)) {
  # Create the fixed effects formula part
#  fixed_formula <- paste(combinations[[i]], collapse = " + ")
  
  # Create the full formula for the model
#  formula <- as.formula(paste("Dcs ~", fixed_formula, "+ (1|Watercontactsite)"))
  
  # Fit the model using Beta distribution with logit link
#  model <- glmmTMB(formula, 
#                   data = negative_Dcs_data_sc, 
#                   family = beta_family(link = "logit"), 
#                   control = control_params)
  
  # Store the formula and AIC
#  results_cs_ex_4$Formula[i] <- paste(deparse(formula), collapse = " ")
#  results_cs_ex_4$AIC[i] <- AIC(model)
  
  
  # Calculate pseudo-R² values using the `performance` package
#  r2_values <- r2(model)
  
  # Store marginal and conditional R² values
#  results_cs_ex_4$R2_c[i] <- r2_values$R2_conditional
  
#  pvals <- summary(model)$coefficients$cond[, "Pr(>|z|)"]  # Extract p-values for fixed effects
#  significant_vars <- names(pvals)[pvals < 0.05]  # Variables with p-value < 0.05
  
  # Store significant variables as a concatenated string
#  results_cs_ex_4$Significant_Vars[i] <- ifelse(length(significant_vars) > 0, paste(significant_vars, collapse = ", "), "None")
  
#}

# Print the results sorted by AIC
#results_cs_ex_4 <- results_cs_ex_4 %>% arrange(AIC)

#write.csv(results_cs_ex_4,'~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_short_unex_obs_complete.csv')


model_uo_short = glmmTMB(Dcs ~ time_sin + cumulative_sampling_freq + ID  + samp_den + (1 | Watercontactsite), 
                        data = negative_Dcs_data_sc, 
                        family = beta_family(link = "logit"), 
                        control = control_params)


model_uo_short |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      samp_den ~ "Sampling density",
      cumulative_sampling_freq ~ 'Cumulative samp. freq.')
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

library(ggplot2)
library(dplyr)

response_fn <- predict(model_fn_short, newdata = positive_Dcs_data_sc, type = "response")
response_uo <- predict(model_uo_short, newdata = negative_Dcs_data_sc, type = "response")

# Create a combined dataframe with predictions
positive_Dcs_data1 <- positive_Dcs_data %>% mutate(type = "False Negatives", response = response_fn)
negative_Dcs_data1 <- negative_Dcs_data %>% mutate(type = "Unexpected observations", response = response_uo)

combined_data <- bind_rows(positive_Dcs_data1, negative_Dcs_data1)

summary(combined_data)

# Plot both predictions as line trends
ggplot(combined_data, aes(x = cumulative_sampling_freq, y = response, color = ID, shape = type)) +
  geom_point(alpha = 0.5, size = 3) +  # Light points to show data
  theme_minimal() +
  labs(
    x = "Cumulative samp. frequency (CSF) / reduced dataset",
    y = "Predicted Response",
    shape = "Group") 


