
# Models

library(ggplot2)
library(scales)
library(lme4)
library(tidyverse)

rm(list=ls())

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 
ex_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_complete.csv", sep = ",", header = TRUE) 

# Convert categorical variables to factors
ex_data$Site.type <- as.factor(ex_data$Site.type)
cs_data$Site.type <- as.factor(cs_data$Site.type)

# Scale data 
ex_data_model <- ex_data %>%
  mutate(across(c(pp_oneweek,temp_modis,NDVI_q90_t, elev, flow_acc, SS), scale))

cs_data_model <- cs_data %>%
  mutate(across(c(pp_oneweek,temp_modis,NDVI_q90_t, elev, flow_acc, SS), scale))

var_ex = ex_data_model %>% 
  dplyr::select(pp_oneweek,temp_modis,NDVI_q90_t, elev, flow_acc, SS, Site.type) 

var_cs = cs_data_model %>% 
  dplyr::select(pp_oneweek,temp_modis,NDVI_q90_t, elev, flow_acc, SS, Site.type) 

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

# Loop over all combinations and fit models
#for (i in seq_along(combinations)) {
  # Create the fixed effects formula part
#  fixed_formula <- paste(combinations[[i]], collapse = " + ")
  
  # If interaction term is present in the combination, include it
#  if (all(c("Site.type", "NDVI_q90_t") %in% combinations[[i]])) {
#    fixed_formula <- paste(fixed_formula, "+ Site.type * NDVI_q90_t")
#  }
  
  # Create the full formula for the model
#  formula <- as.formula(paste("bio_pres ~", fixed_formula, "+ (1 | Watercontactsite)"))
  
  # Fit the model
#  model <- glmer(formula, data = ex_data_model, family = binomial, control = control_params)
  
  # Store the formula, AIC, and R2 values
#  results$Formula[i] <- paste(deparse(formula), collapse = " ")
#  results$AIC[i] <- AIC(model)
  
  # Calculate R2 values
#  r2_values <- r.squaredGLMM(model)
#  results$R2_m[i] <- r2_values[1]
#  results$R2_c[i] <- r2_values[3]

#}

# Sort the results by AIC from smallest to largest
#results <- results %>% arrange(AIC)

#write.csv(results, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_models_ex_land.csv", row.names = FALSE)

# Selected model for the expert 

model_expert <- glmer(bio_pres ~ pp_oneweek + Site.type*NDVI_q90_t + 
                         elev + (1 | Watercontactsite),
                       data = ex_data_model, family = binomial,
                       control = control_params)
summary(model_expert)

# Fit models
model_with_interaction <- glmer(bio_pres ~ pp_oneweek + Site.type * NDVI_q90_t + elev + (1 | Watercontactsite), 
                                family = binomial, data = ex_data_model)
model_without_interaction <- glmer(bio_pres ~ pp_oneweek + Site.type + NDVI_q90_t + elev + (1 | Watercontactsite), 
                                   family = binomial, data = ex_data_model)

# Likelihood Ratio Test
anova(model_with_interaction, model_without_interaction)





library(gtsummary)

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
  bold_p(t = 0.10) |>
  bold_labels() |>
  italicize_levels()

library(broom.mixed)

# Tidy up the model and extract coefficients
tidy_model_ex <- broom.mixed::tidy(model_expert, conf.int = TRUE)

# Convert log-odds to odds ratios by exponentiating the estimates
tidy_model_ex <- tidy_model_ex %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(conf.low),
         upper_ci = exp(conf.high))

# Filter out the intercept and random effects
tidy_model_ex <- tidy_model_ex %>%
  filter(effect == "fixed", term != "(Intercept)")


# Create the forest plot
ggplot(tidy_model_ex, aes(x = term, y = odds_ratio)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), color = "#104862", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +  # Reference line at OR = 1
  scale_y_log10(    breaks = c(0.1, 1, 10, 100),  # Specify custom breaks for x-axis
                    labels = c("0.1", "1", "10", "100")  # Custom labels to avoid scientific notation
  ) +  # Logarithmic scale for x-axis
  # Rename predictor variables on the y-axis
  scale_x_discrete(
    labels = c(
               "elev" = "Elevation",
      "Site.typeWetland:NDVI_q90_t" = "Site type (Wetland) * NDVI",
      "Site.typeWetland" = "Site type (Wetland)",
      "Site.typeStream:NDVI_q90_t" = " Site type (Stream) * NDVI",
      "Site.typeStream" = "Site type (Stream)",
      "Site.typeSpring:NDVI_q90_t" = "Site type (Spring) * NDVI",
      "Site.typeSpring" = "Site type (Spring)",
      "NDVI_q90_t" = "NDVI",
      'pp_oneweek' = 'Precipitation'
    )) +
  coord_flip() +  # Flip to horizontal orientation
  labs(x = "Predictor variables",
       y = "Odds Ratio (95% CI)") +
  theme_minimal()  # Use a clean theme

# Extract residuals
deviance_residuals_ex <- residuals(model_expert, type = "deviance")
response_residuals_ex <- residuals(model_expert, type = "response")

fitted_values_ex <- fitted(model_expert)

# Plot residuals vs fitted values
plot(fitted_values_ex, deviance_residuals_ex, 
     xlab = "Fitted Values", ylab = "Deviance Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a reference line at 0

# Extract fitted values (on the link scale - log-odds)
fitted_log_odds <- predict(model_expert, type = "link")

# Scatter plot of flow accumulation vs. log-odds
p1 = ggplot(ex_data_model, aes(x = pp_oneweek, y = fitted_log_odds, col = Site.type)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess curve to check for nonlinearity
  labs( x = "Precipitation", y = "Log-Odds") + theme_minimal()

# Scatter plot of elevation vs. log-odds
p2 = ggplot(ex_data_model, aes(x = elev, y = fitted_log_odds, col= Site.type)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs( x = "Elevation", y = "Log-Odds") + theme_minimal()

# Scatter plot of NDVI_q90_t vs. log-odds
p3 = ggplot(ex_data_model, aes(x = NDVI_q90_t, y = fitted_log_odds, col= Site.type)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs( x = "NDVI (90th Percentile)", y = "Log-Odds") + theme_minimal()

combined_plot <- p1 / p2 / p3

combined_plot

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


# Create a dataframe for the variable frequencies
variable_frequency_ex <- data.frame(
  Variable = c('Site.type', 'Elev', 'pp', 'temp', 'NDVI', 'flow_acc','SS', 'Site_type*NDVI'),
  Frequency = c(13, 8, 6, 4, 3, 2,0, 3)
)

# Create a horizontal bar plot
ggplot(variable_frequency_ex, aes(x = Frequency, y = reorder(Variable, Frequency))) +
  geom_bar(stat = "identity", fill = "royalblue4") +
  labs(title = "Variable frequency in top 15 models by AIC (Expert)",
       x = "Frequency",
       y = "Variable") +
  theme_bw()

# Create a dataframe for the variable frequencies
variable_frequency_cs <- data.frame(
  Variable = c('Site.type', 'Elev', 'pp', 'temp', 'NDVI', 'flow_acc', 'SS','Site_type*NDVI'),
  Frequency = c(9, 1, 2, 2, 9, 2, 4, 10)
)

# Create a horizontal bar plot
ggplot(variable_frequency_cs, aes(x = Frequency, y = reorder(Variable, Frequency))) +
  geom_bar(stat = "identity", fill = "violetred4") +
  labs(title = "Variable frequency in top 15 models by AIC (CS)",
       x = "Frequency",
       y = "Variable") +
  theme_bw()


#write.csv(results_cs, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_models_cs_land.csv", row.names = FALSE)

# Total number of models for Expert and CS groups
total_models_expert <- 13
total_models_cs <- 10



# Create dataframes for expert and CS variable frequencies
variable_frequency_ex <- data.frame(
  Variable = c('Site type', 'Elevation', 'Precipitation', 'Temperature', 'NDVI', 'Flow accumulation','Slope steepness', 'Site type * NDVI'),
  Frequency = c(13, 8, 6, 4, 3, 2, 0, 3),
  Model = "Expert"
)

variable_importance_ex <- data.frame(
  Variable = c('Site type', 'Elevation', 'Precipitation', 'Temperature', 'NDVI', 'Flow accumulation','Slope steepness', 'Site type * NDVI'),
  Frequency = c(4, 0, 0, 0, 3, 0, 0, 0),
  Model = "Expert_i"
)

variable_frequency_cs <- data.frame(
  Variable = c('Site type', 'Elevation', 'Precipitation', 'Temperature', 'NDVI', 'Flow accumulation', 'Slope steepness', 'Site type * NDVI'),
  Frequency = c(10, 2, 2, 2, 10, 2, 5, 10),
  Model = "CS"
)

variable_importance_cs <- data.frame(
  Variable = c('Site type', 'Elevation', 'Precipitation', 'Temperature', 'NDVI', 'Flow accumulation','Slope steepness', 'Site type * NDVI'),
  Frequency = c(7, 0, 0, 0, 10, 0, 0, 10),
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
  mutate(Total_Models = ifelse(Model == "Expert", 13, 10),
         Percentage = (Frequency / Total_Models) * 100) %>%
  ungroup()


# Load required library for pattern support
if (!requireNamespace("ggpattern", quietly = TRUE)) {
  install.packages("ggpattern")
}
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
    alpha = 0.7,
    color = 'black'# Transparency for clarity
  ) +
  # Smaller bars for importance with border and hatching
  geom_bar_pattern(
    data = combined_data %>% filter(Type == "Importance"),
    aes(
      y = Percentage, 
      x = reorder(Variable, Percentage), 
      fill = Model,
      pattern_fill = Model
    ),
    stat = "identity",
    position = position_dodge(width = 0.8),  # Align with larger bars
    width = 0.4,  # Narrower width for smaller bars
    alpha = 1,  # Opaque bars
    pattern = "crosshatch",  # Pattern type
    color = "black"  # Border color
  ) +
  # Custom colors and patterns to match styling
  scale_fill_manual(values = c("Expert" = "#104862", "CS" = "#83CBEB")) +
  #scale_pattern_fill_manual(values = c("Expert" = "#FFFFFF", "CS" = "#FFFFFF")) +
  # Flip coordinates to match horizontal layout
  coord_flip() +
  # Add labels and styling
  labs(
    subtitle = "Expert: 13 models, CS: 10 models",
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

# Create the updated plot with nested bars
p <- ggplot() +
  # Larger bars for frequency
  geom_bar(
    data = combined_data %>% filter(Type == "Frequency"),
    aes(y = Percentage, x = reorder(Variable, Percentage), fill = Model),
    stat = "identity",
    position = position_dodge(width = 0.8),  # Side-by-side for models
    width = 0.8,  # Full bar width for outer bars
    alpha = 0.7  # Transparency for clarity
  ) +
  # Smaller bars for importance
  geom_bar(
    data = combined_data %>% filter(Type == "Importance"),
    aes(y = Percentage, x = reorder(Variable, Percentage), fill = Model),
    stat = "identity",
    position = position_dodge(width = 0.8),  # Align with larger bars
    width = 0.4,
    alpha = 1, # Narrower width for smaller bars
    color = 'white'
  ) +
  # Custom colors to match existing styling
  scale_fill_manual(values = c("Expert" = "#104862", "CS" = "#83CBEB")) +
  # Flip coordinates to match horizontal layout
  coord_flip() +
  # Add labels and styling
  labs(subtitle = "Expert: 13 models, CS: 10 models",
    x = "Variable",
    y = "Percentage (%)",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "right"
  )

p



# Create a grouped bar plot with percentages
#p = ggplot(combined_data, aes(x = Percentage, y = reorder(Variable, Percentage), fill = Model)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = c("Expert" = "#104862", "CS" = "#83CBEB")) +
#  labs(subtitle = "Expert: 13 models, CS: 10 models",
#       x = "Percentage (%)",
#       y = "Variable") +
#  theme_minimal() +
#  theme(legend.title = element_blank())  # Remove legend title for a cleaner look


# Adding star and triangle annotations
# Star (★) and Triangle (▲) for 'Site type' and 'NDVI'
p1 = p + annotate("text", x = 8.05, y = 103.5, label = "*", color = "#104862", size = 7) +
  annotate("text", x = 7.65, y = 103.5, label = "*", color = "#83CBEB", size = 7) +
  annotate("text", x = 6.05, y = 103.5, label = "*", color = "#104862", size = 7) +
  annotate("text", x = 6.65, y = 103.5, label = "*", color = "#83CBEB", size = 7) +
  
  # Only Star for 'Site type * NDVI'
  annotate("text", x = 5.65, y = 103.5, label = "*", color = "#83CBEB", size = 7)
 p1


# Significant variables per group 

# Create dataframes for expert and CS variable frequencies
#variable_sig_ex <- data.frame(
#  Variable = c('Site type - Stream', 'NDVI'),
#  Frequency = c(4,3),
#  Model = "Expert"
#)

#variable_sig_cs <- data.frame(
#  Variable = c('Site type - Stream', 'S.Stream:NDVI', 'S.Spring:NDVI', 'NDVI'),
#  Frequency = c(7,9,9,9),
#  Model = "CS"
#)

# Combine the two dataframes
#combined_data_sig <- rbind(variable_sig_ex, variable_sig_cs)

# Calculate percentages based on the total number of models for each group
#combined_data <- combined_data_sig %>%
#  mutate(Total_Models = ifelse(Model == "Expert", total_models_expert, total_models_cs),
#         Percentage = (Frequency / Total_Models) * 100)

# title = "Significant variables frequency in top models by AIC",
# Create a grouped bar plot with percentages
#ggplot(combined_data, aes(x = Percentage, y = reorder(Variable, Percentage), fill = Model)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  scale_fill_manual(values = c("Expert" = "#104862", "CS" = "#83CBEB")) +
#  labs(subtitle = "Expert: 13 models, CS: 9 models",
#       x = "Percentage (%)",
#       y = "Variable") +
#  theme_minimal() +
#  theme(legend.title = element_blank())  # Remove legend title for a cleaner look

# Model cs

model_cs <- glmer(bio_pres ~  flow_acc + Site.type*NDVI_q90_t + 
                           SS + (1 | Watercontactsite),
                        data = cs_data_model, family = binomial,
                        control = control_params)
summary(model_cs)


model_cs |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      Site.type ~ "Site type",
      NDVI_q90_t ~ "NDVI",
      flow_acc ~ 'Flow accumulation',
      SS ~ 'Slope steepness'
    )
  ) |>
  bold_p(t = 0.10) |>
  bold_labels() |>
  italicize_levels()


library(broom.mixed)

# Tidy up the model and extract coefficients
tidy_model_ex <- broom.mixed::tidy(model_expert, conf.int = TRUE)

# Convert log-odds to odds ratios by exponentiating the estimates
tidy_model_ex <- tidy_model_ex %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(conf.low),
         upper_ci = exp(conf.high))

# Filter out the intercept and random effects
tidy_model_ex <- tidy_model_ex %>%
  filter(effect == "fixed", term != "(Intercept)")


# Create the forest plot
ggplot(tidy_model_ex, aes(x = term, y = odds_ratio)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), color = "#104862", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +  # Reference line at OR = 1
  scale_y_log10(    breaks = c(0.1, 1, 10, 100),  # Specify custom breaks for x-axis
                    labels = c("0.1", "1", "10", "100")  # Custom labels to avoid scientific notation
  ) +  # Logarithmic scale for x-axis
  # Rename predictor variables on the y-axis
  scale_x_discrete(
    labels = c(
      "elev" = "Elevation",
      "Site.typeWetland:NDVI_q90_t" = "Site type (Wetland) * NDVI",
      "Site.typeWetland" = "Site type (Wetland)",
      "Site.typeStream:NDVI_q90_t" = " Site type (Stream) * NDVI",
      "Site.typeStream" = "Site type (Stream)",
      "Site.typeSpring:NDVI_q90_t" = "Site type (Spring) * NDVI",
      "Site.typeSpring" = "Site type (Spring)",
      "NDVI_q90_t" = "NDVI",
      'pp_oneweek' = 'Precipitation'
    )) +
  coord_flip() +  # Flip to horizontal orientation
  labs(x = "Predictor variables",
       y = "Odds Ratio (95% CI)") +
  theme_minimal()  # Use a clean theme

# Tidy up the model and extract coefficients
tidy_model_cs <- broom.mixed::tidy(model_cs, conf.int = TRUE)

# Convert log-odds to odds ratios by exponentiating the estimates
tidy_model_cs <- tidy_model_cs %>%
  mutate(odds_ratio = exp(estimate),
         lower_ci = exp(conf.low),
         upper_ci = exp(conf.high))

# Filter out the intercept and random effects
tidy_model_cs <- tidy_model_cs %>%
  filter(effect == "fixed", term != "(Intercept)")


# Create the forest plot
ggplot(tidy_model_cs, aes(x = term, y = odds_ratio)) +
  geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), color = "#83CBEB", size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "darkred") +  # Reference line at OR = 1
  scale_y_log10(    breaks = c(0.1, 1, 10, 100),  # Specify custom breaks for x-axis
                    labels = c("0.1", "1", "10", "100")  # Custom labels to avoid scientific notation
  ) +  # Logarithmic scale for x-axis
  # Rename predictor variables on the y-axis
  scale_x_discrete(
    labels = c(
      "elev" = "Elevation",
      "Site.typeWetland:NDVI_q90_t" = "Site type (Wetland) * NDVI",
      "Site.typeWetland" = "Site type (Wetland)",
      "Site.typeStream:NDVI_q90_t" = " Site type (Stream) * NDVI",
      "Site.typeStream" = "Site type (Stream)",
      "Site.typeSpring:NDVI_q90_t" = "Site type (Spring) * NDVI",
      "Site.typeSpring" = "Site type (Spring)",
      "NDVI_q90_t" = "NDVI",
      'pp_oneweek' = 'Precipitation',
      'SS' = 'Slope steepness',
      'flow_acc' = 'Flow accumulation'
    )) +
  coord_flip() +  # Flip to horizontal orientation
  labs(x = "Predictor variables",
       y = "Odds Ratio (95% CI)") +
  theme_minimal()  # Use a clean theme

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


# Extract residuals
deviance_residuals_cs <- residuals(model_cs, type = "deviance")
response_residuals_cs <- residuals(model_cs, type = "response")

fitted_values_cs <- fitted(model_cs)

# Plot residuals vs fitted values
plot(fitted_values_cs, deviance_residuals_cs, 
     xlab = "Fitted Values", ylab = "Deviance Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # Add a reference line at 0

# Extract fitted values (on the link scale - log-odds)
fitted_log_odds <- predict(model_cs, type = "link")

# Scatter plot of flow accumulation vs. log-odds
p1 = ggplot(cs_data_model, aes(x = SS, y = fitted_log_odds, col = Site.type)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +  # Loess curve to check for nonlinearity
  labs( x = "Slope steepness", y = "Log-Odds") + theme_minimal()

# Scatter plot of elevation vs. log-odds
p2 = ggplot(cs_data_model, aes(x = flow_acc, y = fitted_log_odds, col= Site.type)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs( x = "Flow accumulation", y = "Log-Odds") + theme_minimal()

# Scatter plot of NDVI_q90_t vs. log-odds
p3 = ggplot(cs_data_model, aes(x = NDVI_q90_t, y = fitted_log_odds, col= Site.type)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs( x = "NDVI (90th Percentile)", y = "Log-Odds") + theme_minimal()

combined_plot <- p1 / p2 / p3

combined_plot

library(ggeffects)

interaction_effect <- ggpredict(model_cs, terms = c("NDVI_q90_t", "Site.type"))

# Plot the interaction
plot(interaction_effect) +
  labs(x = "NDVI", y = "Predicted probability", title = "") +
  theme_minimal()

test_predictions(interaction_effect)

# After fitting the model, check model assumptions 

# Linearity on the Link Scale:

# Load necessary library
library(ggplot2)

# Obtain fitted values and residuals
fitted_values <- fitted(model_cs)
pearson_residuals <- residuals(model_cs, type = "pearson")

# Plot Pearson residuals vs fitted values
ggplot(data = data.frame(Fitted = fitted_values, Residuals = pearson_residuals),
       aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # Loess smoother to detect patterns
  labs(title = "Pearson Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Pearson Residuals") +
  theme_bw()


# Get fitted values on the logit scale (logits)
logit_fitted_values <- predict(model_cs, type = "link")

# Plot fitted values (on the logit scale) vs. continuous predictor 'flow_acc'
ggplot(cs_data_model, aes(x = flow_acc, y = logit_fitted_values, color = Site.type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +  # Loess smoother to visualize non-linearity
  labs(title = "Fitted Values (Logit Scale) vs. Flow Accumulation",
       x = "Flow Accumulation",
       y = "Fitted Values (Logit Scale)") +
  theme_bw()



# Install and load the arm package if not already installed
if (!require(arm)) install.packages("arm")
library(arm)

# Binned residual plot for the interaction term 'Site.type'
cs_data_model$logit_fitted_values <- predict(model_cs, type = "link")  # Add fitted values to data

# Create a binned residual plot for each level of 'Site.type'
ggplot(cs_data_model, aes(x = logit_fitted_values, y = residuals(model_cs, type = "pearson"))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  facet_wrap(~ Site.type) +  # Create separate plots for each 'Site.type'
  labs(title = "Binned Residual Plot by Site Type",
       x = "Fitted Values (Logit Scale)",
       y = "Pearson Residuals") +
  theme_bw()


predicted_probs <- predict(model_cs, type = "response")

# Create a data frame with actual outcomes and predicted probabilities
comparison_probs <- data.frame(
  Actual = cs_data_model$bio_pres,
  Predicted = predicted_probs
)

ggplot(comparison_probs, aes(x = Predicted, y = Actual)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Calibration Plot: Observed vs Predicted Probabilities",
       x = "Predicted Probability",
       y = "Observed Probability") +
  theme_bw()


# Step 1 was to select the models 

# Step 2 Model output comparison


# DETECTION MODEL *********************************

# Predictions in cs data 
cs_data_model$PDe <- predict(model_expert, newdata = cs_data_model, type = "response")

# Predictions in cs data 
cs_data_model$PDcs <- predict(model_cs, newdata = cs_data_model, type = "response")

correlation <- cor(cs_data_model$PDe, cs_data_model$PDcs,method = 's')

# Create the scatter plot and add the correlation coefficient
ggplot(cs_data_model, aes(x = PDe, y = PDcs)) +
  geom_point(color = "purple4", alpha = 0.6) +
  labs(x = "Predictions from Expert Model (PDex)",
       y = "Predictions from CS Model (PDcs)") +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation: ", round(correlation, 2)),
           hjust = 1.1, vjust = 1, size = 4, color = "darkred") +
  theme_minimal()

cs_data_model$ID = as.factor(cs_data_model$ID)

# Create the scatter plot and add the correlation coefficient
ggplot(cs_data_model, aes(x = PDe, y = PDcs, color = ID)) +
  geom_point(alpha = 0.6) +
  labs(
       x = "Predictions from Expert Model (PDex)",
       y = "Predictions from CS Model (PDcs)") +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation: ", round(correlation, 2)),
           hjust = 1.1, vjust = 1, size = 4, color = "darkred") +
  theme_minimal()  

# Create the scatter plot and add the correlation coefficient
ggplot(cs_data_model, aes(x = PDe, y = PDcs, color = Site.type)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Predictions from Expert Model (PDex)",
    y = "Predictions from CS Model (PDcs)") +
  annotate("text", x = Inf, y = Inf, label = paste("Correlation: ", round(correlation, 2)),
           hjust = 1.1, vjust = 1, size = 4, color = "darkred") +
  theme_minimal() 

# Matrix comparison 

#cs_data_model$PDe_bin <- with(cs_data_model, ifelse(cs_data_model$PDe >= 0.5, 1, 0))
#cs_data_model$PDcs_bin <- with(cs_data_model, ifelse(cs_data_model$PDcs >= 0.5, 1, 0))

# Confusion matrix graph 

# Create the confusion matrix
#confusion_matrix_cs_ex <- table(Predicted = cs_data_model$PDcs_bin, Actual = cs_data_model$PDe_bin)

# Convert the confusion matrix to a data frame
#confusion_cs_ex <- as.data.frame(as.table(confusion_matrix_cs_ex))

# Rename the columns for clarity
#colnames(confusion_cs_ex) <- c("Predicted", "Actual", "Freq")

# Plot the heatmap
#ggplot(confusion_cs_ex, aes(x = Actual, y = Predicted, fill = Freq)) +
#  geom_tile(color = "white") +
#  scale_fill_gradient(low = "wheat1", high = "thistle") +
#  geom_text(aes(label = Freq), vjust = 1) +
#  labs(x = "Actual = Expert model",
#       y = "Predicted = CS model",
#       fill = "Frequency") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Extract true positives, false positives, true negatives, and false negatives
#tp <- confusion_matrix_cs_ex[2, 2]
#fp <- confusion_matrix_cs_ex[2, 1]
#tn <- confusion_matrix_cs_ex[1, 1]
#fn <- confusion_matrix_cs_ex[1, 2]

# Calculate precision
#precision <- tp / (tp + fp)

# Calculate recall (also known as sensitivity)
#recall <- tp / (tp + fn)

# Calculate F1-score
#f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
#precision
#recall
#f1_score

# Dcs (Citizen science detection response variable)

# Checking normality of residuals after fitting model 
cs_data_model$Dcs = cs_data_model$PDe - cs_data_model$bio_pres

# Correct control parameters for lmer()
control_params1 <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 5e5))

# Extract all fixed effect terms from the full model (without the interaction term)
fixed_effects <- c("time", "cons", "cumulative_sampling_freq", "time_dur",'ID')

# Function to generate all subsets of a set (without the empty set)
generate_combinations <- function(effects) {
  unlist(lapply(1:length(effects), function(k) combn(effects, k, simplify = FALSE)), recursive = FALSE)
}

# Generate all combinations of the fixed effects
combinations <- generate_combinations(fixed_effects)

results_cs_ex_0 <- data.frame(Formula = character(length(combinations)),
                              AIC = numeric(length(combinations)),
                              R2_m = numeric(length(combinations)),
                              R2_c = numeric(length(combinations)),
                              stringsAsFactors = FALSE)

# Loop over all combinations and fit models
for (i in seq_along(combinations)) {
  # Create the fixed effects formula part
  fixed_formula <- paste(combinations[[i]], collapse = " + ")
  
  # Create the full formula for the model
  formula <- as.formula(paste("Dcs ~", fixed_formula, "+ (1 | Watercontactsite)"))
  
  # Fit the model
  model <- lmer(formula, data = cs_data_model, control = control_params1)
  
  # Store the formula, AIC, and R2 values
  results_cs_ex_0$Formula[i] <- paste(deparse(formula), collapse = " ")
  results_cs_ex_0$AIC[i] <- AIC(model)
  
  # Calculate R2 values
  r2_values <- r.squaredGLMM(model)
  results_cs_ex_0$R2_m[i] <- r2_values[1]
  results_cs_ex_0$R2_c[i] <- r2_values[2]
  
}

# Sort the results by AIC from smallest to largest
results_cs_ex_0 <- results_cs_ex_0 %>% arrange(AIC)

model_det = lmer(Dcs ~ cumulative_sampling_freq + time_dur + (1 | Watercontactsite), data = cs_data_model, control = control_params)
  
residuals <- residuals(model_expert)

qqnorm(residuals)
qqline(residuals, col = "red")

hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")
shapiro.test(residuals)

# Then go for gamma but only positive // maybe two gammas (one for the benefits / mistakes *-1)

# First revert the scaling

#cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 

# Calculate and save means and standard deviations
#means <- cs_data %>%
#  summarise(across(c(pp_oneweek, temp_modis, NDVI_q90_t, NDVI_aqua, NDWI_aqua, Turbidity_aqua, elev, flow_acc, TPI, SS, time, cons, cumulative_sampling_freq, time_dur), ~ mean(.x, na.rm = TRUE)))

#sds <- cs_data %>%
#  summarise(across(c(pp_oneweek, temp_modis, NDVI_q90_t, NDVI_aqua, NDWI_aqua, Turbidity_aqua, elev, flow_acc, TPI, SS, time, cons, cumulative_sampling_freq, time_dur), ~ sd(.x, na.rm = TRUE)))

# Revert scaling
#cs_data_reverted <- cs_data_model %>%
#  mutate(across(c(pp_oneweek, temp_modis, NDVI_q90_t, NDVI_aqua, NDWI_aqua, Turbidity_aqua, elev, flow_acc, TPI, SS, time, cons, cumulative_sampling_freq, time_dur),
#                ~ .x * sds[[cur_column()]] + means[[cur_column()]]))

# Convert the time variable 
# Sinusoidal transformation for time in minutes (0–1440)
#cs_data_reverted$time_sin <- sin(2 * pi * cs_data_reverted$time / 86400)

# Now let's split the data in positive and negative 

#head(cs_data_reverted)
#library(dplyr)

## THISSS


# Predictions in cs data 
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
  mutate(across(c(time_sin, time_cos, cons, cumulative_sampling_freq, time_dur), scale))

negative_Dcs_data_sc <- negative_Dcs_data %>%
  mutate(across(c(time_sin, time_cos, cons, cumulative_sampling_freq, time_dur), scale))

# Load necessary libraries
library(glmmTMB)
library(MuMIn)  # For R2 calculation
library(car)

# First correlations 

var_cs = positive_Dcs_data %>% 
  dplyr::select(time_sin, time_cos, cons, cumulative_sampling_freq, time_dur, Site.type) 

numeric_vars_cs <- var_cs %>%
  select_if(is.numeric)

numeric_vars_cs <- numeric_vars_cs %>%
  select_if(~ !any(is.na(.)))

correlation_matrix_cs <- cor(numeric_vars_cs, use = 'complete.obs',method = 's') 

#colnames(numeric_vars_ex)

# Visualize correlation matrix
colnames(correlation_matrix_cs) <- c("Time (sin)","Time (cos)", "Consistency", "Cum. samp. freq.", "Scooping duration")
rownames(correlation_matrix_cs) <- c("Time (sin)","Time (cos)", "Consistency", "Cum. samp. freq.", "Scooping duration")
corrplot(correlation_matrix_cs, method = "number", type= c('full'), order = 'alphabet', tl.cex = 0.8)

var_cs = negative_Dcs_data %>% 
  dplyr::select(time_sin, time_cos, cons, cumulative_sampling_freq, time_dur, Site.type) 

numeric_vars_cs <- var_cs %>%
  select_if(is.numeric)

numeric_vars_cs <- numeric_vars_cs %>%
  select_if(~ !any(is.na(.)))

correlation_matrix_cs <- cor(numeric_vars_cs, use = 'complete.obs',method = 's') 

#colnames(numeric_vars_ex)

# Visualize correlation matrix
colnames(correlation_matrix_cs) <- c("Time (sin)","Time (cos)", "Consistency", "Cum. samp. freq.", "Scooping duration")
rownames(correlation_matrix_cs) <- c("Time (sin)","Time (cos)", "Consistency", "Cum. samp. freq.", "Scooping duration")
corrplot(correlation_matrix_cs, method = "number", type= c('full'), order = 'alphabet', tl.cex = 0.8)

vif(lm(Dcs ~ time_sin + time_cos + cons + cumulative_sampling_freq + time_dur, data = positive_Dcs_data))

#install.packages("TMB",type="source")
# Summarize the response variable within each Watercontactsite
summary_stats <- positive_Dcs_data %>%
  group_by(Watercontactsite) %>%
  summarise(
    mean_response = mean(Dcs, na.rm = TRUE),
    sd_response = sd(Dcs, na.rm = TRUE),
    n = n()
  )

# View the summary statistics
print(summary_stats)


# Extract all fixed effect terms from the full model (without the interaction term)
fixed_effects <- c("time_sin",'time_cos', "cons", "cumulative_sampling_freq", "time_dur",'ID' )

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
  formula <- as.formula(paste("Dcs ~", fixed_formula, "+ (1|ID/Watercontactsite)"))
  
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
}

# Print the results sorted by AIC
results_cs_ex_1 <- results_cs_ex_1 %>% arrange(AIC)
print(results_cs_ex_1)

model_det_pos = glmmTMB(Dcs ~ time_sin + time_cos+ cumulative_sampling_freq + (1 | Watercontactsite), 
                    data = positive_Dcs_data_sc, 
                    family = beta_family(link = "logit"), 
                    control = control_params)

full_model_det_pos = glmmTMB(Dcs ~ time_sin + time_cos+ cumulative_sampling_freq + ID + cons + time_dur
                               + (1 | Watercontactsite), 
                             data = positive_Dcs_data_sc, 
                             family = beta_family(link = "logit"), 
                             control = control_params)

negative_Dcs_data_sc$ID <- factor(negative_Dcs_data_sc$ID, 
                                  levels = c("10", 
                                             setdiff(levels(negative_Dcs_data_sc$ID), "10")))



full_model_det_neg = glmmTMB(Dcs ~ time_sin + time_cos+ cumulative_sampling_freq + ID + cons + time_dur
                             + (1 | Watercontactsite), 
                             data = negative_Dcs_data_sc, 
                             family = beta_family(link = "logit"), 
                             control = control_params)


summary(model_det_pos)

model_det_pos_alt = glmmTMB(Dcs ~ time_sin + time_cos+ cumulative_sampling_freq + ID + (1 | Watercontactsite), 
                        data = positive_Dcs_data_sc, 
                        family = beta_family(link = "logit"), 
                        control = control_params)

summary(model_det_pos_alt)


full_model_det_pos |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      time_sin ~ "Time sin",
      time_cos ~ "Time cos",
      cumulative_sampling_freq ~ 'Cumulative samp. freq.',
      time_dur ~ "Samp. duration",
      cons ~ "Consistency"
    )
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

full_model_det_neg |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      time_sin ~ "Time sin",
      time_cos ~ "Time cos",
      cumulative_sampling_freq ~ 'Cumulative samp. freq.',
      time_dur ~ "Samp. duration",
      cons ~ "Consistency"
    )
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()





# Fit the full model with glmmTMB (or lme4, if applicable)
full_model_1 <- glmmTMB(Dcs ~ time_sin + time_cos + cons + cumulative_sampling_freq + time_dur
                       +  (1 | ID/Watercontactsite),
                      family = beta_family(link = "logit"), data = positive_Dcs_data_sc, control = control_params, na.action = na.fail)


full_model_2 <- glmmTMB(Dcs ~ time_sin + time_cos + cons + cumulative_sampling_freq + time_dur
                        + Site.type +  (1 | ID/Watercontactsite),
                        family = beta_family(link = "logit"), data = positive_Dcs_data_sc, control = control_params, na.action = na.fail)

full_model_3 <- glmmTMB(Dcs ~ time_sin + time_cos + cons + cumulative_sampling_freq + time_dur
                        + ID +  (1 | ID/Watercontactsite),
                        family = beta_family(link = "logit"), data = positive_Dcs_data_sc, control = control_params, na.action = na.fail)


# Fit the full model with glmmTMB (or lme4, if applicable)
full_model <- glmmTMB(Dcs ~ time_sin + time_cos + cons + cumulative_sampling_freq + time_dur
                      + ID +  (1 | Watercontactsite/ID),
                      family = beta_family(link = "logit"), data = positive_Dcs_data_sc, control = control_params, na.action = na.fail)


# Perform model selection and model averaging
model_set <- dredge(full_model)
model_avg <- model.avg(model_set)

# Summary of model-averaged importance
summary(model_avg)

# Fit the full model with glmmTMB (or lme4, if applicable)
full_model_n <- glmmTMB(Dcs ~ time_sin + time_cos + cons + cumulative_sampling_freq + time_dur
                      + ID +  (1 | Watercontactsite),
                      family = beta_family(link = "logit"), data = negative_Dcs_data_sc, control = control_params, na.action = na.fail)

# Perform model selection and model averaging
model_set_n <- dredge(full_model_n)
model_avg_n <- model.avg(model_set_n)

# Summary of model-averaged importance
summary(model_avg_n)


# Extract all fixed effect terms from the full model (without the interaction term)
fixed_effects <- c("time_sin",'time_cos', "cons", "cumulative_sampling_freq", "time_dur",'ID', 'Site.type' )

# Function to generate all subsets of a set (without the empty set)
generate_combinations <- function(effects) {
  unlist(lapply(1:length(effects), function(k) combn(effects, k, simplify = FALSE)), recursive = FALSE)
}

# Dropping columns due to rank deficient conditional model 
# Initialize storage for AIC, R2 values, and formulas
results_cs_ex_1a <- data.frame(Formula = character(length(combinations)),
                              AIC = numeric(length(combinations)),
                              R2_m = numeric(length(combinations)),
                              R2_c = numeric(length(combinations)),
                              stringsAsFactors = FALSE)

# Loop over all combinations and fit models
for (i in seq_along(combinations)) {
  # Create the fixed effects formula part
  fixed_formula <- paste(combinations[[i]], collapse = " + ")
  
  # Create the full formula for the model
  
  # If interaction term is present in the combination, include it
  if (all(c("Site.type", "time_sin") %in% combinations[[i]])) {
    fixed_formula <- paste(fixed_formula, "+ Site.type * time_sin")
  }
  
  # If interaction term is present in the combination, include it
  if (all(c("Site.type", "time_cos") %in% combinations[[i]])) {
    fixed_formula <- paste(fixed_formula, "+ Site.type * time_cos")
  }
  
  formula <- as.formula(paste("Dcs ~", fixed_formula, "+ (1|Watercontactsite)"))
  
  # Fit the model using Beta distribution with logit link
  model <- glmmTMB(formula, 
                   data = positive_Dcs_data_sc, 
                   family = beta_family(link = "logit"), 
                   control = control_params)
  
  # Store the formula and AIC
  results_cs_ex_1a$Formula[i] <- paste(deparse(formula), collapse = " ")
  results_cs_ex_1a$AIC[i] <- AIC(model)
  
  
  # Calculate pseudo-R² values using the `performance` package
  r2_values <- r2(model)
  
  # Store marginal and conditional R² values
  results_cs_ex_1a$R2_m[i] <- r2_values$R2_marginal
  results_cs_ex_1a$R2_c[i] <- r2_values$R2_conditional
}

# Print the results sorted by AIC
results_cs_ex_1a <- results_cs_ex_1a %>% arrange(AIC)

model_det_pos |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      time_sin ~ "Time - cyclical",
      cumulative_sampling_freq ~ "Cumulative sampling frequency"
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
}

# Print the results sorted by AIC
results_cs_ex_2 <- results_cs_ex_2 %>% arrange(AIC)
print(results_cs_ex_2)

model_det_neg = glmmTMB(Dcs ~ time_sin + cumulative_sampling_freq + ID + (1 | Watercontactsite), 
                        data = negative_Dcs_data_sc, 
                        family = beta_family(link = "logit"), 
                        control = control_params)



model_nested_neg = glmmTMB(Dcs ~ time_sin + cumulative_sampling_freq + ID + (1 | ID/Watercontactsite), 
        data = negative_Dcs_data_sc, 
        family = beta_family(link = "logit"), 
        control = control_params)

summary(model_det_neg)

model_det_neg |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      time_sin ~ "Time sin",
      cumulative_sampling_freq ~ 'Cumulative samp. freq.'
    )
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

model_det_pos_alt |>
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = label_style_pvalue(digits = 2),
    label = list(
      time_sin ~ "Time sin",
      time_cos ~ "Time cos",
      cumulative_sampling_freq ~ 'Cumulative samp. freq.'
    )
  ) |>
  bold_p(t = 0.05) |>
  bold_labels() |>
  italicize_levels()

# Interpreting the results: 

# Predicted probability of false negative in my data

positive_Dcs_data_sc$time_sin_nsc = positive_Dcs_data$time_sin
positive_Dcs_data_sc$predicted_prob <- predict(model_det_pos, newdata = positive_Dcs_data_sc, type = "response")


# Convert time_seconds to a more readable format (HH:MM)
positive_Dcs_data_sc$time_of_the_day <- format(as.POSIXct(positive_Dcs_data_sc$time, origin="1970-01-01", tz="UTC"), "%H:%M")

# Plotting the predictions
ggplot(positive_Dcs_data_sc, aes(x = time, y = predicted_prob)) +
  geom_point(color = "darkblue") +
  labs(x = "Time of the day (hours)", y = "Predicted probability of false negatives") +
  scale_x_continuous(breaks = seq(0, 86400, by = 7200), labels = function(x) format(as.POSIXct(x, origin = "1970-01-01"), format = "%H:%M")) +
  theme_minimal()

# Create a density plot
ggplot(positive_Dcs_data_sc, aes(x = time, y = predicted_prob)) +
  geom_density_2d_filled(aes(fill = ..level..), contour_var = "ndensity") +
  labs(x = "Time of the day (hours)",
       y = "Predicted probability of false negatives", fill = 'Density') +
  scale_x_continuous(breaks = seq(0, 86400, by = 7200), 
                     labels = function(x) format(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"), format = "%H:%M")) +
  theme_minimal()

library(caret)

# Check for near-zero variance
nzv <- nearZeroVar(positive_Dcs_data, saveMetrics = TRUE)
nzv

summary(positive_Dcs_data)
str(positive_Dcs_data)

# Check for any missing values
any(is.na(positive_Dcs_data))

# 'check this change instead of boby

control_params <- glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 1e6))

library(car)
vif(lm(Dcs ~ time + cons + cumulative_sampling_freq + time_dur, data = positive_Dcs_data))


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
  formula <- as.formula(paste("Dcs ~", fixed_formula, "+ (1 | ID/Watercontactsite)"))
  
  # Fit the model using Beta distribution
  model <- glmmTMB(formula, 
                   data = positive_Dcs_data, 
                   family = beta_family(link = "logit")
                   )
  
  # Store the formula, AIC, and R2 values
  results_cs_ex_1$Formula[i] <- paste(deparse(formula), collapse = " ")
  results_cs_ex_1$AIC[i] <- AIC(model)
  
  # Calculate R2 values
  r2_values <- r.squaredGLMM(model)
  results_cs_ex_1$R2_m[i] <- r2_values[1]
  results_cs_ex_1$R2_c[i] <- r2_values[4]
}

# Sort the results by AIC from smallest to largest
results_cs_ex_1 <- results_cs_ex_1 %>% arrange(AIC)

model_positive <- glmer(Dcs ~ time + (1 | Watercontactsite), data = positive_Dcs_data, family = Gamma(link = "log"), control = control_params)

summary(model_positive)


library(ggplot2)

positive_Dcs_data$ID = as.factor(positive_Dcs_data$ID)

ggplot(positive_Dcs_data, aes(x = Watercontactsite, y = Dcs, col = ID)) +
  geom_boxplot() +
  labs( x = "Watercontactsite", y = "Imperfect detection (positive)") +
  theme_minimal() +
  coord_flip()  # Flip for better readability

negative_Dcs_data$Dcs = negative_Dcs_data$Dcs*(-1)

negative_Dcs_data$ID = as.factor(negative_Dcs_data$ID)

ggplot(negative_Dcs_data, aes(x = Watercontactsite, y = Dcs, col = ID)) +
  geom_boxplot() +
  labs( x = "Watercontactsite", y = "Imperfect detection (negative)") +
  theme_minimal() +
  coord_flip()  # Flip for better readability



# Negative

vif(lm(Dcs ~ time + cons + cumulative_sampling_freq + time_dur, data = negative_Dcs_data))

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
  formula <- as.formula(paste("Dcs ~", fixed_formula, "+ (1 | Watercontactsite)"))
  
  # Fit the model using Gamma distribution
  model <- glmer(formula, data = negative_Dcs_data, family = Gamma(link = "log"), control = control_params)
  
  # Store the formula, AIC, and R2 values
  results_cs_ex_2$Formula[i] <- paste(deparse(formula), collapse = " ")
  results_cs_ex_2$AIC[i] <- AIC(model)
  
  # Calculate R2 values
  r2_values <- r.squaredGLMM(model)
  results_cs_ex_2$R2_m[i] <- r2_values[1]
  results_cs_ex_2$R2_c[i] <- r2_values[4]
}

# Sort the results by AIC from smallest to largest
results_cs_ex_2 <- results_cs_ex_2 %>% arrange(AIC)

model_negative <- glmer(Dcs ~ time + cons + (1 | Watercontactsite), data = negative_Dcs_data, family = Gamma(link = "log"), control = control_params)

summary(model_negative)




cs_data_model$Dcs_t <- log(cs_data_model$Dcs+1)

cs_data_model$Dcs_t  <- ifelse(cs_data_model$Dcs > median(cs_data_model$Dcs), 1, 0)

summary(cs_data_model$Dcs_t)

hist(cs_data_model$PDe)

summary(cs_data_model$PDe)

# First let's try it in the expert data to define threshold

ex_data_model$PDe <- predict(model_expert, newdata = ex_data_model, type = "response")

# Define a range of thresholds
thresholds <- seq(0.1, 0.9, by = 0.1)

# Initialize storage for metrics
metrics <- data.frame(threshold = thresholds, precision = NA, recall = NA, f1_score = NA)

# Calculate metrics for each threshold
for (t in thresholds) {
  binary_preds <- ifelse(ex_data_model$PDe >= t, 1, 0)
  confusion <- table(Predicted = binary_preds, Actual = ex_data_model$bio_pres )
  
  precision <- confusion[2,2] / sum(confusion[2,])
  recall <- confusion[2,2] / sum(confusion[,2])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  metrics[metrics$threshold == t, "precision"] <- precision
  metrics[metrics$threshold == t, "recall"] <- recall
  metrics[metrics$threshold == t, "f1_score"] <- f1_score
}

# Print the metrics
print(metrics)


# Expertise model 

cs_data_model$PDe_bin <- with(cs_data_model, ifelse(cs_data_model$PDe >= 0.5, 1, 0))

# Create binary response variable
cs_data_model$Dcs_bin <- cs_data_model$PDe_bin - cs_data_model$bio_pres

table(cs_data_model$Dcs_bin)

# Create a contingency table
contingency_table <- table(cs_data_model$Site.type, cs_data_model$Dcs_bin)

# Print the contingency table
print(contingency_table)

contingency_table1 <- table(cs_data_model$Watercontactsite, cs_data_model$Dcs_bin)
print(contingency_table1)

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 

# Calculate and save means and standard deviations
means <- cs_data %>%
  summarise(across(c(pp_oneweek, temp_modis, NDVI_q90_t, NDVI_aqua, NDWI_aqua, Turbidity_aqua, elev, flow_acc, TPI, SS, time, cons, cumulative_sampling_freq, time_dur), ~ mean(.x, na.rm = TRUE)))

sds <- cs_data %>%
  summarise(across(c(pp_oneweek, temp_modis, NDVI_q90_t, NDVI_aqua, NDWI_aqua, Turbidity_aqua, elev, flow_acc, TPI, SS, time, cons, cumulative_sampling_freq, time_dur), ~ sd(.x, na.rm = TRUE)))

# Revert scaling
cs_data_reverted <- cs_data_model %>%
  mutate(across(c(pp_oneweek, temp_modis, NDVI_q90_t, NDVI_aqua, NDWI_aqua, Turbidity_aqua, elev, flow_acc, TPI, SS, time, cons, cumulative_sampling_freq, time_dur),
                ~ .x * sds[[cur_column()]] + means[[cur_column()]]))

# Filter 
# Find Watercontactsites with at least one Dcs_bin = 1
sites_with_ones <- cs_data_reverted %>%
  filter(Dcs_bin == 1) %>%
  pull(Watercontactsite) %>%
  unique() 

# Filter the data to keep only the Watercontactsites with Dcs_bin = 1
filtered_data <- cs_data_reverted %>%
  filter(Watercontactsite %in% sites_with_ones)

# Convert Watercontactsite to factor if it is not already
filtered_data$Watercontactsite <- factor(filtered_data$Watercontactsite)

# Drop unused levels
filtered_data$Watercontactsite <- droplevels(filtered_data$Watercontactsite)

contingency_table2 <- table(filtered_data$Watercontactsite, filtered_data$Dcs_bin)
print(contingency_table2)

# Remove observations with -1 

# Assuming cs_data_model is your dataset
filtered_data <- filtered_data %>%
  filter(Dcs_bin != -1)

# Check the result
table(filtered_data$Dcs_bin)

contingency_table3 <- table(filtered_data$Watercontactsite, filtered_data$Dcs_bin)
print(contingency_table3)

# Model alternative 1 without removing sampling 

# Scale the data
cs_model_ex_1 <- filtered_data %>%
  mutate(across(c(time, cons, cumulative_sampling_freq, time_dur), scale))

# Full formula 

full_formula_1 <- Dcs ~ time_dur + cumulative_sampling_freq + cons + time 

# Extract all fixed effect terms from the full model (without the interaction term)
fixed_effects <- c("time", "cons", "cumulative_sampling_freq", "time_dur")

# Function to generate all subsets of a set (without the empty set)
generate_combinations <- function(effects) {
  unlist(lapply(1:length(effects), function(k) combn(effects, k, simplify = FALSE)), recursive = FALSE)
}

# Generate all combinations of the fixed effects
combinations <- generate_combinations(fixed_effects)

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
  formula <- as.formula(paste("Dcs_bin ~", fixed_formula, "+ (1 | Watercontactsite)"))
  
  # Fit the model
  model <- glmer(formula, data = cs_model_ex_1, family = binomial, control = control_params)
  
  # Store the formula, AIC, and R2 values
  results_cs_ex_1$Formula[i] <- paste(deparse(formula), collapse = " ")
  results_cs_ex_1$AIC[i] <- AIC(model)
  
  # Calculate R2 values
  r2_values <- r.squaredGLMM(model)
  results_cs_ex_1$R2_m[i] <- r2_values[1]
  results_cs_ex_1$R2_c[i] <- r2_values[3]
  
}

# Sort the results by AIC from smallest to largest
results_cs_ex_1 <- results_cs_ex_1 %>% arrange(AIC)

write.csv(results_cs_ex_1, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_models_cs_ex_1.csv", row.names = FALSE)

# Model cs

model_cs_ex_1 <- glmer(Dcs_bin ~  cons + cumulative_sampling_freq + time_dur + (1 | Watercontactsite),
                  data = cs_model_ex_1, family = binomial,
                  control = control_params)
summary(model_cs_ex_1)

# Model alternative 2 removing sampling 

# Function to balance the dataset for a single Watercontactsite
balance_site <- function(df) {
  # Find the number of 1s and 0s in Dcs_bin
  count_1 <- sum(df$Dcs_bin == 1)
  count_0 <- sum(df$Dcs_bin == 0)
  
  # Determine the smaller count
  min_count <- min(count_1, count_0)
  
  # Randomly sample min_count 1s and min_count 0s
  df_1 <- df %>%
    filter(Dcs_bin == 1) %>%
    sample_n(min_count)
  
  df_0 <- df %>%
    filter(Dcs_bin == 0) %>%
    sample_n(min_count)
  
  # Combine the sampled data
  balanced_df <- bind_rows(df_1, df_0)
  
  return(balanced_df)
}

# Apply the balance function to each Watercontactsite
balanced_data <- filtered_data %>%
  group_by(Watercontactsite) %>%
  do(balance_site(.)) %>%
  ungroup()

# Ungroup the data (optional, to remove the grouping structure)
balanced_data <- ungroup(balanced_data)

# Check the result
table(balanced_data$Watercontactsite, balanced_data$Dcs_bin)


# Scale the data
cs_model_ex_2 <- balanced_data %>%
  mutate(across(c(time, cons, cumulative_sampling_freq, time_dur), scale))

# Full formula 

full_formula_2 <- Dcs ~ time_dur + cumulative_sampling_freq + cons + time 

# Extract all fixed effect terms from the full model (without the interaction term)
fixed_effects <- c("time", "cons", "cumulative_sampling_freq", "time_dur")

# Function to generate all subsets of a set (without the empty set)
generate_combinations <- function(effects) {
  unlist(lapply(1:length(effects), function(k) combn(effects, k, simplify = FALSE)), recursive = FALSE)
}

# Generate all combinations of the fixed effects
combinations <- generate_combinations(fixed_effects)

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
  formula <- as.formula(paste("Dcs_bin ~", fixed_formula, "+ (1 | ID)"))
  
  # Fit the model
  model <- glmer(formula, data = cs_model_ex_2, family = binomial, control = control_params)
  
  # Store the formula, AIC, and R2 values
  results_cs_ex_2$Formula[i] <- paste(deparse(formula), collapse = " ")
  results_cs_ex_2$AIC[i] <- AIC(model)
  
  # Calculate R2 values
  r2_values <- r.squaredGLMM(model)
  results_cs_ex_2$R2_m[i] <- r2_values[1]
  results_cs_ex_2$R2_c[i] <- r2_values[3]
  
}

# Sort the results by AIC from smallest to largest
results_cs_ex_2 <- results_cs_ex_2 %>% arrange(AIC)

write.csv(results_cs_ex_1, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_models_cs_ex_1.csv", row.names = FALSE)















# Without SS
model_expert2 <- glmer(bio_pres ~ pp_oneweek + temp_modis + Site.type*NDVI_q90_t + 
                         elev + (1 | Watercontactsite),
                       data = ex_data_model, family = binomial,
                       control = control_params)

summary(model_expert2)

# Without temp

model_expert3 <-  glmer(bio_pres ~ pp_oneweek  + Site.type*NDVI_q90_t + 
                          elev + (1 | Watercontactsite),
                        data = ex_data_model, family = binomial,
                        control = control_params)

summary(model_expert3)

# Without pp

model_expert4 <-  glmer(bio_pres ~ Site.type*NDVI_q90_t + elev + (1 | Watercontactsite),
                        data = ex_data_model, family = binomial,
                        control = control_params)

summary(model_expert4)

# Best model following the AIC finished there 

full_model_cs_1 <- glmer(bio_pres ~ pp_oneweek + temp_modis + Site.type*NDVI_q90_t + 
                               elev + flow_acc + SS + (1 | Watercontactsite),
                             data = cs_data_model, family = binomial,
                             control = control_params)
summary(full_model_cs_1)

# Without elev

model_cs1 <- glmer(bio_pres ~ pp_oneweek + temp_modis + Site.type*NDVI_q90_t + flow_acc + SS + 
                     (1 | Watercontactsite), data = cs_data_model, family = binomial,
                   control = control_params)

summary(model_cs1)

# Without flow_acc

model_cs2 <- glmer(bio_pres ~ pp_oneweek + temp_modis + Site.type*NDVI_q90_t  + SS + 
                     (1 | Watercontactsite), data = cs_data_model, family = binomial,
                   control = control_params)

summary(model_cs2)

# Without pp_oneweek

model_cs3 <- glmer(bio_pres ~ temp_modis + Site.type*NDVI_q90_t  + SS + 
                     (1 | Watercontactsite), data = cs_data_model, family = binomial,
                   control = control_params)

summary(model_cs3)

# Without temp

model_cs4 <- glmer(bio_pres ~ Site.type*NDVI_q90_t  + SS + 
                     (1 | Watercontactsite), data = cs_data_model, family = binomial,
                   control = control_params)

summary(model_cs4)

# Without SS

model_cs5 <- glmer(bio_pres ~ Site.type*NDVI_q90_t + 
                     (1 | Watercontactsite), data = cs_data_model, family = binomial,
                   control = control_params)

summary(model_cs5)

# Best model model_cs5

# Lake models 

# Scaling needs to be done for the subset

ex_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_complete.csv", sep = ",", header = TRUE) 

ex_data_model_lake <- na.omit(ex_data)

ex_data_lake_scaled <- ex_data_model_lake %>%
  mutate(across(c(pp_oneweek,temp_modis,NDVI_q90_t, NDVI_aqua, NDWI_aqua,Turbidity_aqua, elev, flow_acc, TPI, SS), scale))

lake_expert1 <- glmer(bio_pres ~ pp_oneweek + temp_modis + NDVI_q90_t + 
                        NDWI_aqua + Turbidity_aqua  +
                               elev + flow_acc + SS + (1 | Watercontactsite),
                             data = ex_data_lake_scaled, family = binomial,
                             control = control_params)
summary(lake_expert1)

# Without pp

lake_expert2 <- glmer(bio_pres ~ temp_modis + NDVI_q90_t + 
                        NDWI_aqua + Turbidity_aqua  +
                        elev + flow_acc + SS + (1 | Watercontactsite),
                      data = ex_data_lake_scaled, family = binomial,
                      control = control_params)
summary(lake_expert2)

# Without flow_acc

lake_expert3 <- glmer(bio_pres ~ temp_modis + NDVI_q90_t + 
                        NDWI_aqua + Turbidity_aqua  +
                        elev  + SS + (1 | Watercontactsite),
                      data = ex_data_lake_scaled, family = binomial,
                      control = control_params)
summary(lake_expert3)

# Without elev

lake_expert4 <- glmer(bio_pres ~ temp_modis + NDVI_q90_t + 
                        NDWI_aqua + Turbidity_aqua  +
                          SS + (1 | Watercontactsite),
                      data = ex_data_lake_scaled, family = binomial,
                      control = control_params)
summary(lake_expert4)

# Without NDWI_aqua

lake_expert5 <- glmer(bio_pres ~ temp_modis + NDVI_q90_t + Turbidity_aqua  +
                        SS + (1 | Watercontactsite),
                      data = ex_data_lake_scaled, family = binomial,
                      control = control_params)
summary(lake_expert5)


# Without temp

lake_expert6 <- glmer(bio_pres ~ NDVI_q90_t + Turbidity_aqua  +
                        SS + (1 | Watercontactsite),
                      data = ex_data_lake_scaled, family = binomial,
                      control = control_params)
summary(lake_expert6)

# Without SS // This is the best model 

lake_expert7 <- glmer(bio_pres ~ NDVI_q90_t + Turbidity_aqua  +
                       (1 | Watercontactsite),
                      data = ex_data_lake_scaled, family = binomial,
                      control = control_params)
summary(lake_expert7)

## CS lake model 

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 

cs_data_model_lake <- na.omit(cs_data)

cs_data_lake_scaled <- cs_data_model_lake %>%
  mutate(across(c(pp_oneweek,temp_modis,NDVI_q90_t, NDVI_aqua, NDWI_aqua,Turbidity_aqua, elev, flow_acc, TPI, SS), scale))

lake_cs1 <- glmer(bio_pres ~ pp_oneweek + temp_modis + NDVI_q90_t + 
                        NDWI_aqua + Turbidity_aqua  +
                        elev + flow_acc + SS + (1 | Watercontactsite),
                      data = cs_data_lake_scaled, family = binomial,
                      control = control_params)
summary(lake_cs1)

# without flow acc

lake_cs2 <- glmer(bio_pres ~ pp_oneweek + temp_modis + NDVI_q90_t + 
                    NDWI_aqua + Turbidity_aqua  +
                    elev  + SS + (1 | Watercontactsite),
                  data = cs_data_lake_scaled, family = binomial,
                  control = control_params)
summary(lake_cs2)

# without temp

lake_cs3 <- glmer(bio_pres ~ pp_oneweek  + NDVI_q90_t + 
                    NDWI_aqua + Turbidity_aqua  +
                    elev  + SS + (1 | Watercontactsite),
                  data = cs_data_lake_scaled, family = binomial,
                  control = control_params)
summary(lake_cs3)

# without elev

lake_cs4 <- glmer(bio_pres ~ pp_oneweek  + NDVI_q90_t + 
                    NDWI_aqua + Turbidity_aqua
                      + SS + (1 | Watercontactsite),
                  data = cs_data_lake_scaled, family = binomial,
                  control = control_params)
summary(lake_cs4)


### Trial plots 

# First 

# Plot cumulative sampling frequency original against prob

positive_Dcs_data$predicted_prob <- predict(model_det_pos_alt, newdata = positive_Dcs_data_sc, type = "response")

scatterplot(positive_Dcs_data$cumulative_sampling_freq, positive_Dcs_data$predicted_prob)

negative_Dcs_data$predicted_prob <- predict(model_det_neg, newdata = negative_Dcs_data_sc, type = "response")

scatterplot(negative_Dcs_data$cumulative_sampling_freq, negative_Dcs_data$predicted_prob)



