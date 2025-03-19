# Pre modeling 

library(ggplot2)
library(scales)
library(lme4)
library(tidyverse)
library(corrplot)


rm(list=ls())

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 
ex_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_complete.csv", sep = ",", header = TRUE) 

## Correlation analysis 

var_ex = ex_data %>% 
  dplyr::select(pp_oneweek,temp_modis,NDVI_q90_t, elev, flow_acc, TPI, SS, Site.type) 

var_cs = cs_data %>% 
  dplyr::select(pp_oneweek,temp_modis,NDVI_q90_t, elev, flow_acc, TPI, SS,time_sin, time_cos,cons,cumulative_sampling_freq,time_dur,Site.type, samp_den) 

numeric_vars_ex <- var_ex %>%
  select_if(is.numeric)

numeric_vars_ex <- numeric_vars_ex %>%
  select_if(~ !any(is.na(.)))

correlation_matrix_ex <- cor(numeric_vars_ex, use = 'complete.obs',method = 's') 

#colnames(numeric_vars_ex)

# Visualize correlation matrix
colnames(correlation_matrix_ex) <- c("Precipitation", "Temperature", "NDVI", "Elevation", "Flow\naccumulation", 'Topographic\nIndex', 'Slope\nsteepness')
rownames(correlation_matrix_ex) <- c("Precipitation", "Temperature", "NDVI", "Elevation", "Flow\naccumulation", 'Topographic\nIndex', 'Slope\nsteepness')
corrplot(correlation_matrix_ex, method = "number", type= c('full'), order = 'alphabet', tl.cex = 0.8)

numeric_vars_cs <- var_cs %>%
  select_if(is.numeric)

numeric_vars_cs <- numeric_vars_cs %>%
  select_if(~ !any(is.na(.)))

numeric_vars_cs = numeric_vars_cs[1:7]

correlation_matrix_cs <- cor(numeric_vars_cs, use = 'complete.obs',method = 's') 

# Visualize correlation matrix
colnames(correlation_matrix_cs) <- c("Precipitation", "Temperature", "NDVI", "Elevation", "Flow\naccumulation", 'Topographic\nIndex', 'Slope\nsteepness')
rownames(correlation_matrix_cs) <- c("Precipitation", "Temperature", "NDVI", "Elevation", "Flow\naccumulation", 'Topographic\nIndex', 'Slope\nsteepness')
corrplot(correlation_matrix_cs, method = "number", type= c('full'), order = 'alphabet', tl.cex = 0.8)

# Set up a plotting area with 2 columns for side-by-side plots
par(mfrow = c(1, 2))  # 1 row, 2 columns

# Adjust margins to make space for titles
par(mar = c(0, 0, 0, 0))  # bottom, left, top, right



rm(list=ls())

# Read expert model 

model_expert = readRDS("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/PD_expert.RDS")

# Predictions in cs data 

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 

cs_data_model <- cs_data %>%
  mutate(across(c(pp_oneweek,temp_modis,NDVI_q90_t, elev, flow_acc, SS), scale))

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


det_false_neg <- positive_Dcs_data %>%
  dplyr::select(time_sin, time_cos, cons, cumulative_sampling_freq,time_dur,samp_den)


det_unex_obs <- negative_Dcs_data %>%
  dplyr::select(time_sin, time_cos, cons, cumulative_sampling_freq,time_dur,samp_den)


# Set up a plotting area with 2 columns for side-by-side plots
par(mfrow = c(1, 2))  # 1 row, 2 columns

# Adjust margins to make space for titles
par(mar = c(0, 0, 0, 0))  # bottom, left, top, right

correlation_matrix_cs_FN <- cor(det_false_neg, use = 'complete.obs',method = 's') 

# Visualize correlation matrix
colnames(correlation_matrix_cs_FN) <- c("Sampling time (sin)","Sampling time (cos)", "Consistency", "Cumulative sampling\nfrequency", "Sampling duration",'Sampling density' )
rownames(correlation_matrix_cs_FN) <- c("Sampling time (sin)","Sampling time (cos)","Consistency", "Cumulative sampling\nfrequency", "Sampling duration", 'Sampling density')
corrplot(correlation_matrix_cs_FN, method = "number", type= c('full'), order = 'alphabet', tl.cex = 0.8)

correlation_matrix_cs_UO <- cor(det_unex_obs, use = 'complete.obs',method = 's') 

# Visualize correlation matrix
colnames(correlation_matrix_cs_UO) <- c("Sampling time (sin)","Time of the day (cos)", "Consistency", "Cumulative sampling\nfrequency", "Sampling duration",'Sampling density' )
rownames(correlation_matrix_cs_UO) <- c("Sampling time (sin)","Time of the day (cos)","Consistency", "Cumulative sampling\nfrequency", "Sampling duration", 'Sampling density')
corrplot(correlation_matrix_cs_UO, method = "number", type= c('full'), order = 'alphabet', tl.cex = 0.8)

# Load necessary libraries
library(car)
library(lme4)

# Extract only the fixed effect part of the model
# You can extract the fixed effects part of the formula and refit without random effects for VIF calculation

# VIF model for the expert 

rm(list=ls())

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 
ex_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_complete.csv", sep = ",", header = TRUE) 


# Convert categorical variables to factors
ex_data$Site.type <- as.factor(ex_data$Site.type)
cs_data$Site.type <- as.factor(cs_data$Site.type)

fixed_model_ex <- glm(bio_pres ~ pp_oneweek + temp_modis + Site.type * NDVI_q90_t + 
                     elev + flow_acc + SS + TPI, 
                   data = ex_data, family = binomial)

# Calculate VIF for the fixed effects
vif(fixed_model_ex) 

fixed_model_cs <- glm(bio_pres ~ pp_oneweek + temp_modis + Site.type * NDVI_q90_t + 
                        elev + flow_acc + SS + TPI, 
                      data = cs_data, family = binomial)
vif(fixed_model_cs) 

# Load necessary libraries
library(glmmTMB)
library(MuMIn)  # For R2 calculation
library(car)

rm(list=ls())

# Read expert model 

model_expert = readRDS("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/PD_expert.RDS")

# Predictions in cs data 

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", sep = ",", header = TRUE) 

cs_data_model <- cs_data %>%
  mutate(across(c(pp_oneweek,temp_modis,NDVI_q90_t, elev, flow_acc, SS), scale))

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


det_false_neg <- positive_Dcs_data %>%
  dplyr::select(time_sin, time_cos, cons, cumulative_sampling_freq,time_dur,samp_den, Dcs)


det_unex_obs <- negative_Dcs_data %>%
  dplyr::select(time_sin, time_cos, cons, cumulative_sampling_freq,time_dur,samp_den, Dcs)

det_unex_obs$Dcs = det_unex_obs$Dcs*(-1)

control_params <- glmmTMBControl(optCtrl = list(iter.max = 1e5, eval.max = 1e5))

fixed_model_fn <- glmmTMB(Dcs ~ time_sin + time_cos + cons + cumulative_sampling_freq + time_dur + samp_den, 
                      data = det_false_neg, 
                      family = beta_family(link = "logit"), 
                      control = control_params)


fixed_model_uo <- glm(Dcs ~ time_sin + time_cos + cons + cumulative_sampling_freq + time_dur + samp_den, 
                      data = det_unex_obs, family = beta_family(link = "logit"), 
                      control = control_params)


library(performance)  # Provides check_collinearity()

# Compute VIF for fixed effects
vif_results_fn <- check_collinearity(fixed_model_fn)

vif_results_uo <- check_collinearity(fixed_model_uo)

cbind(vif_fn = vif_results_fn$VIF,vif_uo = vif_results_uo$VIF )






##############

# Graph to see overlap of the sampling instances 

library(dplyr)
library(ggplot2)

# Select needed columns and add a Source column
ex_data_model_selected <- ex_data %>%
  select(Watercontactsite, date) %>%
  mutate(Source = "Expert")

cs_data_model_selected <- cs_data %>%
  select(Watercontactsite, date) %>%
  mutate(Source = "Citizen")

# Combine the selected columns
combined_data <- bind_rows(ex_data_model_selected, cs_data_model_selected)

# Ensure date is in Date format
combined_data$date <- ymd(combined_data$date)

library(ggplot2)

ggplot(combined_data, aes(x = date, y = Watercontactsite, color = Source)) +
  geom_point(position = position_dodge(width = 0.8), size =2, shape=45) +  # Smaller points
  scale_x_date(
    date_breaks = "4 months", 
    date_labels = "%b %Y"
  ) +
  scale_color_manual(
    values = c("Expert" = "#104862", "Citizen" = "#83CBEB")  # Custom colors
  ) +
  labs(
    x = "Date",
    y = "Water contact site",
    color = "Data Source"
  ) +
  theme_minimal() +  # Clean minimal theme
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # White background
    panel.grid = element_line(color = "#e0e0e0"),  # Lighter grid lines
    axis.text.x = element_text(angle = 45, hjust = 1),  # Angled x-axis labels
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "top",  # Legend on top
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )

ggsave("plot_sampling_period1.pdf", width = 13, height = 8.5, units='in', limitsize = F)
#http://127.0.0.1:38011/graphics/plot_zoom_png?width=1280&height=799
