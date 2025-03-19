# Load necessary libraries
library(lme4)
library(performance)

# Fit the full model
model_expert <- glmer(bio_pres ~ pp_oneweek + Site.type*NDVI_q90_t + elev + (1 | Watercontactsite),
                      data = ex_data_model, family = binomial,
                      control = control_params)

model_N_pp = glmer(bio_pres ~ Site.type*NDVI_q90_t + elev + (1 | Watercontactsite),
                   data = ex_data_model, family = binomial,
                   control = control_params)

model_N_elev <- glmer(bio_pres ~ pp_oneweek + Site.type*NDVI_q90_t + (1 | Watercontactsite),
                      data = ex_data_model, family = binomial,
                      control = control_params)

model_N_NDVI <- glmer(bio_pres ~ pp_oneweek + Site.type  + elev + (1 | Watercontactsite),
                      data = ex_data_model, family = binomial,
                      control = control_params)

model_N_Site <- glmer(bio_pres ~ pp_oneweek + NDVI_q90_t + elev + (1 | Watercontactsite),
                      data = ex_data_model, family = binomial,
                      control = control_params)

model_N_inte <- glmer(bio_pres ~ pp_oneweek + Site.type + NDVI_q90_t + elev + (1 | Watercontactsite),
                      data = ex_data_model, family = binomial,
                      control = control_params)

# Calculate partial R^2 for each variable individually

full_R = r2(model_expert)
N_pp_R = r2(model_N_pp)
N_elev_R = r2(model_N_elev)
N_NDVI_R = r2(model_N_NDVI)
N_Site_R = r2(model_N_Site)
N_inter_R = r2(model_N_inte)

# Partial R^2 for each variable
pp_R = full_R$R2_marginal - N_pp_R$R2_marginal
elev_R = full_R$R2_marginal - N_elev_R$R2_marginal
NDVI_R = full_R$R2_marginal - N_NDVI_R$R2_marginal
Site_R = full_R$R2_marginal - N_Site_R$R2_marginal
inter_R = full_R$R2_marginal - N_inter_R$R2_marginal

rm(list=ls())

control_params <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 5e5))

# Load required library
library(performance)  # Make sure 'performance' is installed

# Fit the full model
model_cs <- glmer(bio_pres ~ Site.type + NDVI_q90_t + flow_acc + SS + Site.type * NDVI_q90_t + (1 | Watercontactsite),
                  data = cs_data_model, family = binomial,
                  control = control_params)

# Fit the reduced models (one predictor removed each time)
model_N_Site_cs <- glmer(bio_pres ~ NDVI_q90_t + flow_acc + SS + (1 | Watercontactsite),
                         data = cs_data_model, family = binomial,
                         control = control_params)

model_N_NDVI_cs <- glmer(bio_pres ~ Site.type + flow_acc + SS  + (1 | Watercontactsite),
                         data = cs_data_model, family = binomial,
                         control = control_params)

model_N_flow_cs <- glmer(bio_pres ~  SS + Site.type * NDVI_q90_t + (1 | Watercontactsite),
                         data = cs_data_model, family = binomial,
                         control = control_params)

model_N_SS_cs <- glmer(bio_pres ~ flow_acc + Site.type * NDVI_q90_t + (1 | Watercontactsite),
                       data = cs_data_model, family = binomial,
                       control = control_params)

model_N_interaction_cs <- glmer(bio_pres ~ Site.type + NDVI_q90_t + flow_acc + SS + (1 | Watercontactsite),
                                data = cs_data_model, family = binomial,
                                control = control_params)

# Calculate marginal R^2 for each model using the performance package
full_R_cs = r2(model_cs)
N_Site_R_cs = r2(model_N_Site_cs)
N_NDVI_R_cs = r2(model_N_NDVI_cs)
N_flow_R_cs = r2(model_N_flow_cs)
N_SS_R_cs = r2(model_N_SS_cs)
N_interaction_R_cs = r2(model_N_interaction_cs)

# Calculate partial R^2 for each variable by subtracting the reduced model R^2 from the full model R^2
Site_R_cs = full_R_cs$R2_marginal - N_Site_R_cs$R2_marginal
NDVI_R_cs = full_R_cs$R2_marginal - N_NDVI_R_cs$R2_marginal
flow_R_cs = full_R_cs$R2_marginal - N_flow_R_cs$R2_marginal
SS_R_cs = full_R_cs$R2_marginal - N_SS_R_cs$R2_marginal
interaction_R_cs = full_R_cs$R2_marginal - N_interaction_R_cs$R2_marginal

# Print results for interpretation
cat("Partial R^2 for Site.type: ", Site_R_cs, "\n")
cat("Partial R^2 for NDVI_q90_t: ", NDVI_R_cs, "\n")
cat("Partial R^2 for flow_acc: ", flow_R_cs, "\n")
cat("Partial R^2 for SS: ", SS_R_cs, "\n")
cat("Partial R^2 for interaction (Site.type * NDVI_q90_t): ", interaction_R_cs, "\n")

