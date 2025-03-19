library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

cs_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_env_geo.csv", sep = ",", header = TRUE) 
ex_data <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_env_geo.csv", sep = ",", header = TRUE) 

# Detection variables 

# Time duration is done as difference time in minutes (match with original file)
# Consider Watercontactsite and date

original_cs <- read.csv("~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_filtered.csv", sep = ",", header = TRUE) 

names(original_cs)[names(original_cs)=='today'] = 'date'
names(original_cs)[names(original_cs)=='What.is.the.time.now.'] = 'initial_time'
names(original_cs)[names(original_cs)=='You.are.done.scooping..What.is.the.time.now.'] = 'final_time'

original_cs$date = as.Date(original_cs$date)

cs_data$date = as.Date(cs_data$date)

original_cs = original_cs %>% dplyr::select("Watercontactsite",'date', "initial_time", "final_time") 

table(duplicated(original_cs[, c("Watercontactsite", "date")]))

cs_data <- merge(cs_data, original_cs[, c("Watercontactsite",'date', "initial_time", "final_time")],
                 by = c("Watercontactsite",'date'), 
                 all.x = TRUE)

summary(cs_data)

# dif in hours translated to minutes 
cs_data$time_dur = as.numeric((as.difftime(cs_data$final_time) - as.difftime(cs_data$initial_time))*60)

# Cumulative sampling frequency

cs_data <- cs_data %>%
  group_by(ID) %>%
  mutate(cumulative_sampling_freq = row_number()) %>%
  ungroup()

# Consistency - # how many reports per month divided in the total number expected per month 

df1 <- cs_data %>%
  group_by(ID, month=(format(ymd(date), "%Y-%m"))) %>%
  summarize(reports_per_month = n()) 

#format(dmy(df$EndDate), "%Y-%m")

ID_reports = read.csv('~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/D_ID_reports.csv', sep = ',', header = T)

df2 <- merge(df1, ID_reports, by = "ID", all.x = TRUE)

df2$cons = df2$reports_per_month / (df2$exp_rep / 12)

cs_data$month = format(ymd(cs_data$date), "%Y-%m")

df3 <- merge(cs_data, df2[, c("ID",'month', "cons")] , by = c("ID",'month'), all.x = TRUE)

cs_data = df3

# Sampling time 

library(scales)

#df3$final_time = hm(sub("^(\\d{2}:\\d{2}).*", "\\1", df3$final_time))

#cs_data$initial_time <- as.POSIXct(strptime(cs_data$initial_time, "%H:%M",  tz = "UTC"))

# Aggregate ex_data to get the first Site.type for each Watercontactsite
ex_data_unique <- ex_data %>%
  group_by(Watercontactsite) %>%
  summarize(Site.type = first(Site.type))

cs_data <- merge(cs_data, ex_data_unique, by = "Watercontactsite", all.x = TRUE)

cs_data$final_timeX <- as.POSIXct(strptime(cs_data$final_time, "%H:%M",  tz = "UTC"))
head(cs_data$final_timeX)

library(hms)

cs_data$time <- hms::hms(second(cs_data$final_timeX), minute(cs_data$final_timeX), hour(cs_data$final_timeX))  

cs_data$time <- as.POSIXct(cs_data$time)

# time in seconds for comparison e.g. 16:21 is 58860 
cs_data$time = as.numeric(cs_data$time)

summary(cs_data)

cs_data$bio_pres <- ifelse(cs_data$bio > 0, 1, 0)
ex_data$bio_pres <- ifelse(ex_data$bio > 0, 1, 0)

cs_data <- cs_data %>% dplyr::select(-month, -NDVI_med_t,-initial_time,-final_time,-final_timeX)

# Convert the time variable 
# Sinusoidal transformation for time in minutes (0–1440)
cs_data$time_sin <- sin(2 * pi * cs_data$time / 86400) 
cs_data$time_cos = cos(2 * pi * cs_data$time / 86400)

# Sampling density 

# Summarize the expert data
ex_wcs_samp = ex_data %>%
  group_by(Watercontactsite) %>%
  summarise(ex_wcs_samp = n(), .groups = "drop")

# Summarize the citizen science data
cs_wcs_samp = cs_data %>%
  group_by(Watercontactsite) %>%
  summarise(cs_wcs_samp = n(), .groups = "drop")

# Merge the two summaries
wcs_samp = full_join(ex_wcs_samp, cs_wcs_samp, by = "Watercontactsite")

cs_data = left_join(cs_data,wcs_samp, by = 'Watercontactsite')

cs_data$samp_den = cs_data$cs_wcs_samp/cs_data$ex_wcs_samp

write.csv(cs_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_cs_complete.csv", row.names = FALSE)
write.csv(ex_data, "~/Library/CloudStorage/OneDrive-KULeuven/Uganda_Congo/Data/Uganda/Clean/D_ex_complete.csv", row.names = FALSE)




