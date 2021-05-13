## Load packages

library(tidyverse)
library(lubridate)
ggplot2::theme_set(theme_minimal())

## Load data

data <- read_csv("C:/Users/angie/OneDrive - McGill University/Pai Team/COVET/Nigeria program data/SHOPS Plus Progam Data 2018-2021.csv", guess_max = 84070)

## Transform fields, filter to just include 2018-2020 data

data$monthyear <- mdy(data$monthyear)
data <- data %>% 
  filter(monthyear <= "2020-12-31") 

data$fac_type_sub[data$fac_type == "Hospital"] <- "Hospital"
data$fac_type[data$fac_type_sub == "Hospital"] <- "Clinical Faclity"
data$fac_type_sub[data$fac_type_sub == "."] <- NA
data$fac_type[data$fac_type == "Clinical Faclity"] <- "Clinical Facility"


## 9a. Sum Total of PTB Cases Bacteriologically Diagnosed and 9b. Sum Total of PTB Cases Clinically Diagnosed
### Applies to Clinical facilities only

diag_ptb <- data %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, diagbacter_ptb_total, diagclinic_ptb_total) %>% 
  filter(fac_type == "Clinical Facility") %>% 
  pivot_longer(cols = starts_with("diag"), names_to = "diag_type", names_prefix = "diag", values_to = "pts")

diag_ptb$diag_type <- factor(diag_ptb$diag_type, labels=c("Bacteriological", "Clinical"))

ggplot(diag_ptb, aes(x=monthyear, y=pts)) + 
  geom_col(aes(fill = diag_type)) + 
  facet_wrap(~state) + 
  labs(x="Month", y="Number of Patients Diagnosed", title="Pulmonary TB Cases Bacteriologically and Clinically Diagnosed per month by state", caption = "Data collected among clinical facilities only", fill = "Diagnosis Type") + 
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y", limits=as.Date(c("2018-09-01", "2021-01-01"))) +
  theme(plot.caption = element_text(hjust=0), strip.text = element_text(face = "bold"))
ggsave(path = "~/GitHub/nigeria-shops-analysis/outputs/img", filename = "diagptb.png", width = 8, height = 5, units = "in")

