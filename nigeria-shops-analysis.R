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
data$fac_id <- as.character(data$fac_id)
data$fac_type_sub[data$fac_type == "Hospital"] <- "Hospital"
data$fac_type[data$fac_type_sub == "Hospital"] <- "Clinical Faclity"
data$fac_type[data$fac_type == "Clinical Faclity"] <- "Clinical Facility"

## Which variables apply to each facility type?

### Step 1: Examine which facility types have data for each variable (sum all numeric variables, everything with >0 counted as TRUE, ignoring NAs)
variables <- data %>% 
  select(c(1:grep("^ontreat_ds_total", colnames(data)))) %>% 
  group_by(fac_type) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-c(fac_id, month, year))

variables <- data.frame(t(variables[,-1]))
colnames(variables) <- c("CF", "CP", "L", "P")

variables <- variables %>% 
  mutate(CF_any = ifelse(CF > 0, TRUE, FALSE), CP_any = ifelse(CP > 0, TRUE, FALSE), L_any = ifelse(L > 0, TRUE, FALSE), P_any = ifelse(P > 0, TRUE, FALSE))

variables$var_name <- rownames(variables)
rownames(variables) <- NULL
variables <- variables[,c(9, 1:8)]
write_csv(variables, path="./outputs/data/variables.csv")

### Step 2: Cross-check with information shared by partners in Nigeria (formatting done in Excel)

codebook <- read_csv("TB Nigeria_Program Data_Codebook.csv")
colnames(codebook)[1] <- "var_name"
colnames(codebook)[7:10] <- c("CF_codebook", "CP_codebook", "L_codebook", "P_codebook")

crosscheck <- left_join(variables, codebook, by = "var_name")

mismatches <- crosscheck %>% 
  select(var_name, CF_any, CF_codebook, CP_any, CP_codebook, L_any, L_codebook, P_any, P_codebook) %>% 
  filter(CF_any != CF_codebook | CP_any != CP_codebook | L_any != L_codebook | P_any != P_codebook) %>% 
  pivot_longer(cols = c(2:9), names_to = c("fac_type", "source"), names_sep = "_", values_to = "test") %>% 
  pivot_wider(names_from = "source", values_from = "test") %>% 
  rename(actual_data = any) %>% 
  filter(actual_data != codebook) %>% 
  arrange(actual_data)

### actual_data = FALSE & codebook = TRUE means there isn't any data where there should be data
### actual_data = TRUE & codebook = FALSE means there is data where there shouldn't be any

### Decision: if a variable should have data for a given facility type, classify it as such. If a variable has data for a specific facility type when it shouldn't, still classify that variable as valid for that facility type

to_be_labeled_true <- mismatches$var_name[mismatches$actual_data==TRUE]

codebook_new <- crosscheck %>% 
  transmute(var_name = var_name,`Clinical Facility` = CF_codebook, `Community Pharmacy` = CP_codebook, `Lab` = L_codebook, `PPMV` = P_codebook) 

codebook_new$`Clinical Facility`[codebook_new$var_name %in% to_be_labeled_true] <- TRUE

## Exploring gap in Kano data

check_feb_2020 <- data %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-02-01") 

check_feb_2020 <- check_feb_2020 %>% 
  mutate(reported_any_data_feb_2020 = rowSums(is.na(check_feb_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_feb_2020, .before = q_recd_cat1kit)  %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, reported_any_data_feb_2020)

check_mar_2020 <- data %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-03-01") 

check_mar_2020 <- check_mar_2020 %>% 
  mutate(reported_any_data_mar_2020 = rowSums(is.na(check_mar_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_mar_2020, .before = q_recd_cat1kit)  %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, reported_any_data_mar_2020)

check_apr_2020 <- data %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-04-01") 

check_apr_2020 <- check_apr_2020 %>% 
  mutate(reported_any_data_apr_2020 = rowSums(is.na(check_apr_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_apr_2020, .before = q_recd_cat1kit) %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, reported_any_data_apr_2020)

check_may_2020 <- data %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-05-01") 

check_may_2020 <- check_may_2020 %>% 
  mutate(reported_any_data_may_2020 = rowSums(is.na(check_may_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_may_2020, .before = q_recd_cat1kit) %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, reported_any_data_may_2020)

check_jun_2020 <- data %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-06-01") 

check_jun_2020 <- check_jun_2020 %>% 
  mutate(reported_any_data_jun_2020 = rowSums(is.na(check_jun_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_jun_2020, .before = q_recd_cat1kit) %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, reported_any_data_jun_2020)

check_dec_2020 <- data %>% 
  filter(monthyear == "2020-12-01") %>% 
  select(fac_id, facility_closed_shutdown) 

data_check <- check_feb_2020 %>% 
  left_join(check_mar_2020, by = "fac_id") %>% 
  left_join(check_apr_2020, by = "fac_id") %>% 
  left_join(check_may_2020, by = "fac_id") %>% 
  left_join(check_jun_2020, by = "fac_id") %>% 
  left_join(check_dec_2020, by = "fac_id") %>% 
  select(fac_id, monthyear.x, state.x, lga.x, fac_name.x, fac_type.x, fac_type_sub.x, 
         reported_any_data_feb_2020, reported_any_data_mar_2020, reported_any_data_apr_2020, 
         reported_any_data_may_2020, reported_any_data_jun_2020, facility_closed_shutdown)

data_check %>% 
  group_by(state.x) %>% 
  summarize(sum(reported_any_data_feb_2020), sum(reported_any_data_mar_2020), sum(reported_any_data_apr_2020), sum(reported_any_data_may_2020), sum(reported_any_data_jun_2020))

summary(as.factor(diag_ptb_kano_dec2020$facility_closed_shutdown))

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





## 10. Grand Total of HIV Status of TB Cases

hiv <- data %>%
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, cases_hivpos_total, cases_hivneg_total, cases_hivdk_total, cases_hivstatus_total) %>% 
  filter(fac_type == "Lab") %>% 
  pivot_longer(cols = c(cases_hivpos_total, cases_hivneg_total, cases_hivdk_total), names_to = "status", names_prefix = "cases_hiv", values_to = "pts")


## Anomalous variables

# Subset dataset to include only `presumptives` variables
variables[c(25:31),c(1:5)]

# presumptives_total for Community Pharmacies equals m0-14 + f0-14 + m15_plus + f15_plus. It **does not** include presumptives_m or presumptives_f
sum(variables[c(25:28),3]) == variables[variables$var_name == "presumptives_total",3]

# presumptives_total for PPMVs doesn't make any sense at all
sum(variables[c(25:28),5]) == variables[variables$var_name == "presumptives_total",5]
sum(variables[c(25:30),5]) == variables[variables$var_name == "presumptives_total",5]
