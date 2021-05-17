Nigeria Shops Analysis
================

## Load data

``` r
data <- read_csv("C:/Users/angie/OneDrive - McGill University/Pai Team/COVET/Nigeria program data/SHOPS Plus Progam Data 2018-2021.csv", guess_max = 84070)
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   monthyear = col_character(),
    ##   state = col_character(),
    ##   lga = col_character(),
    ##   fac_name = col_character(),
    ##   fac_type = col_character(),
    ##   fac_type_sub = col_character(),
    ##   start = col_character(),
    ##   end = col_character(),
    ##   today = col_character(),
    ##   username = col_character(),
    ##   selected_month = col_character(),
    ##   close_date1 = col_character(),
    ##   open_date1 = col_character(),
    ##   close_date2 = col_character(),
    ##   open_date2 = col_character(),
    ##   close_date3 = col_character(),
    ##   open_date3 = col_character(),
    ##   close_date4 = col_character(),
    ##   open_date4 = col_character(),
    ##   close_date5 = col_character()
    ##   # ... with 38 more columns
    ## )

    ## See spec(...) for full column specifications.

## Transform fields, filter to just include 2018-2020 data

``` r
data$monthyear <- mdy(data$monthyear)
data <- data %>% 
  filter(monthyear <= "2020-12-31") 
data$fac_type_sub[data$fac_type == "Hospital"] <- "Hospital"
data$fac_type[data$fac_type_sub == "Hospital"] <- "Clinical Faclity"
data$fac_type[data$fac_type == "Clinical Faclity"] <- "Clinical Facility"
```

## Which variables apply to each facility type?

``` r
# Step 1: Examine which facility types have data for each variable 
# (sum all numeric variables, everything with >0 counted as TRUE, ignoring NAs)
variables <- data %>% 
  select(c(1:grep("^ontreat_ds_total", colnames(data)))) %>% 
  group_by(fac_type) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  select(-c(fac_id, month, year))

variables <- data.frame(t(variables[,-1]))
colnames(variables) <- c("CF", "CP", "L", "P")

variables <- variables %>% 
  mutate(CF_any = ifelse(CF > 0, TRUE, FALSE), CP_any = ifelse(CP > 0, TRUE, FALSE),
         L_any = ifelse(L > 0, TRUE, FALSE), P_any = ifelse(P > 0, TRUE, FALSE))

variables$var_name <- rownames(variables)
rownames(variables) <- NULL
variables <- variables[,c(9, 1:8)]
write_csv(variables, path="./outputs/data/variables.csv")

# Step 2: Cross-check with information shared by partners in Nigeria 
# (formatting done in Excel)

codebook <- read_csv("TB Nigeria_Program Data_Codebook.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   `Variable Name` = col_character(),
    ##   `Variable Label` = col_character(),
    ##   `Answer Label` = col_character(),
    ##   `Answer Code` = col_double(),
    ##   `Variable Type` = col_character(),
    ##   Notes = col_character(),
    ##   `Clinical facilities` = col_logical(),
    ##   CPs = col_logical(),
    ##   Labs = col_logical(),
    ##   PPMVs = col_logical()
    ## )

``` r
colnames(codebook)[1] <- "var_name"
colnames(codebook)[7:10] <- c("CF_codebook", "CP_codebook", 
                              "L_codebook", "P_codebook")

crosscheck <- left_join(variables, codebook, by = "var_name")

mismatches <- crosscheck %>% 
  select(var_name, CF_any, CF_codebook, 
         CP_any, CP_codebook, 
         L_any, L_codebook, 
         P_any, P_codebook) %>% 
  filter(CF_any != CF_codebook | 
           CP_any != CP_codebook | 
           L_any != L_codebook | 
           P_any != P_codebook) %>% 
  pivot_longer(cols = c(2:9), names_to = c("fac_type", "source"), 
               names_sep = "_", values_to = "test") %>% 
  pivot_wider(names_from = "source", values_from = "test") %>% 
  rename(actual_data = any) %>% 
  filter(actual_data != codebook) %>% 
  arrange(actual_data)

# actual_data = FALSE & codebook = TRUE means there isn't any data where there should be data
# actual_data = TRUE & codebook = FALSE means there is data where there shouldn't be any

# Decision: 
# if a variable should have data for a given facility type, classify it as such. 
# If a variable has data for a specific facility type when it shouldn't, 
# still classify that variable as valid for that facility type

to_be_labeled_true <- mismatches$var_name[mismatches$actual_data==TRUE]

codebook_new <- crosscheck %>% 
  transmute(var_name = var_name,
            `Clinical Facility` = CF_codebook, 
            `Community Pharmacy` = CP_codebook, 
            `Lab` = L_codebook, 
            `PPMV` = P_codebook) 

codebook_new$`Clinical Facility`[codebook_new$var_name %in% to_be_labeled_true] <- TRUE
```

## Exploring gap in Kano data

``` r
check_feb_2020 <- data %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-02-01") 

check_feb_2020 <- check_feb_2020 %>% 
  mutate(reported_any_data_feb_2020 =
           rowSums(is.na(check_feb_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_feb_2020, .before = q_recd_cat1kit)  %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         reported_any_data_feb_2020)

check_mar_2020 <- data %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-03-01") 

check_mar_2020 <- check_mar_2020 %>% 
  mutate(reported_any_data_mar_2020 =
           rowSums(is.na(check_mar_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_mar_2020, .before = q_recd_cat1kit)  %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         reported_any_data_mar_2020)

check_apr_2020 <- data %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-04-01") 

check_apr_2020 <- check_apr_2020 %>% 
  mutate(reported_any_data_apr_2020 =
           rowSums(is.na(check_apr_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_apr_2020, .before = q_recd_cat1kit) %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         reported_any_data_apr_2020)

check_may_2020 <- data %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-05-01") 

check_may_2020 <- check_may_2020 %>% 
  mutate(reported_any_data_may_2020 =
           rowSums(is.na(check_may_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_may_2020, .before = q_recd_cat1kit) %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         reported_any_data_may_2020)

check_jun_2020 <- data %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         c(codebook_new$var_name)) %>%  
  filter(monthyear == "2020-06-01") 

check_jun_2020 <- check_jun_2020 %>% 
  mutate(reported_any_data_jun_2020 =
           rowSums(is.na(check_jun_2020[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
  relocate(reported_any_data_jun_2020, .before = q_recd_cat1kit) %>% 
  select(fac_id,
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         reported_any_data_jun_2020)

check_dec_2020 <- data %>% 
  filter(monthyear == "2020-12-01") %>% 
  select(fac_id, facility_closed_shutdown) 

data_check <- check_feb_2020 %>% 
  left_join(check_mar_2020, by = "fac_id") %>% 
  left_join(check_apr_2020, by = "fac_id") %>% 
  left_join(check_may_2020, by = "fac_id") %>% 
  left_join(check_jun_2020, by = "fac_id") %>% 
  left_join(check_dec_2020, by = "fac_id") %>% 
  select(fac_id, 
         monthyear.x, 
         state.x, 
         lga.x, 
         fac_name.x, 
         fac_type.x, 
         fac_type_sub.x, 
         reported_any_data_feb_2020, 
         reported_any_data_mar_2020, 
         reported_any_data_apr_2020, 
         reported_any_data_may_2020, 
         reported_any_data_jun_2020, 
         facility_closed_shutdown)

data_check %>% 
  group_by(state.x) %>% 
  summarize(sum(reported_any_data_feb_2020), 
            sum(reported_any_data_mar_2020), 
            sum(reported_any_data_apr_2020), 
            sum(reported_any_data_may_2020), 
            sum(reported_any_data_jun_2020))
```

    ## # A tibble: 2 x 6
    ##   state.x `sum(reported_an~ `sum(reported_an~ `sum(reported_an~ `sum(reported_a~
    ##   <chr>               <int>             <int>             <int>            <int>
    ## 1 Kano                  810               822               589              594
    ## 2 Lagos                1123              1122              1114             1112
    ## # ... with 1 more variable: sum(reported_any_data_jun_2020) <int>

## 9a. Sum Total of PTB Cases Bacteriologically Diagnosed and 9b. Sum Total of PTB Cases Clinically Diagnosed (Applies to Clinical facilities only)

``` r
diag_ptb <- data %>% 
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         diagbacter_ptb_total, 
         diagclinic_ptb_total) %>% 
  filter(fac_type == "Clinical Facility") %>% 
  pivot_longer(cols = starts_with("diag"), 
               names_to = "diag_type", 
               names_prefix = "diag", 
               values_to = "pts")

diag_ptb$diag_type <- factor(diag_ptb$diag_type, 
                             labels=c("Bacteriological", "Clinical"))

ggplot(diag_ptb, aes(x=monthyear, y=pts)) + 
  geom_col(aes(fill = diag_type)) + 
  facet_wrap(~state) + 
  labs(x="Month", 
       y="Number of Patients Diagnosed", 
       title="Pulmonary TB Cases Bacteriologically and Clinically Diagnosed per month by state", 
       caption = "Data collected among clinical facilities only", 
       fill = "Diagnosis Type") + 
  scale_x_date(date_breaks = "6 months", 
               date_labels = "%b %y", 
               limits=as.Date(c("2018-09-01", "2021-01-01"))) +
  theme(plot.caption = element_text(hjust=0), 
        strip.text = element_text(face = "bold"))
```

    ## Warning: Removed 14462 rows containing missing values (position_stack).

    ## Warning: Removed 412 rows containing missing values (geom_col).

![](outputs/img/README-diagptb-1.png)<!-- -->

## 10\. Grand Total of HIV Status of TB Cases

``` r
hiv <- data %>%
  select(fac_id, 
         monthyear, 
         state, 
         lga, 
         fac_name, 
         fac_type, 
         fac_type_sub, 
         cases_hivpos_total, 
         cases_hivneg_total, 
         cases_hivdk_total, 
         cases_hivstatus_total) %>% 
  filter(fac_type == "Lab") %>% 
  pivot_longer(cols = c(cases_hivpos_total, 
                        cases_hivneg_total, 
                        cases_hivdk_total), 
               names_to = "status", 
               names_prefix = "cases_hiv", 
               values_to = "pts")
```

## Telemedicine Analysis

``` r
#number of facilities using telemedicine
sum(data$telemedicine_service, na.rm=TRUE)
```

    ## [1] 186

``` r
#number of facilities using telemedicine for TB services
sum(data$telemedicine_for_tb_service, na.rm=TRUE)
```

    ## [1] 65

``` r
data_telem <- data %>% 
  filter(telemedicine_service == 1)

data_telem <- data_telem[,-c(10:593)]

data_telem$no_tb_presumptive_using_telem[is.na(data_telem$no_tb_presumptive_using_telem)] <- 0

data_telem$no_tb_presumptive_tested_telem[is.na(data_telem$no_tb_presumptive_tested_telem)] <- 0

data_telem$no_tb_diagnoscouns_using_telem[is.na(data_telem$no_tb_diagnoscouns_using_telem)] <- 0

data_telem$no_tb_trtmonitoring_using_telem[is.na(data_telem$no_tb_trtmonitoring_using_telem)] <- 0

data_telem <- data_telem %>% 
  mutate(telemedicine_for_tb_type = 
           ifelse(telemedicine_for_tb_service == 1 & 
                    telemedicine_for_tb_screening == 1 &
                    tb_diagnosedcounsel_using_telem != 1 &
                    tb_trtmonitoring_using_telem != 1, 
                  "Screening Only",
           ifelse(telemedicine_for_tb_service == 1 &
                           telemedicine_for_tb_screening != 1 &
                           tb_diagnosedcounsel_using_telem == 1 &
                           tb_trtmonitoring_using_telem != 1, 
                  "Diagnosis/Counseling Only", 
           ifelse(telemedicine_for_tb_service == 1 & 
                    telemedicine_for_tb_screening != 1 &
                    tb_diagnosedcounsel_using_telem != 1 &
                    tb_trtmonitoring_using_telem == 1, 
                  "Monitoring Only", 
           ifelse(telemedicine_for_tb_service == 1 & 
                    telemedicine_for_tb_screening == 1 &
                    tb_diagnosedcounsel_using_telem == 1 & 
                    tb_trtmonitoring_using_telem != 1, 
                  "Screening & Diagnosis", 
           ifelse(telemedicine_for_tb_service == 1 & 
                    telemedicine_for_tb_screening != 1 &
                    tb_diagnosedcounsel_using_telem == 1 &
                    tb_trtmonitoring_using_telem == 1, 
                  "Diagnosis & Monitoring", 
           ifelse(telemedicine_for_tb_service == 1 & 
                    telemedicine_for_tb_screening == 1 &
                    tb_diagnosedcounsel_using_telem != 1 &
                    tb_trtmonitoring_using_telem == 1, 
                  "Screening & Monitoring", 
           ifelse(telemedicine_for_tb_service == 1 & 
                    telemedicine_for_tb_screening == 1 &
                    tb_diagnosedcounsel_using_telem == 1 &
                    tb_trtmonitoring_using_telem == 1, 
                  "Screening, Diagnosis, & Monitoring", 
           ifelse(telemedicine_for_tb_service == 1 & 
                    telemedicine_for_tb_screening != 1 &
                    tb_diagnosedcounsel_using_telem != 1 &
                    tb_trtmonitoring_using_telem != 1, 
                  "Unspecified", 
                  NA)))))))))

data_telem$telemedicine_for_tb_type <- factor(data_telem$telemedicine_for_tb_type,
                                              levels = c("Screening Only",
                                                         "Diagnosis/Counseling Only", 
                                                         "Monitoring Only",
                                                         "Screening & Diagnosis",
                                                         "Diagnosis & Monitoring",
                                                         "Screening & Monitoring",
                                                         "Screening, Diagnosis, & Monitoring", 
                                                         "Unspecified", NA))


data_telem$no_patients = data_telem$no_tb_presumptive_tested_telem + 
  data_telem$no_tb_diagnoscouns_using_telem + 
  data_telem$no_tb_trtmonitoring_using_telem

data_telem$no_patients2 = data_telem$no_tb_presumptive_using_telem + 
  data_telem$no_tb_presumptive_tested_telem + 
  data_telem$no_tb_diagnoscouns_using_telem + 
  data_telem$no_tb_trtmonitoring_using_telem

telem_summary <- data_telem %>% 
  group_by(state) %>% 
  summarize("Facilities offering Telemedicine" = sum(telemedicine_service), 
            "Facilities offering Telemedicine for TB" = sum(telemedicine_for_tb_service), 
            "Facilities offering TB Screening via Telemedicine" = sum(telemedicine_for_tb_screening, na.rm=TRUE), 
            "Facilities offering TB Diagnosis/Counseling via Telemedicine" = sum(tb_diagnosedcounsel_using_telem, na.rm=TRUE), 
            "Facilities offering TB Treatment Monitoring via Telemedicine" = sum(tb_trtmonitoring_using_telem, na.rm=TRUE),
            "TB Patients using Telemedicine (any kind)" = sum(no_tb_presumptive_using_telem),
            "Patients Tested for TB using Telemedicine" = sum(no_tb_presumptive_tested_telem),
            "Patients Diagnosed/Counseled for TB using Telemedicine" = sum(no_tb_diagnoscouns_using_telem),
            "Patients Monitored for TB Treatment using Telemedicine" = sum(no_tb_trtmonitoring_using_telem))

telem_summary <- data.frame(t(telem_summary)[-1,])
names(telem_summary) = c("Kano", "Lagos")
telem_summary$Kano = as.numeric(telem_summary$Kano)
telem_summary$Lagos = as.numeric(telem_summary$Lagos)

telem_summary
```

    ##                                                              Kano Lagos
    ## Facilities offering Telemedicine                               37   149
    ## Facilities offering Telemedicine for TB                        10    55
    ## Facilities offering TB Screening via Telemedicine               6    26
    ## Facilities offering TB Diagnosis/Counseling via Telemedicine    4    29
    ## Facilities offering TB Treatment Monitoring via Telemedicine    4    42
    ## TB Patients using Telemedicine (any kind)                      29   151
    ## Patients Tested for TB using Telemedicine                       8    33
    ## Patients Diagnosed/Counseled for TB using Telemedicine          7    44
    ## Patients Monitored for TB Treatment using Telemedicine         15   108

``` r
ggplot(filter(data_telem, telemedicine_for_tb_service == 1),
       aes(x=telemedicine_for_tb_type, fill = telemedicine_for_tb_type))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size = 3)+
  ylim(c(0,20))+
  facet_wrap(~state)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "blank", 
        plot.margin = unit(c(0.5,0.5,1,1), "cm"))+
  labs(title = "Facilities offering each type of Telemedicine Services for TB", 
       y = "Count of Facilities", 
       x = NULL, 
       fill = "Telemedicine Services for TB")
```

![](outputs/img/README-telemedicine5-1.png)<!-- -->
