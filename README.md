Nigeria Shops Analysis
================

## Load data

``` r
data <- read_csv("C:/Users/angie/OneDrive - McGill University/Pai Team/COVET/Nigeria program data/SHOPS Plus Progam Data 2018-2021.csv", guess_max = 84070)

# Transform fields, filter to just include 2018-2020 data

data$monthyear <- mdy(data$monthyear)
data <- data %>% 
  filter(monthyear <= "2020-12-31") 
data$fac_type_sub[data$fac_type == "Hospital"] <- "Hospital"
data$fac_type[data$fac_type_sub == "Hospital"] <- "Clinical Faclity"
data$fac_type[data$fac_type == "Clinical Faclity"] <- "Clinical Facility"

length(unique(data$monthyear))
```

    ## [1] 32

There are 32 unique months of data in this dataset. We can check to make
sure no facility id’s have greater or fewer than 32 observations.

``` r
data %>% 
  group_by(fac_id) %>% 
  count() %>% 
  filter(n != 32)
```

    ## # A tibble: 1 x 2
    ## # Groups:   fac_id [1]
    ##   fac_id     n
    ##    <dbl> <int>
    ## 1   2102    64

``` r
head(data %>% filter(fac_id == 2102) %>% arrange(monthyear) %>% select(c(1:9)))
```

    ## # A tibble: 6 x 9
    ##   fac_id month  year monthyear  state lga    fac_name    fac_type fac_type_sub
    ##    <dbl> <dbl> <dbl> <date>     <chr> <chr>  <chr>       <chr>    <chr>       
    ## 1   2102     5  2018 2018-05-01 Lagos Mushin Real Vision PPMV     .           
    ## 2   2102     5  2018 2018-05-01 Lagos Mushin Real Vision Lab      .           
    ## 3   2102     6  2018 2018-06-01 Lagos Mushin Real Vision PPMV     .           
    ## 4   2102     6  2018 2018-06-01 Lagos Mushin Real Vision Lab      .           
    ## 5   2102     7  2018 2018-07-01 Lagos Mushin Real Vision PPMV     .           
    ## 6   2102     7  2018 2018-07-01 Lagos Mushin Real Vision Lab      .

``` r
data %>% filter(fac_id == 2103) %>% select(c(1:9))
```

    ## # A tibble: 0 x 9
    ## # ... with 9 variables: fac_id <dbl>, month <dbl>, year <dbl>,
    ## #   monthyear <date>, state <chr>, lga <chr>, fac_name <chr>, fac_type <chr>,
    ## #   fac_type_sub <chr>

The facility with a `fac_id` of 2102 has two data points in each month,
one classified as a PPMV and the other as a lab. There is no facility
with a `fac_id` of 2103.

Rather than remove the observations for this facility, we can relabel
the facility with ID = 2102 and type = Lab to be facility \#2103.

``` r
data$fac_id[data$fac_id==2102 & data$fac_type=="Lab"] <- 2103

data %>% 
  group_by(fac_id) %>% 
  count() %>% 
  filter(n != 32)
```

    ## # A tibble: 0 x 2
    ## # Groups:   fac_id [0]
    ## # ... with 2 variables: fac_id <dbl>, n <int>

There are no remaining facilities with more or less than 32
observations, so our issue is fixed.

## Which variables apply to each facility type?

*Step 1:* Examine which facility types contain data for each variable
(sum all numeric variables, everything with &gt;0 counted as TRUE,
ignoring NAs)

``` r
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
head(variables)
```

    ##            var_name      CF CP L P CF_any CP_any L_any P_any
    ## 1    q_recd_cat1kit    9127  0 0 0   TRUE  FALSE FALSE FALSE
    ## 2  phys_inv_cat1kit   26609  0 0 0   TRUE  FALSE FALSE FALSE
    ## 3        q_recd_inh   79843  0 0 0   TRUE  FALSE FALSE FALSE
    ## 4      phys_inv_inh 1381728  0 0 0   TRUE  FALSE FALSE FALSE
    ## 5   q_recd_rhz_paed    5671  0 0 0   TRUE  FALSE FALSE FALSE
    ## 6 phys_inv_rhz_paed   13933  0 0 0   TRUE  FALSE FALSE FALSE

``` r
write_csv(variables, file="./outputs/data/variables.csv")
```

Step 2: Cross-check with the codebook shared by our partners in Nigeria
(formatting done in Excel).

``` r
codebook <- read_csv("C:/Users/angie/OneDrive - McGill University/Pai Team/COVET/Nigeria program data/TB Nigeria_Program Data_Codebook.csv")
head(codebook)
```

    ## # A tibble: 6 x 10
    ##   `Variable Name` `Variable Label` `Answer Label` `Answer Code` `Variable Type`
    ##   <chr>           <chr>            <chr>                  <dbl> <chr>          
    ## 1 fac_id          fac_id           Open ended                NA Numeric        
    ## 2 month           month            Open ended                NA Numeric        
    ## 3 year            year             Open ended                NA Numeric        
    ## 4 monthyear       monthyear        Open ended                NA Numeric        
    ## 5 state           state            Open ended                NA String         
    ## 6 lga             lga              Open ended                NA String         
    ## # ... with 5 more variables: Notes <chr>, Clinical facilities <lgl>, CPs <lgl>,
    ## #   Labs <lgl>, PPMVs <lgl>

``` r
colnames(codebook)[1] <- "var_name"
colnames(codebook)[7:10] <- c("CF_codebook", "CP_codebook", "L_codebook", "P_codebook")

crosscheck <- left_join(variables, codebook, by = "var_name")

mismatches <- crosscheck %>% 
  select(var_name, CF_any, CF_codebook, CP_any, CP_codebook, 
         L_any, L_codebook, P_any, P_codebook) %>% 
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

head(mismatches)
```

    ## # A tibble: 6 x 4
    ##   var_name          fac_type actual_data codebook
    ##   <chr>             <chr>    <lgl>       <lgl>   
    ## 1 presumptives_dr_m L        FALSE       TRUE    
    ## 2 presumptives_dr_f L        FALSE       TRUE    
    ## 3 examined_xpert_f  CP       FALSE       TRUE    
    ## 4 examined_afb_m    CP       FALSE       TRUE    
    ## 5 examined_afb_m    P        FALSE       TRUE    
    ## 6 examined_afb_f    CP       FALSE       TRUE

When `actual_data` is FALSE and `codebook` is TRUE, that means there is
no data where there should be data.

When `actual_data` is TRUE and `codebook` is FALSE, this means there are
data where there should not be any.

*Decision:*

-   If a variable should have data for a given facility type, classify
    it as such.
    -   if `actual_data` = FALSE & `codebook` = TRUE, keep label as TRUE
-   If a variable has data for a specific facility type when it
    shouldn’t, classify that variable as valid for that facility type.
    -   if `actual_data` = TRUE & `codebook` = FALSE, re-label as TRUE

``` r
mismatches %>% 
  filter(actual_data==TRUE)
```

    ## # A tibble: 18 x 4
    ##    var_name                        fac_type actual_data codebook
    ##    <chr>                           <chr>    <lgl>       <lgl>   
    ##  1 lab_walkins                     CF       TRUE        FALSE   
    ##  2 ss_sent_xpert                   CF       TRUE        FALSE   
    ##  3 ref_trt_m15_plus                CF       TRUE        FALSE   
    ##  4 ref_trt_f15_plus                CF       TRUE        FALSE   
    ##  5 ref_trt_total                   CF       TRUE        FALSE   
    ##  6 cases_starttrt_pvthosp_m15_plus CF       TRUE        FALSE   
    ##  7 cases_starttrt_pvthosp_f15_plus CF       TRUE        FALSE   
    ##  8 cases_starttrt_pvthosp_total    CF       TRUE        FALSE   
    ##  9 cases_starttrt_pubhosp_m15_plus CF       TRUE        FALSE   
    ## 10 cases_starttrt_pubhosp_f15_plus CF       TRUE        FALSE   
    ## 11 cases_starttrt_pubhosp_total    CF       TRUE        FALSE   
    ## 12 cases_hivneg_m15_plus           CF       TRUE        FALSE   
    ## 13 cases_hivneg_f15_plus           CF       TRUE        FALSE   
    ## 14 cases_hivneg_total              CF       TRUE        FALSE   
    ## 15 cases_hivdk_m15_plus            CF       TRUE        FALSE   
    ## 16 cases_hivdk_f15_plus            CF       TRUE        FALSE   
    ## 17 cases_hivdk_total               CF       TRUE        FALSE   
    ## 18 cases_hivstatus_total           CF       TRUE        FALSE

The only facility type where `actual_data` = TRUE & `codebook` = FALSE
is clinical facilities, so those are the only ones that need to be
re-labeled as TRUE.

``` r
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

Using a custom function (`datacheck`), for each of the 32 months I will:

-   Select the facility identifiers (`fac_id`, `state`, `lga`,
    `fac_name`, `fac_type`, and `fac_type_sub`) and all the routine
    measures for each facility (`q_recd_cat1kit` to `ontreat_ds_total`)

-   Create a column called `reported_any_data` for each month and year,
    which is:

    -   marked FALSE if a facility reported blank values (marked `NA`)
        for **all** routine measures in that month, or
    -   marked TRUE if a facility reported on **at least 1** routine
        measure (i.e. the number of cells with NA is not equal to the
        number of routine variables).
    -   Zeros are counted as reported data.

-   Then, I will sum the number of facilities with any reported data for
    all 32 `reported_any_data` columns and observe the change in number
    of facilities with any reported data during the period when Nigeria
    was under lockdown (April/May 2020).

``` r
datacheck <- function(x) {
  check <- data %>% 
  select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, 
         c(codebook_new$var_name)) %>%  
  filter(monthyear == as.Date(x)) 
  check <- check %>% 
    mutate(monthcheck = rowSums(is.na(check[,c(codebook_new$var_name)])) != length(codebook_new$var_name)) %>% 
    select(fac_id, monthyear, state, lga, fac_name, fac_type, fac_type_sub, monthcheck) %>%
    mutate("reported_data_{x}" := monthcheck)
  return(check)
}

dates <- unique(data$monthyear)
check_list <- lapply(dates, datacheck)
names(check_list) = dates

data_check <- check_list[["2018-05-01"]][c(1,3:7,9)] %>% 
  left_join(check_list[["2018-06-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2018-07-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2018-08-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2018-09-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2018-10-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2018-11-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2018-12-01"]][c(1,9)], by = "fac_id") %>%
  left_join(check_list[["2019-01-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-02-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-03-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-04-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-05-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-06-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-07-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-08-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-09-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-10-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-11-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2019-12-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-01-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-02-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-03-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-04-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-05-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-06-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-07-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-08-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-09-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-10-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-11-01"]][c(1,9)], by = "fac_id") %>% 
  left_join(check_list[["2020-12-01"]][c(1,9)], by = "fac_id") 

str(data_check)
```

    ## tibble [2,402 x 38] (S3: tbl_df/tbl/data.frame)
    ##  $ fac_id                  : num [1:2402] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ state                   : chr [1:2402] "Kano" "Kano" "Kano" "Kano" ...
    ##  $ lga                     : chr [1:2402] "Bebeji" "Bebeji" "Bebeji" "Bebeji" ...
    ##  $ fac_name                : chr [1:2402] "Abu Muhammad Medicine Store" "Aifak  Chemist" "Alfijir Nursing Home" "Al-Yusrai (Taimako Clinic and Maternity)" ...
    ##  $ fac_type                : chr [1:2402] "PPMV" "PPMV" "Clinical Facility" "Clinical Facility" ...
    ##  $ fac_type_sub            : chr [1:2402] "." "." "." "." ...
    ##  $ reported_data_2018-05-01: logi [1:2402] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ reported_data_2018-06-01: logi [1:2402] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ reported_data_2018-07-01: logi [1:2402] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ reported_data_2018-08-01: logi [1:2402] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ reported_data_2018-09-01: logi [1:2402] FALSE FALSE FALSE FALSE FALSE FALSE ...
    ##  $ reported_data_2018-10-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2018-11-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2018-12-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-01-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-02-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-03-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-04-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-05-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-06-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-07-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-08-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-09-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-10-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-11-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2019-12-01: logi [1:2402] FALSE FALSE FALSE TRUE FALSE FALSE ...
    ##  $ reported_data_2020-01-01: logi [1:2402] FALSE FALSE TRUE TRUE TRUE FALSE ...
    ##  $ reported_data_2020-02-01: logi [1:2402] FALSE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ reported_data_2020-03-01: logi [1:2402] FALSE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ reported_data_2020-04-01: logi [1:2402] FALSE TRUE FALSE FALSE FALSE TRUE ...
    ##  $ reported_data_2020-05-01: logi [1:2402] FALSE TRUE FALSE FALSE FALSE TRUE ...
    ##  $ reported_data_2020-06-01: logi [1:2402] FALSE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ reported_data_2020-07-01: logi [1:2402] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ reported_data_2020-08-01: logi [1:2402] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ reported_data_2020-09-01: logi [1:2402] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##  $ reported_data_2020-10-01: logi [1:2402] TRUE FALSE TRUE TRUE TRUE TRUE ...
    ##  $ reported_data_2020-11-01: logi [1:2402] TRUE FALSE TRUE TRUE TRUE TRUE ...
    ##  $ reported_data_2020-12-01: logi [1:2402] TRUE FALSE TRUE TRUE TRUE TRUE ...

``` r
data_check_summary_table <- data_check %>% 
  group_by(state, fac_type) %>% 
  summarize_if(is.logical, sum) 

data_check_summary <- data_check_summary_table %>% 
  pivot_longer(cols = c(3:34), names_to = "date", names_pattern = "reported_data_(.*)", values_to = "num_reported")

data_check_summary$date <- as.Date(data_check_summary$date)
```

``` r
ggplot(data_check_summary, aes(x=as.Date(date), y=num_reported)) +
  geom_line(group=1, size = 1, color = "steelblue") +
  facet_wrap(~fac_type+state, ncol=2, 
             scales = "free", strip.position = "top", 
             labeller = label_wrap_gen(multi_line=FALSE)) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2018-01-01"), 
                                      to = as.Date("2021-01-01"), 
                                      by = "6 months"), 
               minor_breaks = "1 month", date_labels = "%b %y") +
  labs(title = "Facilities Reporting Over Time", 
       caption = "Recreation of Fig 8 @ https://github.com/bbdaniels/nigeria-shops-analysis/tree/main/outputs", 
       x = NULL, y = NULL) +
  theme(axis.ticks.x = element_line())
```

<img src="outputs/img/README-reporting-1.png" style="display: block; margin: auto;" />

Through this process, I ended up recreating the “Facilities Reporting
Over Time” graph that Ben previously created using the
`attendance_total` variable. **There does appear to be a gap in data
reported by Clinical Facilities and Community Pharmacies in Kano in
April and May 2020.**

Below I’ve displayed the same data in a table and added the number of
facilities in each state and facility type that reported being shutdown
at least once during the COVID lockdown period in Nigeria (April/May
2020).

``` r
check_dec_2020 <- data %>% 
  filter(monthyear == "2020-12-01") %>% 
  select(state, fac_type, facility_closed_shutdown) %>% 
  group_by(state, fac_type) %>% 
  summarize(facility_closed_shutdown = sum(facility_closed_shutdown, na.rm = TRUE))

data_check_summary_table <- data_check_summary_table %>% 
  left_join(check_dec_2020, by=c("state", "fac_type"))

knitr::kable(data_check_summary_table[,c(1,2,23:31,35)], 
             col.names = c("State", "Facility Type", 
                           "Number of Facilities that Reported Any Data in Jan 2020", 
                           "Feb 2020", "Mar 2020", "Apr 2020", 
                           "May 2020", "Jun 2020", "Jul 2020", 
                           "Aug 2020", "Sep 2020", 
                           "Number of facilities Reporting Having Shut Down during COVID"))
```

| State | Facility Type      | Number of Facilities that Reported Any Data in Jan 2020 | Feb 2020 | Mar 2020 | Apr 2020 | May 2020 | Jun 2020 | Jul 2020 | Aug 2020 | Sep 2020 | Number of facilities Reporting Having Shut Down during COVID |
|:------|:-------------------|--------------------------------------------------------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|---------:|-------------------------------------------------------------:|
| Kano  | Clinical Facility  |                                                     205 |      205 |      205 |        1 |        1 |      212 |      214 |      199 |      198 |                                                           25 |
| Kano  | Community Pharmacy |                                                      26 |       26 |       26 |        0 |        0 |       26 |       26 |       18 |       18 |                                                            4 |
| Kano  | Lab                |                                                      30 |       30 |       30 |       30 |       30 |       30 |       30 |       25 |       25 |                                                            2 |
| Kano  | PPMV               |                                                     461 |      549 |      561 |      558 |      563 |      551 |      563 |      615 |      634 |                                                           46 |
| Lagos | Clinical Facility  |                                                     433 |      433 |      433 |      433 |      432 |      433 |      433 |      439 |      439 |                                                           19 |
| Lagos | Community Pharmacy |                                                     138 |      138 |      137 |      133 |      133 |      133 |      133 |      133 |      133 |                                                           10 |
| Lagos | Lab                |                                                     118 |      118 |      118 |      118 |      117 |      118 |      116 |      117 |      117 |                                                           18 |
| Lagos | PPMV               |                                                     404 |      403 |      403 |      399 |      399 |      399 |      399 |      398 |      390 |                                                           55 |

# Additional Variable Analysis

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

<img src="outputs/img/README-diagptb-1.png" style="display: block; margin: auto;" />

## 10. Grand Total of HIV Status of TB Cases

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

<img src="outputs/img/README-telemedicine5-1.png" style="display: block; margin: auto;" />
