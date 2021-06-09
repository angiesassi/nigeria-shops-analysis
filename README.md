Nigeria Shops Analysis
================

## Load data

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

    ## # A tibble: 0 x 2
    ## # Groups:   fac_id [0]
    ## # ... with 2 variables: fac_id <dbl>, n <int>

<table>
<caption>
Table 1. Summary of Facilities in this Dataset by State and Type
</caption>
<thead>
<tr>
<th style="text-align:left;">
State
</th>
<th style="text-align:left;">
Facility Type
</th>
<th style="text-align:right;">
Total Number of Facilities (N)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
217
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
35
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
804
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
472
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
195
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
140
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
511
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
1084
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
1318
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
2402
</td>
</tr>
</tbody>
</table>

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

*Step 2:* Cross-check with the codebook shared by our partners in
Nigeria (formatting done in Excel).

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

<!-- -->

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

## Exploring Gap in Kano Data

Using a custom function (`datacheck`), for each of the 32 months in this
dataset I will:

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

data_check_summary_table <- data_check %>% 
  group_by(state, fac_type) %>% 
  summarize_if(is.logical, sum) 

data_check_summary <- data_check_summary_table %>% 
  pivot_longer(cols = c(3:34), names_to = "date", names_pattern = "reported_data_(.*)", values_to = "num_reported")

data_check_summary$date <- as.Date(data_check_summary$date)
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

<table>
<caption>
Table 2. Number of Facilities Reporting Over Time by State and Type
</caption>
<thead>
<tr>
<th style="text-align:left;">
State
</th>
<th style="text-align:left;">
Facility Type
</th>
<th style="text-align:right;">
Number of Facilities that Reported Any Data in Jan 2020
</th>
<th style="text-align:right;">
Feb 2020
</th>
<th style="text-align:right;">
Mar 2020
</th>
<th style="text-align:right;">
Apr 2020
</th>
<th style="text-align:right;">
May 2020
</th>
<th style="text-align:right;">
Jun 2020
</th>
<th style="text-align:right;">
Jul 2020
</th>
<th style="text-align:right;">
Aug 2020
</th>
<th style="text-align:right;">
Sep 2020
</th>
<th style="text-align:right;">
Number of facilities Reporting Having Shut Down during COVID
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
205
</td>
<td style="text-align:right;">
205
</td>
<td style="text-align:right;">
205
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
212
</td>
<td style="text-align:right;">
214
</td>
<td style="text-align:right;">
199
</td>
<td style="text-align:right;">
198
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
461
</td>
<td style="text-align:right;">
549
</td>
<td style="text-align:right;">
561
</td>
<td style="text-align:right;">
558
</td>
<td style="text-align:right;">
563
</td>
<td style="text-align:right;">
551
</td>
<td style="text-align:right;">
563
</td>
<td style="text-align:right;">
615
</td>
<td style="text-align:right;">
634
</td>
<td style="text-align:right;">
46
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
433
</td>
<td style="text-align:right;">
433
</td>
<td style="text-align:right;">
433
</td>
<td style="text-align:right;">
433
</td>
<td style="text-align:right;">
432
</td>
<td style="text-align:right;">
433
</td>
<td style="text-align:right;">
433
</td>
<td style="text-align:right;">
439
</td>
<td style="text-align:right;">
439
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:right;">
138
</td>
<td style="text-align:right;">
137
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
133
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
118
</td>
<td style="text-align:right;">
118
</td>
<td style="text-align:right;">
118
</td>
<td style="text-align:right;">
118
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
118
</td>
<td style="text-align:right;">
116
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
117
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
404
</td>
<td style="text-align:right;">
403
</td>
<td style="text-align:right;">
403
</td>
<td style="text-align:right;">
399
</td>
<td style="text-align:right;">
399
</td>
<td style="text-align:right;">
399
</td>
<td style="text-align:right;">
399
</td>
<td style="text-align:right;">
398
</td>
<td style="text-align:right;">
390
</td>
<td style="text-align:right;">
55
</td>
</tr>
</tbody>
</table>

# Additional Variable Analysis

## 9a. Sum Total of PTB Cases Bacteriologically Diagnosed and 9b. Sum Total of PTB Cases Clinically Diagnosed (Applies to Clinical facilities only)

<img src="outputs/img/README-diagptb-1.png" style="display: block; margin: auto;" />

## 10. Grand Total of HIV Status of TB Cases

<img src="outputs/img/README-hiv-1.png" style="display: block; margin: auto;" />

## 16. Grand Total of Cases Notified (Clinical Facilities and Labs Only)

<img src="outputs/img/README-notif-1.png" style="display: block; margin: auto;" /><img src="outputs/img/README-notif-2.png" style="display: block; margin: auto;" />

# Results from the December 2020 COVID Survey

## General survey information

Information on 1598 facilities out of 2402 total facilities was included
in the responses to the COVID questionnaire.

<table>
<caption>
Table 3. Summary of Response to Dec 2020 COVID Survey
</caption>
<thead>
<tr>
<th style="text-align:left;">
State
</th>
<th style="text-align:left;">
Facility Type
</th>
<th style="text-align:right;">
Number of Facilities Responding to COVID Survey
</th>
<th style="text-align:right;">
Total Number of Facilities
</th>
<th style="text-align:right;">
Percent Response (%)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
151
</td>
<td style="text-align:right;">
217
</td>
<td style="text-align:right;">
69.59
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:right;">
60.71
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:right;">
57.14
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
550
</td>
<td style="text-align:right;">
804
</td>
<td style="text-align:right;">
68.41
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
395
</td>
<td style="text-align:right;">
472
</td>
<td style="text-align:right;">
83.69
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
195
</td>
<td style="text-align:right;">
46.67
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
140
</td>
<td style="text-align:right;">
77.14
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
266
</td>
<td style="text-align:right;">
511
</td>
<td style="text-align:right;">
52.05
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
738
</td>
<td style="text-align:right;">
1084
</td>
<td style="text-align:right;">
68.08
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
860
</td>
<td style="text-align:right;">
1318
</td>
<td style="text-align:right;">
65.25
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
1598
</td>
<td style="text-align:right;">
2402
</td>
<td style="text-align:right;">
66.53
</td>
</tr>
</tbody>
</table>

## Current Status of Facilities

<table>
<caption>
Table 4. Percent of Facilities Open and Accepting Clients at time of
Survey (Dec 2020)
</caption>
<thead>
<tr>
<th style="text-align:left;">
State
</th>
<th style="text-align:left;">
Facility Type
</th>
<th style="text-align:right;">
Number of Open Facilities
</th>
<th style="text-align:right;">
Total N of Responding Facilities
</th>
<th style="text-align:right;">
Percent Currently Open (%)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
148
</td>
<td style="text-align:right;">
151
</td>
<td style="text-align:right;">
98.01
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
100.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
85.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
502
</td>
<td style="text-align:right;">
550
</td>
<td style="text-align:right;">
91.27
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
389
</td>
<td style="text-align:right;">
395
</td>
<td style="text-align:right;">
98.48
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
88
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
96.70
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
100.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
248
</td>
<td style="text-align:right;">
266
</td>
<td style="text-align:right;">
93.23
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
684
</td>
<td style="text-align:right;">
738
</td>
<td style="text-align:right;">
92.68
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
833
</td>
<td style="text-align:right;">
860
</td>
<td style="text-align:right;">
96.86
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
1517
</td>
<td style="text-align:right;">
1598
</td>
<td style="text-align:right;">
94.93
</td>
</tr>
</tbody>
</table>

## Previous Status during Lockdown

<table>
<caption>
Table 5. Summary of Facilities Reporting Having Closed During the COVID
Lockdown (April-May 2020
</caption>
<thead>
<tr>
<th style="text-align:left;">
State
</th>
<th style="text-align:left;">
Facility Type
</th>
<th style="text-align:right;">
Number of Facilities that Closed at least once during COVID lockdown
</th>
<th style="text-align:right;">
Total N of Responding Facilities
</th>
<th style="text-align:right;">
Percent Closed at least once during COVID lockdown (%)
</th>
<th style="text-align:right;">
Mean Number of Times Each Facility Closed
</th>
<th style="text-align:right;">
Std. Dev. of Number of Times Closed
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
151
</td>
<td style="text-align:right;">
16.56
</td>
<td style="text-align:right;">
1.12
</td>
<td style="text-align:right;">
0.33
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
23.53
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
550
</td>
<td style="text-align:right;">
8.36
</td>
<td style="text-align:right;">
1.28
</td>
<td style="text-align:right;">
1.34
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
395
</td>
<td style="text-align:right;">
4.81
</td>
<td style="text-align:right;">
1.21
</td>
<td style="text-align:right;">
0.42
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
10.99
</td>
<td style="text-align:right;">
1.50
</td>
<td style="text-align:right;">
0.97
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
16.67
</td>
<td style="text-align:right;">
1.11
</td>
<td style="text-align:right;">
0.32
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
55
</td>
<td style="text-align:right;">
266
</td>
<td style="text-align:right;">
20.68
</td>
<td style="text-align:right;">
1.29
</td>
<td style="text-align:right;">
0.79
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
738
</td>
<td style="text-align:right;">
10.43
</td>
<td style="text-align:right;">
1.21
</td>
<td style="text-align:right;">
1.06
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
102
</td>
<td style="text-align:right;">
860
</td>
<td style="text-align:right;">
11.86
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
0.69
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
179
</td>
<td style="text-align:right;">
1598
</td>
<td style="text-align:right;">
11.20
</td>
<td style="text-align:right;">
1.24
</td>
<td style="text-align:right;">
1.06
</td>
</tr>
</tbody>
</table>

## Has facility added incremental fees to cover PPE cost?

<table>
<caption>
Table 6. Percent of Facilities That Increased Fees due to PPE Cost
</caption>
<thead>
<tr>
<th style="text-align:left;">
State
</th>
<th style="text-align:left;">
Facility Type
</th>
<th style="text-align:right;">
Number of Facilities That Added PPE Fee
</th>
<th style="text-align:right;">
Total N of Responding Facilities
</th>
<th style="text-align:right;">
Percent of Facilities That Increased Fees due to PPE Cost (%)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
151
</td>
<td style="text-align:right;">
11.26
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
17.65
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
0.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:right;">
550
</td>
<td style="text-align:right;">
4.91
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Clinical Facility
</td>
<td style="text-align:right;">
77
</td>
<td style="text-align:right;">
395
</td>
<td style="text-align:right;">
19.49
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Community Pharmacy
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:right;">
91
</td>
<td style="text-align:right;">
39.56
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Lab
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
108
</td>
<td style="text-align:right;">
13.89
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
PPMV
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
266
</td>
<td style="text-align:right;">
21.43
</td>
</tr>
<tr>
<td style="text-align:left;">
Kano
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
738
</td>
<td style="text-align:right;">
6.37
</td>
</tr>
<tr>
<td style="text-align:left;">
Lagos
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
185
</td>
<td style="text-align:right;">
860
</td>
<td style="text-align:right;">
21.51
</td>
</tr>
<tr>
<td style="text-align:left;">
Total
</td>
<td style="text-align:left;">
Total
</td>
<td style="text-align:right;">
232
</td>
<td style="text-align:right;">
1598
</td>
<td style="text-align:right;">
14.52
</td>
</tr>
</tbody>
</table>
<div id="ppprseyrlp" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ppprseyrlp .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ppprseyrlp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ppprseyrlp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ppprseyrlp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ppprseyrlp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ppprseyrlp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ppprseyrlp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ppprseyrlp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ppprseyrlp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ppprseyrlp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ppprseyrlp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ppprseyrlp .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ppprseyrlp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ppprseyrlp .gt_from_md > :first-child {
  margin-top: 0;
}

#ppprseyrlp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ppprseyrlp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ppprseyrlp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ppprseyrlp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ppprseyrlp .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ppprseyrlp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ppprseyrlp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ppprseyrlp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ppprseyrlp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ppprseyrlp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ppprseyrlp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ppprseyrlp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ppprseyrlp .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ppprseyrlp .gt_left {
  text-align: left;
}

#ppprseyrlp .gt_center {
  text-align: center;
}

#ppprseyrlp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ppprseyrlp .gt_font_normal {
  font-weight: normal;
}

#ppprseyrlp .gt_font_bold {
  font-weight: bold;
}

#ppprseyrlp .gt_font_italic {
  font-style: italic;
}

#ppprseyrlp .gt_super {
  font-size: 65%;
}

#ppprseyrlp .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Kano</strong>, N = 738<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Lagos</strong>, N = 860<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">facility_incrmnt_fees_due_ppe</td>
<td class="gt_row gt_center">47 (6.4%)</td>
<td class="gt_row gt_center">185 (22%)</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          n (%)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Pearson's Chi-squared test
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
<div id="koqhmptgdj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#koqhmptgdj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#koqhmptgdj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#koqhmptgdj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#koqhmptgdj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#koqhmptgdj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#koqhmptgdj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#koqhmptgdj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#koqhmptgdj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#koqhmptgdj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#koqhmptgdj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#koqhmptgdj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#koqhmptgdj .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#koqhmptgdj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#koqhmptgdj .gt_from_md > :first-child {
  margin-top: 0;
}

#koqhmptgdj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#koqhmptgdj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#koqhmptgdj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#koqhmptgdj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#koqhmptgdj .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#koqhmptgdj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#koqhmptgdj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#koqhmptgdj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#koqhmptgdj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#koqhmptgdj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#koqhmptgdj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#koqhmptgdj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#koqhmptgdj .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#koqhmptgdj .gt_left {
  text-align: left;
}

#koqhmptgdj .gt_center {
  text-align: center;
}

#koqhmptgdj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#koqhmptgdj .gt_font_normal {
  font-weight: normal;
}

#koqhmptgdj .gt_font_bold {
  font-weight: bold;
}

#koqhmptgdj .gt_font_italic {
  font-style: italic;
}

#koqhmptgdj .gt_super {
  font-size: 65%;
}

#koqhmptgdj .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="5">
        <span class="gt_column_spanner"><strong>Kano State</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="5">
        <span class="gt_column_spanner"><strong>Lagos State</strong></span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Clinical Facility</strong>, N = 151<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Community Pharmacy</strong>, N = 17<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Lab</strong>, N = 20<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>PPMV</strong>, N = 550<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Clinical Facility</strong>, N = 395<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Community Pharmacy</strong>, N = 91<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Lab</strong>, N = 108<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>PPMV</strong>, N = 266<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">3</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">facility_incrmnt_fees_due_ppe</td>
<td class="gt_row gt_center">17 (11%)</td>
<td class="gt_row gt_center">3 (18%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">27 (4.9%)</td>
<td class="gt_row gt_center">0.006</td>
<td class="gt_row gt_center">77 (19%)</td>
<td class="gt_row gt_center">36 (40%)</td>
<td class="gt_row gt_center">15 (14%)</td>
<td class="gt_row gt_center">57 (21%)</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="11">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          n (%)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Fisher's exact test
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>3</em>
          </sup>
           
          Pearson's Chi-squared test
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
<div id="hxqxiytrmt" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hxqxiytrmt .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hxqxiytrmt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hxqxiytrmt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hxqxiytrmt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hxqxiytrmt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hxqxiytrmt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hxqxiytrmt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hxqxiytrmt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hxqxiytrmt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hxqxiytrmt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hxqxiytrmt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hxqxiytrmt .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#hxqxiytrmt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hxqxiytrmt .gt_from_md > :first-child {
  margin-top: 0;
}

#hxqxiytrmt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hxqxiytrmt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hxqxiytrmt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#hxqxiytrmt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hxqxiytrmt .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#hxqxiytrmt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hxqxiytrmt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hxqxiytrmt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hxqxiytrmt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hxqxiytrmt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hxqxiytrmt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#hxqxiytrmt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hxqxiytrmt .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#hxqxiytrmt .gt_left {
  text-align: left;
}

#hxqxiytrmt .gt_center {
  text-align: center;
}

#hxqxiytrmt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hxqxiytrmt .gt_font_normal {
  font-weight: normal;
}

#hxqxiytrmt .gt_font_bold {
  font-weight: bold;
}

#hxqxiytrmt .gt_font_italic {
  font-style: italic;
}

#hxqxiytrmt .gt_super {
  font-size: 65%;
}

#hxqxiytrmt .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Kano</strong>, N = 47<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Lagos</strong>, N = 185<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ppe_cost_bearingadd_separate_pp</td>
<td class="gt_row gt_center">14 (30%)</td>
<td class="gt_row gt_center">39 (21%)</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">ppe_cost_bearingincrease_in_con</td>
<td class="gt_row gt_center">8 (17%)</td>
<td class="gt_row gt_center">17 (9.2%)</td>
<td class="gt_row gt_center">0.12</td></tr>
    <tr><td class="gt_row gt_left">ppe_cost_bearingincrease_in_reg</td>
<td class="gt_row gt_center">5 (11%)</td>
<td class="gt_row gt_center">15 (8.1%)</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left">ppe_cost_bearingincrease_in_lab</td>
<td class="gt_row gt_center">26 (55%)</td>
<td class="gt_row gt_center">135 (73%)</td>
<td class="gt_row gt_center">0.019</td></tr>
    <tr><td class="gt_row gt_left">ppe_cost_bearingincrease_mentio</td>
<td class="gt_row gt_center">5 (11%)</td>
<td class="gt_row gt_center">22 (12%)</td>
<td class="gt_row gt_center">0.8</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="4">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          n (%)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Pearson's Chi-squared test; Fisher's exact test
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
<div id="hetuguaaqw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hetuguaaqw .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hetuguaaqw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hetuguaaqw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hetuguaaqw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hetuguaaqw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hetuguaaqw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hetuguaaqw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hetuguaaqw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hetuguaaqw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hetuguaaqw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hetuguaaqw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hetuguaaqw .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#hetuguaaqw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hetuguaaqw .gt_from_md > :first-child {
  margin-top: 0;
}

#hetuguaaqw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hetuguaaqw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hetuguaaqw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#hetuguaaqw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hetuguaaqw .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#hetuguaaqw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hetuguaaqw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hetuguaaqw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hetuguaaqw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hetuguaaqw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hetuguaaqw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#hetuguaaqw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hetuguaaqw .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#hetuguaaqw .gt_left {
  text-align: left;
}

#hetuguaaqw .gt_center {
  text-align: center;
}

#hetuguaaqw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hetuguaaqw .gt_font_normal {
  font-weight: normal;
}

#hetuguaaqw .gt_font_bold {
  font-weight: bold;
}

#hetuguaaqw .gt_font_italic {
  font-style: italic;
}

#hetuguaaqw .gt_super {
  font-size: 65%;
}

#hetuguaaqw .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">
        <span class="gt_column_spanner"><strong>Kano State</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="5">
        <span class="gt_column_spanner"><strong>Lagos State</strong></span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Clinical Facility</strong>, N = 17<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Community Pharmacy</strong>, N = 3<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>PPMV</strong>, N = 27<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Clinical Facility</strong>, N = 77<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Community Pharmacy</strong>, N = 36<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Lab</strong>, N = 15<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>PPMV</strong>, N = 57<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">ppe_cost_bearingadd_separate_pp</td>
<td class="gt_row gt_center">4 (24%)</td>
<td class="gt_row gt_center">1 (33%)</td>
<td class="gt_row gt_center">9 (33%)</td>
<td class="gt_row gt_center">0.9</td>
<td class="gt_row gt_center">25 (32%)</td>
<td class="gt_row gt_center">4 (11%)</td>
<td class="gt_row gt_center">4 (27%)</td>
<td class="gt_row gt_center">6 (11%)</td>
<td class="gt_row gt_center">0.006</td></tr>
    <tr><td class="gt_row gt_left">ppe_cost_bearingincrease_in_con</td>
<td class="gt_row gt_center">4 (24%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">4 (15%)</td>
<td class="gt_row gt_center">0.8</td>
<td class="gt_row gt_center">17 (22%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">ppe_cost_bearingincrease_in_reg</td>
<td class="gt_row gt_center">4 (24%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">1 (3.7%)</td>
<td class="gt_row gt_center">0.15</td>
<td class="gt_row gt_center">15 (19%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">ppe_cost_bearingincrease_in_lab</td>
<td class="gt_row gt_center">11 (65%)</td>
<td class="gt_row gt_center">2 (67%)</td>
<td class="gt_row gt_center">13 (48%)</td>
<td class="gt_row gt_center">0.6</td>
<td class="gt_row gt_center">50 (65%)</td>
<td class="gt_row gt_center">31 (86%)</td>
<td class="gt_row gt_center">11 (73%)</td>
<td class="gt_row gt_center">43 (75%)</td>
<td class="gt_row gt_center">0.12</td></tr>
    <tr><td class="gt_row gt_left">ppe_cost_bearingincrease_mentio</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">5 (19%)</td>
<td class="gt_row gt_center">0.2</td>
<td class="gt_row gt_center">6 (7.8%)</td>
<td class="gt_row gt_center">3 (8.3%)</td>
<td class="gt_row gt_center">0 (0%)</td>
<td class="gt_row gt_center">13 (23%)</td>
<td class="gt_row gt_center">0.027</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="10">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          n (%)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Fisher's exact test
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
<div id="kabieseuwb" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#kabieseuwb .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#kabieseuwb .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kabieseuwb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kabieseuwb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kabieseuwb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kabieseuwb .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kabieseuwb .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#kabieseuwb .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#kabieseuwb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#kabieseuwb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#kabieseuwb .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#kabieseuwb .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#kabieseuwb .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#kabieseuwb .gt_from_md > :first-child {
  margin-top: 0;
}

#kabieseuwb .gt_from_md > :last-child {
  margin-bottom: 0;
}

#kabieseuwb .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#kabieseuwb .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#kabieseuwb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kabieseuwb .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#kabieseuwb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kabieseuwb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#kabieseuwb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#kabieseuwb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kabieseuwb .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kabieseuwb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#kabieseuwb .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kabieseuwb .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#kabieseuwb .gt_left {
  text-align: left;
}

#kabieseuwb .gt_center {
  text-align: center;
}

#kabieseuwb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kabieseuwb .gt_font_normal {
  font-weight: normal;
}

#kabieseuwb .gt_font_bold {
  font-weight: bold;
}

#kabieseuwb .gt_font_italic {
  font-style: italic;
}

#kabieseuwb .gt_super {
  font-size: 65%;
}

#kabieseuwb .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <caption>**Table 4. Current Status of Facilities and Previous Status during Lockdown</caption>
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1"><strong>Question</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="5">
        <span class="gt_column_spanner"><strong>Kano State</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="5">
        <span class="gt_column_spanner"><strong>Lagos State</strong></span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Clinical Facility</strong>, N = 151<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Community Pharmacy</strong>, N = 17<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Lab</strong>, N = 20<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>PPMV</strong>, N = 550<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Clinical Facility</strong>, N = 395<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Community Pharmacy</strong>, N = 91<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Lab</strong>, N = 108<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>PPMV</strong>, N = 266<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">3</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Currently Open Facilities</td>
<td class="gt_row gt_center">98% (148 / 151)</td>
<td class="gt_row gt_center">100% (17 / 17)</td>
<td class="gt_row gt_center">85% (17 / 20)</td>
<td class="gt_row gt_center">91% (502 / 550)</td>
<td class="gt_row gt_center">0.005</td>
<td class="gt_row gt_center">98% (389 / 395)</td>
<td class="gt_row gt_center">97% (88 / 91)</td>
<td class="gt_row gt_center">100% (108 / 108)</td>
<td class="gt_row gt_center">93% (248 / 266)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">Facilities that Closed at least once during COVID lockdown</td>
<td class="gt_row gt_center">17% (25 / 151)</td>
<td class="gt_row gt_center">24% (4 / 17)</td>
<td class="gt_row gt_center">10% (2 / 20)</td>
<td class="gt_row gt_center">8.4% (46 / 550)</td>
<td class="gt_row gt_center">0.007</td>
<td class="gt_row gt_center">4.8% (19 / 395)</td>
<td class="gt_row gt_center">11% (10 / 91)</td>
<td class="gt_row gt_center">17% (18 / 108)</td>
<td class="gt_row gt_center">21% (55 / 266)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">Times Each Facility Closed</td>
<td class="gt_row gt_center">1.12 (0.33)</td>
<td class="gt_row gt_center">1.00 (0.00)</td>
<td class="gt_row gt_center">1.00 (0.00)</td>
<td class="gt_row gt_center">1.28 (1.34)</td>
<td class="gt_row gt_center">0.9</td>
<td class="gt_row gt_center">1.21 (0.42)</td>
<td class="gt_row gt_center">1.50 (0.97)</td>
<td class="gt_row gt_center">1.11 (0.32)</td>
<td class="gt_row gt_center">1.29 (0.79)</td>
<td class="gt_row gt_center">0.6</td></tr>
    <tr><td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">Number of Facilities That Did Not Shut Down</td>
<td class="gt_row gt_center">126</td>
<td class="gt_row gt_center">13</td>
<td class="gt_row gt_center">18</td>
<td class="gt_row gt_center">504</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">376</td>
<td class="gt_row gt_center">81</td>
<td class="gt_row gt_center">90</td>
<td class="gt_row gt_center">211</td>
<td class="gt_row gt_center"></td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="11">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Mean (SD) or % (n/N)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Fisher's exact test; Kruskal-Wallis rank sum test
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>3</em>
          </sup>
           
          Fisher's exact test; Pearson's Chi-squared test; Kruskal-Wallis rank sum test
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

### In what ways has this facility passed PPE costs on clients/patients?

`ppe_cost_bearing` was a multi-select question with the following
options:

The question represented by the variable `ppe_cost_bearing` asked: “In
what ways has this facility passed PPE costs on clients/patients?”

Answer options:

| Variable                            | Answer                                                             | Type                            | Amount                         |
|:------------------------------------|:-------------------------------------------------------------------|---------------------------------|--------------------------------|
| ppe\_cost\_bearingadd\_separate\_pp | A. Adding a separate PPE fee on each client/patient’s bill         | addseparateppefee\_onbill\_type | addseparateppefee\_onbill\_amt |
| ppe\_cost\_bearingincrease\_in\_con | B. Increasing consultation fees (for individual consultation fee)  | increaseconsultationfees\_type  | increaseconsultationfees\_amt  |
| ppe\_cost\_bearingincrease\_in\_reg | C. Increasing registration/card fees (for individual registration) | increaseregistrationfees\_type  | increaseregistrationfees\_amt  |
| ppe\_cost\_bearingincrease\_in\_lab | D. Increasing prices of labs, medicines, or other products sold    | increaselabsmed\_type           | increaselabsmed\_amt           |
| ppe\_cost\_bearingincrease\_mentio  | E. Other (specify)                                                 | increasementionedothers\_type   | increasementionedothers\_amt   |
| other\_ppe\_cost\_bearing           | Other: Specific                                                    | N/A                             | N/A                            |

<table>
<thead>
<tr>
<th style="text-align:left;">
answer
</th>
<th style="text-align:right;">
Kano
</th>
<th style="text-align:right;">
Kano\_perc
</th>
<th style="text-align:right;">
Lagos
</th>
<th style="text-align:right;">
Lagos\_perc
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
ppe\_cost\_bearingadd\_separate\_pp
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:right;">
29.79
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
21.08
</td>
</tr>
<tr>
<td style="text-align:left;">
ppe\_cost\_bearingincrease\_in\_con
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
17.02
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
9.19
</td>
</tr>
<tr>
<td style="text-align:left;">
ppe\_cost\_bearingincrease\_in\_reg
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
10.64
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
8.11
</td>
</tr>
<tr>
<td style="text-align:left;">
ppe\_cost\_bearingincrease\_in\_lab
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:right;">
55.32
</td>
<td style="text-align:right;">
135
</td>
<td style="text-align:right;">
72.97
</td>
</tr>
<tr>
<td style="text-align:left;">
ppe\_cost\_bearingincrease\_mentio
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
10.64
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
11.89
</td>
</tr>
<tr>
<td style="text-align:left;">
Total Facilities
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
185
</td>
<td style="text-align:right;">
NA
</td>
</tr>
</tbody>
</table>

    ## # A tibble: 14 x 3
    ## # Groups:   answer [14]
    ##    answer      n    perc
    ##    <fct>   <int>   <dbl>
    ##  1 D         126 0.543  
    ##  2 A          28 0.121  
    ##  3 E          24 0.103  
    ##  4 A D        18 0.0776 
    ##  5 B          10 0.0431 
    ##  6 C           6 0.0259 
    ##  7 B C D       5 0.0216 
    ##  8 A B C D     4 0.0172 
    ##  9 B D         3 0.0129 
    ## 10 A B C       2 0.00862
    ## 11 C D         2 0.00862
    ## 12 D E         2 0.00862
    ## 13 A D E       1 0.00431
    ## 14 B C         1 0.00431

# Regressions

Cascade = Total OPD, Total Screened, Total Diagnosed, Total Treated,
Total Completed/Cured - Comparing April 2019 and April 2020 or Q2 2019
vs. Q2 2020

    ##                           var_name Clinical Facility Community Pharmacy   Lab
    ## 315             outcome_fail_total              TRUE              FALSE FALSE
    ## 320             outcome_ltfu_total              TRUE              FALSE FALSE
    ## 325             outcome_died_total              TRUE              FALSE FALSE
    ## 330        outcome_completed_total              TRUE              FALSE FALSE
    ## 335      outcome_transferout_total              TRUE              FALSE FALSE
    ## 340       outcome_transferin_total              TRUE              FALSE FALSE
    ## 341                  outcome_total              TRUE              FALSE FALSE
    ## 348          notifbacter_ptb_total              TRUE              FALSE  TRUE
    ## 362          notifclinic_ptb_total              TRUE              FALSE  TRUE
    ## 436      contactscreen_index_total              TRUE              FALSE FALSE
    ## 441  contactscreen_household_total              TRUE              FALSE FALSE
    ## 462  contactscreen_diagnosed_total              TRUE              FALSE FALSE
    ## 467 contactscreen_ontreat_ds_total              TRUE              FALSE FALSE
    ## 472 contactscreen_ontreat_dr_total              TRUE              FALSE FALSE
    ## 473   contactscreen_starttrt_total              TRUE              FALSE FALSE
    ##      PPMV
    ## 315 FALSE
    ## 320 FALSE
    ## 325 FALSE
    ## 330 FALSE
    ## 335 FALSE
    ## 340 FALSE
    ## 341 FALSE
    ## 348 FALSE
    ## 362 FALSE
    ## 436 FALSE
    ## 441 FALSE
    ## 462 FALSE
    ## 467 FALSE
    ## 472 FALSE
    ## 473 FALSE

    - outcome_cured_total
    - outcome_fail_total
    - outcome_ltfu_total
    - outcome_died_total
    - outcome_completed_total
    - outcome_total
    - notifbacter_ptb_total
    - notifclinic_ptb_total
    - contactscreen_index_total
    - contactscreen_household_total
    - contactscreen_diagnosed_total
    - contactscreen_starttrt_total
    - contactscreen_ontreat_ds_total

<div id="ugbluctivx" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ugbluctivx .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ugbluctivx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ugbluctivx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ugbluctivx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ugbluctivx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ugbluctivx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ugbluctivx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ugbluctivx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ugbluctivx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ugbluctivx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ugbluctivx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ugbluctivx .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ugbluctivx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ugbluctivx .gt_from_md > :first-child {
  margin-top: 0;
}

#ugbluctivx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ugbluctivx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ugbluctivx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ugbluctivx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ugbluctivx .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ugbluctivx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ugbluctivx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ugbluctivx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ugbluctivx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ugbluctivx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ugbluctivx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ugbluctivx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ugbluctivx .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ugbluctivx .gt_left {
  text-align: left;
}

#ugbluctivx .gt_center {
  text-align: center;
}

#ugbluctivx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ugbluctivx .gt_font_normal {
  font-weight: normal;
}

#ugbluctivx .gt_font_bold {
  font-weight: bold;
}

#ugbluctivx .gt_font_italic {
  font-style: italic;
}

#ugbluctivx .gt_super {
  font-size: 65%;
}

#ugbluctivx .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>April 2019</strong>, N = 472<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>April 2020</strong>, N = 472<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Difference</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">2,3</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">outcome_cured_total</td>
<td class="gt_row gt_center">0.10 (0.69)</td>
<td class="gt_row gt_center">0.20 (0.69)</td>
<td class="gt_row gt_center">-0.11</td>
<td class="gt_row gt_center">-0.19, -0.02</td>
<td class="gt_row gt_center">0.017</td></tr>
    <tr><td class="gt_row gt_left">outcome_fail_total</td>
<td class="gt_row gt_center">0.0070 (0.1079)</td>
<td class="gt_row gt_center">0.0139 (0.1170)</td>
<td class="gt_row gt_center">-0.01</td>
<td class="gt_row gt_center">-0.02, 0.01</td>
<td class="gt_row gt_center">0.4</td></tr>
    <tr><td class="gt_row gt_left">outcome_died_total</td>
<td class="gt_row gt_center">0.0047 (0.0682)</td>
<td class="gt_row gt_center">0.0346 (0.1831)</td>
<td class="gt_row gt_center">-0.03</td>
<td class="gt_row gt_center">-0.05, -0.01</td>
<td class="gt_row gt_center">0.002</td></tr>
    <tr><td class="gt_row gt_left">outcome_completed_total</td>
<td class="gt_row gt_center">0.04 (0.22)</td>
<td class="gt_row gt_center">0.14 (0.43)</td>
<td class="gt_row gt_center">-0.11</td>
<td class="gt_row gt_center">-0.16, -0.07</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">outcome_total</td>
<td class="gt_row gt_center">0.18 (0.88)</td>
<td class="gt_row gt_center">0.51 (1.12)</td>
<td class="gt_row gt_center">-0.35</td>
<td class="gt_row gt_center">-0.47, -0.23</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">notifbacter_ptb_total</td>
<td class="gt_row gt_center">0.43 (1.16)</td>
<td class="gt_row gt_center">0.21 (0.84)</td>
<td class="gt_row gt_center">0.23</td>
<td class="gt_row gt_center">0.13, 0.32</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">notifclinic_ptb_total</td>
<td class="gt_row gt_center">0.11 (0.40)</td>
<td class="gt_row gt_center">0.10 (0.45)</td>
<td class="gt_row gt_center">0.00</td>
<td class="gt_row gt_center">-0.05, 0.06</td>
<td class="gt_row gt_center">>0.9</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_index_total</td>
<td class="gt_row gt_center">0.35 (1.49)</td>
<td class="gt_row gt_center">0.08 (0.47)</td>
<td class="gt_row gt_center">0.28</td>
<td class="gt_row gt_center">0.13, 0.44</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_household_total</td>
<td class="gt_row gt_center">0.79 (3.30)</td>
<td class="gt_row gt_center">0.16 (1.03)</td>
<td class="gt_row gt_center">0.65</td>
<td class="gt_row gt_center">0.31, 1.0</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_diagnosed_total</td>
<td class="gt_row gt_center">0.0233 (0.2255)</td>
<td class="gt_row gt_center">0.0023 (0.0481)</td>
<td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">0.00, 0.04</td>
<td class="gt_row gt_center">0.060</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_starttrt_total</td>
<td class="gt_row gt_center">0.0233 (0.2255)</td>
<td class="gt_row gt_center">0.0023 (0.0481)</td>
<td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">0.00, 0.04</td>
<td class="gt_row gt_center">0.060</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_ontreat_ds_total</td>
<td class="gt_row gt_center">0.0233 (0.2255)</td>
<td class="gt_row gt_center">0.0023 (0.0481)</td>
<td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">0.00, 0.04</td>
<td class="gt_row gt_center">0.060</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="6">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Mean (SD)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Paired t-test
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>3</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>
<div id="fowijetoxw" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#fowijetoxw .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#fowijetoxw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fowijetoxw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#fowijetoxw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#fowijetoxw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fowijetoxw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#fowijetoxw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#fowijetoxw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#fowijetoxw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#fowijetoxw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#fowijetoxw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#fowijetoxw .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#fowijetoxw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#fowijetoxw .gt_from_md > :first-child {
  margin-top: 0;
}

#fowijetoxw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#fowijetoxw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#fowijetoxw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#fowijetoxw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fowijetoxw .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#fowijetoxw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#fowijetoxw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#fowijetoxw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#fowijetoxw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#fowijetoxw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fowijetoxw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#fowijetoxw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#fowijetoxw .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#fowijetoxw .gt_left {
  text-align: left;
}

#fowijetoxw .gt_center {
  text-align: center;
}

#fowijetoxw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#fowijetoxw .gt_font_normal {
  font-weight: normal;
}

#fowijetoxw .gt_font_bold {
  font-weight: bold;
}

#fowijetoxw .gt_font_italic {
  font-style: italic;
}

#fowijetoxw .gt_super {
  font-size: 65%;
}

#fowijetoxw .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>May 2019</strong>, N = 472<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>May 2020</strong>, N = 472<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Difference</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">2,3</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">outcome_cured_total</td>
<td class="gt_row gt_center">0.10 (0.50)</td>
<td class="gt_row gt_center">0.16 (0.67)</td>
<td class="gt_row gt_center">-0.06</td>
<td class="gt_row gt_center">-0.12, 0.00</td>
<td class="gt_row gt_center">0.047</td></tr>
    <tr><td class="gt_row gt_left">outcome_fail_total</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center">0.0093 (0.0959)</td>
<td class="gt_row gt_center">-0.01</td>
<td class="gt_row gt_center">-0.02, 0.00</td>
<td class="gt_row gt_center">0.045</td></tr>
    <tr><td class="gt_row gt_left">outcome_died_total</td>
<td class="gt_row gt_center">0.0115 (0.1068)</td>
<td class="gt_row gt_center">0.0208 (0.1430)</td>
<td class="gt_row gt_center">-0.01</td>
<td class="gt_row gt_center">-0.03, 0.01</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">outcome_completed_total</td>
<td class="gt_row gt_center">0.07 (0.40)</td>
<td class="gt_row gt_center">0.13 (0.50)</td>
<td class="gt_row gt_center">-0.06</td>
<td class="gt_row gt_center">-0.12, -0.01</td>
<td class="gt_row gt_center">0.027</td></tr>
    <tr><td class="gt_row gt_left">outcome_total</td>
<td class="gt_row gt_center">0.21 (0.88)</td>
<td class="gt_row gt_center">0.43 (1.28)</td>
<td class="gt_row gt_center">-0.22</td>
<td class="gt_row gt_center">-0.33, -0.11</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">notifbacter_ptb_total</td>
<td class="gt_row gt_center">0.33 (1.17)</td>
<td class="gt_row gt_center">0.28 (0.96)</td>
<td class="gt_row gt_center">0.06</td>
<td class="gt_row gt_center">-0.02, 0.15</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">notifclinic_ptb_total</td>
<td class="gt_row gt_center">0.09 (0.33)</td>
<td class="gt_row gt_center">0.13 (0.47)</td>
<td class="gt_row gt_center">-0.04</td>
<td class="gt_row gt_center">-0.09, 0.02</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_index_total</td>
<td class="gt_row gt_center">0.18 (1.25)</td>
<td class="gt_row gt_center">0.19 (0.91)</td>
<td class="gt_row gt_center">-0.01</td>
<td class="gt_row gt_center">-0.11, 0.08</td>
<td class="gt_row gt_center">0.8</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_household_total</td>
<td class="gt_row gt_center">0.44 (2.64)</td>
<td class="gt_row gt_center">0.53 (2.53)</td>
<td class="gt_row gt_center">-0.09</td>
<td class="gt_row gt_center">-0.33, 0.16</td>
<td class="gt_row gt_center">0.5</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_diagnosed_total</td>
<td class="gt_row gt_center">0.0276 (0.2788)</td>
<td class="gt_row gt_center">0.0093 (0.1176)</td>
<td class="gt_row gt_center">0.02</td>
<td class="gt_row gt_center">-0.01, 0.05</td>
<td class="gt_row gt_center">0.2</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_starttrt_total</td>
<td class="gt_row gt_center">0.0161 (0.1725)</td>
<td class="gt_row gt_center">0.0023 (0.0481)</td>
<td class="gt_row gt_center">0.01</td>
<td class="gt_row gt_center">0.00, 0.03</td>
<td class="gt_row gt_center">0.11</td></tr>
    <tr><td class="gt_row gt_left">contactscreen_ontreat_ds_total</td>
<td class="gt_row gt_center">0.0161 (0.1725)</td>
<td class="gt_row gt_center">0.0023 (0.0481)</td>
<td class="gt_row gt_center">0.01</td>
<td class="gt_row gt_center">0.00, 0.03</td>
<td class="gt_row gt_center">0.11</td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="6">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Mean (SD)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Paired t-test
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>3</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

## Costs 2019 vs. 2020

-   cost\_of\_registration
-   cost\_of\_consultation\_prior\_diag
-   cost\_of\_sptum\_afb\_test
-   cost\_of\_gene\_xpert\_test
-   cost\_of\_xray\_test
-   treatment\_amount
-   cost\_of\_hiv\_test
-   cost\_tb\_drugs\_for\_6mo\_trt
-   cost\_tb\_consultations\_for\_6mo
-   cost\_tb\_followup\_tests\_for\_6mo

### Kano

<div id="yqplwkdour" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#yqplwkdour .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#yqplwkdour .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yqplwkdour .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#yqplwkdour .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#yqplwkdour .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yqplwkdour .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yqplwkdour .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#yqplwkdour .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#yqplwkdour .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#yqplwkdour .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#yqplwkdour .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#yqplwkdour .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#yqplwkdour .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#yqplwkdour .gt_from_md > :first-child {
  margin-top: 0;
}

#yqplwkdour .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yqplwkdour .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#yqplwkdour .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#yqplwkdour .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yqplwkdour .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#yqplwkdour .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yqplwkdour .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#yqplwkdour .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#yqplwkdour .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yqplwkdour .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yqplwkdour .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#yqplwkdour .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yqplwkdour .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#yqplwkdour .gt_left {
  text-align: left;
}

#yqplwkdour .gt_center {
  text-align: center;
}

#yqplwkdour .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yqplwkdour .gt_font_normal {
  font-weight: normal;
}

#yqplwkdour .gt_font_bold {
  font-weight: bold;
}

#yqplwkdour .gt_font_italic {
  font-style: italic;
}

#yqplwkdour .gt_super {
  font-size: 65%;
}

#yqplwkdour .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="6">
        <span class="gt_column_spanner"><strong>Clinical Facility</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">
        <span class="gt_column_spanner"><strong>Community Pharmacy</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">
        <span class="gt_column_spanner"><strong>Lab</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">
        <span class="gt_column_spanner"><strong>PPMV</strong></span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2019</strong>, N = 217<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2020</strong>, N = 217<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Difference</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">2,3</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2019</strong>, N = 28<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2020</strong>, N = 28<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2019</strong>, N = 35<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2020</strong>, N = 35<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2019</strong>, N = 804<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2020</strong>, N = 804<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">cost_of_registration</td>
<td class="gt_row gt_center">139</td>
<td class="gt_row gt_center">648 (2,282)</td>
<td class="gt_row gt_center">597 (1,078)</td>
<td class="gt_row gt_center">51</td>
<td class="gt_row gt_center">-512, 615</td>
<td class="gt_row gt_center">0.9</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">2</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">750 (1,061)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_consultation_prior_diag</td>
<td class="gt_row gt_center">147</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">599 (937)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">6</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">1,167 (983)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_sptum_afb_test</td>
<td class="gt_row gt_center">250</td>
<td class="gt_row gt_center">5.8252 (59.1198)</td>
<td class="gt_row gt_center">48.9796 (236.8073)</td>
<td class="gt_row gt_center">-43</td>
<td class="gt_row gt_center">-83, -2.9</td>
<td class="gt_row gt_center">0.036</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">15</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">307 (271)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">6</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_gene_xpert_test</td>
<td class="gt_row gt_center">250</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center">7.5442 (58.6208)</td>
<td class="gt_row gt_center">-7.5</td>
<td class="gt_row gt_center">-17, 2.0</td>
<td class="gt_row gt_center">0.12</td>
<td class="gt_row gt_center">2</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">6</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">8</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_xray_test</td>
<td class="gt_row gt_center">132</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center">233.3333 (762.6331)</td>
<td class="gt_row gt_center">-233</td>
<td class="gt_row gt_center">-518, 51</td>
<td class="gt_row gt_center">0.10</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">2</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">treatment_amount</td>
<td class="gt_row gt_center">102</td>
<td class="gt_row gt_center">539 (2,504)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_hiv_test</td>
<td class="gt_row gt_center">243</td>
<td class="gt_row gt_center">294 (450)</td>
<td class="gt_row gt_center">566 (542)</td>
<td class="gt_row gt_center">-273</td>
<td class="gt_row gt_center">-399, -146</td>
<td class="gt_row gt_center"><0.001</td>
<td class="gt_row gt_center">2</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">1,000.0000 (0.0000)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">17</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">1,071 (406)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">8</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">938 (417)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_tb_drugs_for_6mo_trt</td>
<td class="gt_row gt_center">147</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">57.8231 (480.8561)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">6</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_tb_consultations_for_6mo</td>
<td class="gt_row gt_center">147</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">269 (1,747)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">6</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_tb_followup_tests_for_6mo</td>
<td class="gt_row gt_center">147</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">40.8163 (237.7852)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">6</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (0.0000)</td>
<td class="gt_row gt_center"></td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="19">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Mean (SD)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Welch Two Sample t-test
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>3</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

### Lagos

<div id="qxjnxplyco" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#qxjnxplyco .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#qxjnxplyco .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qxjnxplyco .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qxjnxplyco .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qxjnxplyco .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qxjnxplyco .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#qxjnxplyco .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#qxjnxplyco .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#qxjnxplyco .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qxjnxplyco .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qxjnxplyco .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#qxjnxplyco .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#qxjnxplyco .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#qxjnxplyco .gt_from_md > :first-child {
  margin-top: 0;
}

#qxjnxplyco .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qxjnxplyco .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#qxjnxplyco .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#qxjnxplyco .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qxjnxplyco .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#qxjnxplyco .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qxjnxplyco .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qxjnxplyco .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qxjnxplyco .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qxjnxplyco .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qxjnxplyco .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#qxjnxplyco .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#qxjnxplyco .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#qxjnxplyco .gt_left {
  text-align: left;
}

#qxjnxplyco .gt_center {
  text-align: center;
}

#qxjnxplyco .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qxjnxplyco .gt_font_normal {
  font-weight: normal;
}

#qxjnxplyco .gt_font_bold {
  font-weight: bold;
}

#qxjnxplyco .gt_font_italic {
  font-style: italic;
}

#qxjnxplyco .gt_super {
  font-size: 65%;
}

#qxjnxplyco .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1"><strong>Characteristic</strong></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="6">
        <span class="gt_column_spanner"><strong>Clinical Facility</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">
        <span class="gt_column_spanner"><strong>Community Pharmacy</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">
        <span class="gt_column_spanner"><strong>Lab</strong></span>
      </th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">
        <span class="gt_column_spanner"><strong>PPMV</strong></span>
      </th>
    </tr>
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2019</strong>, N = 472<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2020</strong>, N = 472<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>Difference</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>95% CI</strong><sup class="gt_footnote_marks">2,3</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2019</strong>, N = 195<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2020</strong>, N = 195<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2019</strong>, N = 140<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2020</strong>, N = 140<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>N</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2019</strong>, N = 511<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>2020</strong>, N = 511<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1"><strong>p-value</strong></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">cost_of_registration</td>
<td class="gt_row gt_center">583</td>
<td class="gt_row gt_center">1,385 (3,663)</td>
<td class="gt_row gt_center">1,630 (4,476)</td>
<td class="gt_row gt_center">-245</td>
<td class="gt_row gt_center">-1,001, 511</td>
<td class="gt_row gt_center">0.5</td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_consultation_prior_diag</td>
<td class="gt_row gt_center">394</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">823 (2,411)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_sptum_afb_test</td>
<td class="gt_row gt_center">798</td>
<td class="gt_row gt_center">119 (491)</td>
<td class="gt_row gt_center">192 (689)</td>
<td class="gt_row gt_center">-73</td>
<td class="gt_row gt_center">-157, 9.9</td>
<td class="gt_row gt_center">0.084</td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">88</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">993 (1,133)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_gene_xpert_test</td>
<td class="gt_row gt_center">802</td>
<td class="gt_row gt_center">2.4510 (34.9640)</td>
<td class="gt_row gt_center">76.1421 (1,269.2244)</td>
<td class="gt_row gt_center">-74</td>
<td class="gt_row gt_center">-199, 52</td>
<td class="gt_row gt_center">0.3</td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">97</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">51.5464 (326.6348)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_xray_test</td>
<td class="gt_row gt_center">573</td>
<td class="gt_row gt_center">347 (1,207)</td>
<td class="gt_row gt_center">1,656 (2,208)</td>
<td class="gt_row gt_center">-1,309</td>
<td class="gt_row gt_center">-1,658, -960</td>
<td class="gt_row gt_center"><0.001</td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">treatment_amount</td>
<td class="gt_row gt_center">402</td>
<td class="gt_row gt_center">2,190 (7,009)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_of_hiv_test</td>
<td class="gt_row gt_center">796</td>
<td class="gt_row gt_center">732 (796)</td>
<td class="gt_row gt_center">1,035 (792)</td>
<td class="gt_row gt_center">-303</td>
<td class="gt_row gt_center">-413, -192</td>
<td class="gt_row gt_center"><0.001</td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">103</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">1,212 (423)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_tb_drugs_for_6mo_trt</td>
<td class="gt_row gt_center">394</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">2,674 (7,251)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_tb_consultations_for_6mo</td>
<td class="gt_row gt_center">394</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">736 (3,210)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
    <tr><td class="gt_row gt_left">cost_tb_followup_tests_for_6mo</td>
<td class="gt_row gt_center">394</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">251 (793)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">1</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">0.0000 (NA)</td>
<td class="gt_row gt_center"></td>
<td class="gt_row gt_center">0</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center">NA (NA)</td>
<td class="gt_row gt_center"></td></tr>
  </tbody>
  
  <tfoot>
    <tr class="gt_footnotes">
      <td colspan="19">
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>1</em>
          </sup>
           
          Mean (SD)
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>2</em>
          </sup>
           
          Welch Two Sample t-test
          <br />
        </p>
        <p class="gt_footnote">
          <sup class="gt_footnote_marks">
            <em>3</em>
          </sup>
           
          CI = Confidence Interval
          <br />
        </p>
      </td>
    </tr>
  </tfoot>
</table>
</div>

# Additional Variables of Interest

## CAT 1 Kits and Mini Kits

## hivpos\_refforcare\_total

## cases\_txsupporter\_total

## open\_date1

## Telemedicine Analysis

    ## [1] 186

    ## [1] 65

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

<img src="outputs/img/README-telemedicine5-1.png" style="display: block; margin: auto;" />
