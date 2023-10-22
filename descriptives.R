

### Libraries ------------------------------------------------------------------
library(dplyr)
library(haven)
library(ggplot2)

##### Data import --------------------------------------------------------------
demojudges_raw <- read.csv("data/demojudges.csv") |> 
  as_tibble()
    
##### Data preparation  --------------------------------------------------------
demojudges <- demojudges_raw |> 
  mutate(
    ### Create treatment-dummies
    justification = case_when(
      treatment_group == 1 ~ "corruption",
      treatment_group == 2 ~ "corruption",
      treatment_group == 3 ~ "self-serving",
      treatment_group == 4 ~ "self-serving",
      treatment_group == 5 ~ "none",
      treatment_group == 6 ~ "none",
      treatment_group == 7 ~ "corruption",
      treatment_group == 8 ~ "corruption",
      treatment_group == 9 ~ "self-serving",
      treatment_group == 10 ~ "self-serving",
      treatment_group == 11 ~ "none",
      treatment_group == 12 ~ "none"
    ),
    action = case_when(
      treatment_group <= 6 ~ "judiciary",
      treatment_group >= 7 ~ "media"
    ),
    demdef = case_when(
      treatment_group == 1 ~ "yes",
      treatment_group == 2 ~ "no",
      treatment_group == 3 ~ "yes",
      treatment_group == 4 ~ "no",
      treatment_group == 5 ~ "yes",
      treatment_group == 6 ~ "no",
      treatment_group == 7 ~ "yes",
      treatment_group == 8 ~ "no",
      treatment_group == 9 ~ "yes",
      treatment_group == 10 ~ "no",
      treatment_group == 11 ~ "yes",
      treatment_group == 12 ~ "no"),
    
    # Convert to factor
    justification = as.factor(justification),
    action = as.factor(action),
    demdef = as.factor(demdef))

### Set reference categories
demojudges$justification <- relevel(demojudges$justification, ref = "none")
demojudges$action <- relevel(demojudges$action, ref = "media")
demojudges$demdef <- relevel(demojudges$demdef, ref = "no")

### remove 'don't want to says'
demojudges_desc <- demojudges |> 
  mutate(pol_interest = ifelse(pol_interest == "99", NA, pol_interest),
         rile = ifelse(rile == "99", NA, rile),
         post_libdem_frexp = ifelse(post_libdem_frexp == "99", NA, post_libdem_frexp),
         post_libdem_frassc = ifelse(post_libdem_frassc == "99", NA, post_libdem_frassc),
         post_libdem_eqlaw = ifelse(post_libdem_eqlaw == "99", NA, post_libdem_eqlaw),
         post_libdem_unisuff = ifelse(post_libdem_unisuff == "99", NA, post_libdem_unisuff), 
         post_libdem_frelect = ifelse(post_libdem_frelect == "99", NA, post_libdem_frelect),
         post_libdem_judcnstr = ifelse(post_libdem_judcnstr == "99", NA, post_libdem_judcnstr),
         post_dem_satis = ifelse(post_dem_satis == "99", NA, post_dem_satis),
         post_dem_sup = ifelse(post_dem_sup == "99", NA, post_dem_sup))

##### Descriptives Full Sample -------------------------------------------------
### Interval
descriptives <- demojudges_desc |> 
  mutate(age = 2023 - byear) |> 
  select(pol_interest, byear, rile, edu,
         pol_trust_crt, pol_trust_gov, pol_trust_med, pol_trust_pol, pol_trust_par,
         post_libdem_frexp, post_libdem_frassc, post_libdem_eqlaw, post_libdem_unisuff, post_libdem_frelect, post_libdem_judcnstr,
         post_dem_satis, post_dem_sup,
         dem_eval_cn, dem_eval_no, dem_eval_uk, dem_eval_ar, dem_eval_se) |> 
  describe(na.rm = TRUE) |> 
  as_tibble(rownames = "variable") |> 
  select(variable, mean, median, sd, min, max, n) |> 
  mutate("NA" = 9672 - n) |> 
  filter()

descriptives_dvs <- demojudges_desc |> 
  select(dv_eval, post_agree, dv_cred, dv_ambi,
         dv_protest_vote, dv_protest_cont, dv_protest_poster, dv_protest_pers, dv_protest_peti, dv_protest_lawpr, dv_protest_unlaw) |> 
  describe() |> 
  as_tibble(rownames = "variable") |> 
  select(variable, mean, median, sd, min, max, n) |> 
  mutate("NA" = 9672 - n)

country_dem_evals <- demojudges_desc |> 
  mutate(age = 2023 - byear) |> 
  select(dem_eval_cn, dem_eval_no, dem_eval_uk, dem_eval_ar, dem_eval_se) |> 
  describe() |> 
  as_tibble(rownames = "variable") |> 
  select(variable, mean, median, sd, min, max, n) |> 
  mutate("NA" = 9672 - n)

### Non-interval
sex <- demojudges_desc |> 
  select(sex) |> 
  na.omit() |> 
  group_by(sex) |> 
  summarise(count = n()) |> 
  mutate(sex = ifelse(sex == 1, "Male", "Female"))

education <- demojudges_desc |> 
  select(edu) |> 
  na.omit() |> 
  group_by(edu) |> 
  summarise(count = n())




mean(demojudges$dem_eval_no,
     na.rm = TRUE)

sd(demojudges$dem_eval_no,
   na.rm = TRUE)

countries <- demojudges |> 
  group_by(country) |> 
  summarise(count = n()) |> print()

### Treatment groups
treatment_groups <- demojudges |> 
  group_by(treatment_group) |> 
  summarise(count = n())

treatment_groups

