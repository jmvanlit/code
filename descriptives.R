### Libraries ------------------------------------------------------------------
library(dplyr) # for data wrangling
library(haven) # for read.csv()
library(kableExtra)

##### Data import --------------------------------------------------------------
descriptives_raw <- read.csv("data/demojudges.csv") |> 
  as_tibble() |> 
  filter(!is.na(justification)) |> 
  filter(!is.na(action)) |> 
  filter(!is.na(demdef))

# number of respondents who failed to answer the frelect-question.
na_frelect <- descriptives_raw |> 
  filter(is.na(post_libdem_frelect)) |> 
  length()

descriptives <- descriptives_raw |> 
  filter(!is.na(post_libdem_frelect))

##### Treatment groups ---------------------------------------------------------
treatment_groups <- descriptives |> 
  group_by(treatment_group) |> 
  summarise(count = n())

# Table A.1
kable(treatment_groups,
      col.names = c("Treatment group", "N"),
      caption = "Descriptives: countries",
      label = "desc-countries",
      format = "latex",
      booktabs = TRUE)

total_n <- sum(treatment_groups$count)

##### Countries ----------------------------------------------------------------
country_weighted_n <- descriptives |> 
  filter(!is.na(weight)) |> 
  group_by(country) |> 
  summarise(count_w = n()) 

country_freq <- descriptives |> 
  group_by(country) |> 
  summarise(count = n())|> 
  left_join(country_weighted_n, by = c("country")) |> 
  mutate(country = case_when(
    country == "DE" ~ "Germany",
    country == "FR" ~ "France",
    country == "NL" ~ "Netherlands"))

# Table B.1
kable(country_freq,
      col.names = c("Country", "Count (unweighted)", "Count (weighted)"),
      caption = "Descriptives: Respondents per country",
      label = "desc-country",
      format = "latex",
      booktabs = TRUE)

##### Dependent variables ------------------------------------------------------
dvs <- descriptives |> 
    dplyr::select(dv_eval,
    dv_ambi,
    dv_cred,
    post_agree,
    dv_protest_vote,
    dv_protest_poster,
    dv_protest_pers,
    dv_protest_peti,
    dv_protest_lawpr,
    dv_protest_cont,
    dv_protest_unlaw,
    dv_protest) |> 
  describe(na.rm = TRUE) |> 
  as_tibble(rownames = "variable") |> 
  dplyr::select(variable, min, max, median, mean, sd, n) |> 
  mutate("NA" = 9438 - n,
         mean = round(mean, 2),
         sd = round(sd, 2))

# Table B.2
kable(dvs,
      col.names = c("Variable", "Min", "Max", "Median", "Mean", "SD", "N", "Missing"),
      caption = "Descriptives: dependent variables",
      label = "desc-dvs",
      format = "latex",
      booktabs = TRUE)

##### Interval variables -------------------------------------------------------
interval <- descriptives |> 
  mutate(age = 2023 - byear) |> 
  dplyr::select(
    pol_interest,
    rile,
    dem_eval_cn,
    dem_eval_uk,
    dem_eval_ar,
    dem_eval_no,
    dem_eval_se,
    pol_trust_crt,
    pol_trust_gov,
    pol_trust_med,
    pol_trust_pol,
    pol_trust_par,
    post_libdem_frexp,
    post_libdem_frassc,
    post_libdem_unisuff,
    post_libdem_frelect,
    post_libdem_judcnstr,
    post_libdem_eqlaw,
    post_dem_satis,
    post_dem_sup) |> 
  describe(na.rm = TRUE) |> 
  as_tibble(rownames = "variable") |> 
  dplyr::select(variable, min, max, median, mean, sd, n) |> 
  mutate("NA" = 9438 - n,
         mean = round(mean, 2),
         sd = round(sd, 2))

# Table B.3
kable(interval,
      col.names = c("Variable", "Min", "Max", "Median", "Mean", "SD", "N", "Missing"),
      caption = "Descriptives: interval covariates",
      label = "desc-interval",
      format = "latex",
      booktabs = TRUE)

# /./ END OF CODE /./ #