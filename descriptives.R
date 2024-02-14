### Libraries ------------------------------------------------------------------
library(dplyr) # for data wrangling
library(haven) # for read.csv()
library(psych) # for describe()

##### Data import --------------------------------------------------------------
demojudges <- read.csv("data/demojudges.csv") |> 
  as_tibble()

##### Dependent variables ------------------------------------------------------
dvs <- demojudges |> 
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
  mutate("NA" = 9672 - n,
         mean = round(mean, 2),
         sd = round(sd, 2))

kable(dvs,
      col.names = c("Variable", "Min", "Max", "Median", "Mean", "SD", "N", "Missing"),
      caption = "Descriptives: dependent variables",
      label = "desc-dvs",
      format = "latex",
      booktabs = TRUE)

##### Interval variables -------------------------------------------------------
interval <- demojudges |> 
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
  mutate("NA" = 9672 - n,
         mean = round(mean, 2),
         sd = round(sd, 2))

kable(interval,
      col.names = c("Variable", "Min", "Max", "Median", "Mean", "SD", "N", "Missing"),
      caption = "Descriptives: interval covariates",
      label = "desc-interval",
      format = "latex",
      booktabs = TRUE)

##### Treatment groups ---------------------------------------------------------
treatment_groups <- demojudges |> 
  group_by(treatment_group) |> 
  summarise(count = n())

kable(treatment_groups,
      col.names = c("Treatment group", "N"),
      caption = "Descriptives: countries",
      label = "desc-countries",
      format = "latex",
      booktabs = TRUE)

##### Categorical variables ----------------------------------------------------
# sex
sex_freq <- demojudges |> 
  group_by(sex) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         sex = case_when(
           sex == 1 ~ "Male",
           sex == 2 ~ "Female",
           TRUE ~ "Missing"))

kable(sex_freq,
      col.names = c("Sex", "Count", "Percentage"),
      caption = "Descriptives: sex",
      label = "desc-sex",
      format = "latex",
      booktabs = TRUE)

# education
edu_freq <- demojudges |> 
  group_by(edu) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         edu = case_when(
           edu == 1 ~ "Low",
           edu == 2 ~ "Middle",
           edu == 3 ~ "High",
           TRUE ~ "Missing"))

kable(edu_freq,
      col.names = c("Education Level", "Count", "Percentage"),
      caption = "Descriptives: education",
      label = "desc-edu",
      format = "latex",
      booktabs = TRUE)

# vote NL
vote_nl_freq <- demojudges |> 
  filter(country == "NL") |> 
  group_by(vote) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         vote = case_when(
           is.na(vote) ~ "Missing",
           TRUE ~ vote))

kable(vote_nl_freq,
      col.names = c("Vote", "Count", "Percentage"),
      caption = "Descriptives: Vote in Dutch 2021 parliamentary elections",
      label = "desc-vote_nl",
      format = "latex",
      booktabs = TRUE)

# vote FR
vote_fr_freq <- demojudges |> 
  filter(country == "FR") |> 
  group_by(vote) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         vote = case_when(
           is.na(vote) ~ "Missing",
           TRUE ~ vote))

kable(vote_fr_freq,
      col.names = c("Vote", "Count", "Percentage"),
      caption = "Descriptives: Vote in French 2022 presidential elections",
      label = "desc-vote_fr",
      format = "latex",
      booktabs = TRUE)

# vote DE
vote_de_freq <- demojudges |> 
  filter(country == "DE") |> 
  group_by(vote) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         vote = case_when(
           is.na(vote) ~ "Missing",
           TRUE ~ vote))

kable(vote_de_freq,
      col.names = c("Vote", "Count", "Percentage"),
      caption = "Descriptives: Zweitstimme in German 2021 federal elections",
      label = "desc-vote_de",
      format = "latex",
      booktabs = TRUE)

# geo NL
geo_nl_freq <- demojudges |> 
  filter(country == "NL") |> 
  group_by(geo) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         geo = case_when(
           is.na(geo) ~ "Missing",
           TRUE ~ geo))

kable(geo_nl_freq,
      col.names = c("Province", "Count", "Percentage"),
      caption = "Descriptives: Dutch provinces",
      label = "desc-geo_nl",
      format = "latex",
      booktabs = TRUE)

# geo DE
geo_de_freq <- demojudges |> 
  filter(country == "DE") |> 
  group_by(geo) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         geo = case_when(
           is.na(geo) ~ "Missing",
           TRUE ~ geo))

kable(geo_de_freq,
      col.names = c("Bundesland", "Count", "Percentage"),
      caption = "Descriptives: German Bundesländer",
      label = "desc-geo_de",
      format = "latex",
      booktabs = TRUE)

# geo FR
geo_fr_freq <- demojudges |> 
  filter(country == "FR") |> 
  group_by(geo) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         geo = case_when(
           is.na(geo) ~ "Missing",
           TRUE ~ geo))

kable(geo_fr_freq,
      col.names = c("Région", "Count", "Percentage"),
      caption = "Descriptives: French regions",
      label = "desc-geo_fr",
      format = "latex",
      booktabs = TRUE)

# country
country_freq <- demojudges |> 
  group_by(country) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"))

kable(country_freq,
      col.names = c("Country", "Count", "Percentage"),
      caption = "Descriptives: Respondents per country",
      label = "desc-country",
      format = "latex",
      booktabs = TRUE)

# income NL
income_nl_freq <- demojudges |> 
  filter(country == "NL") |> 
  group_by(income_nl) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         income_nl = case_when(
           is.na(income_nl) ~ "Missing",
           income_nl == 1 ~ "Less than € 15285",
           income_nl == 2 ~ "€ 15285 to € 19987",
           income_nl == 3 ~ "€ 19987 to € 23929",
           income_nl == 4 ~ "€ 23929 to € 28584",
           income_nl == 5 ~ "€ 28584 to € 34518",
           income_nl == 6 ~ "€ 34518 to € 42261",
           income_nl == 7 ~ "€ 42261 to € 51218",
           income_nl == 8 ~ "€ 51218 to € 61402",
           income_nl == 9 ~ "€ 61402 to € 76968",
           income_nl == 10 ~ "More than € 76968"))

kable(income_nl_freq,
      col.names = c("Income", "Count", "Percentage"),
      caption = "Descriptives: Income for Dutch respondents",
      label = "desc-income_nl",
      format = "latex",
      booktabs = TRUE)

# income DE
income_de_freq <- demojudges |> 
  filter(country == "DE") |> 
  group_by(income_de) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         income_de = case_when(
           is.na(income_de) ~ "Missing",
           income_de == 1 ~ "Less than € 11.504",
           income_de == 2 ~ "€ 11.504 to € 16.959",
           income_de == 3 ~ "€ 16.959 to € 21.688",
           income_de == 4 ~ "€ 21.688 to € 26.528",
           income_de == 5 ~ "€ 26.528 to € 31.157",
           income_de == 6 ~ "€ 31.157 to € 36.973",
           income_de == 7 ~ "€ 36.973 to € 44.214",
           income_de == 8 ~ "€ 44.214 to € 54.195",
           income_de == 9 ~ "€ 54.195 to € 68.179",
           income_de == 10 ~ "More than € 68.179€"))

kable(income_de_freq,
      col.names = c("Income", "Count", "Percentage"),
      caption = "Descriptives: Income for German respondents",
      label = "desc-income_de",
      format = "latex",
      booktabs = TRUE)

# income FR
income_fr_freq <- demojudges |> 
  filter(country == "FR") |> 
  group_by(income_fr) |> 
  summarise(count = n()) |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"),
         income_fr = case_when(
           is.na(income_fr) ~ "Missing",
           income_fr == 1 ~ "Less than € 13740",
           income_fr == 2 ~ "€ 13740 to € 18750",
           income_fr == 3 ~ "€ 18750 to € 22840",
           income_fr == 4 ~ "€ 22840 to € 27270",
           income_fr == 5 ~ "€ 27270 to € 32350",
           income_fr == 6 ~ "€ 32350 to € 37670",
           income_fr == 7 ~ "€ 37670 to € 44150",
           income_fr == 8 ~ "€ 44150 to € 52800",
           income_fr == 9 ~ "€ 52800 to € 68070",
           income_fr == 10 ~ "More than € 68070"))

kable(income_fr_freq,
      col.names = c("Income", "Count", "Percentage"),
      caption = "Descriptives: Income for French respondents",
      label = "desc-income_fr",
      format = "latex",
      booktabs = TRUE)

# employment
emp_freq <- demojudges |> 
  dplyr::select(starts_with("emp_"),
                -emp_opn) |>
  mutate(
    emp_school = sum(demojudges$emp_school),
    emp_student = sum(demojudges$emp_student),
    emp_self = sum(demojudges$emp_self),
    emp_full = sum(demojudges$emp_full),
    emp_part = sum(demojudges$emp_part),
    emp_diff = sum(demojudges$emp_diff),
    emp_parent = sum(demojudges$emp_parent),
    emp_pension = sum(demojudges$emp_pension),
    emp_unemp = sum(demojudges$emp_unemp),
    emp_diff2 = sum(demojudges$dif2)) |> 
  unique() |> 
  t() |> 
  as_tibble(rownames = "variable") |> 
  rename("count" = "1") |> 
  mutate(perc = round(count / sum(count) * 100, 2),
         perc = paste0(perc, "%"))

kable(emp_freq,
      col.names = c("Variable", "Count", "Percentage"),
      caption = "Descriptives: Education levels",
      label = "employment",
      format = "latex",
      booktabs = TRUE)















































































