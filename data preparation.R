### Libraries ------------------------------------------------------------------
library(dplyr) # for the data cleaning
library(haven) # to read .sav
library (labelled) # to remove value labels to avoid warnings when merging

### Clean NL -------------------------------------------------------------------
df_raw_nl <- read_sav("data/NL/NLOZ23-07-VENI updated labels and variables.sav")

# More income-data was collected in a second survey in October
more_income_nl <- read_sav("data/NL/NLOZ23-07-VENI + NLOZ23-10-VENI with income.sav") |> 
  dplyr::select(Q38.income.category_Oktober, ResponseId) |> 
  filter(!ResponseId == "")

df_nl <- df_raw_nl |> 
  dplyr::select(
    
    # experimental conditions
    treatment_group,
    action,
    demdef,
    justification,
    
    # variables of interest
    dv_eval,
    dv_ambi,
    dv_cred,
    post_agree,
    starts_with("dv_protest"),
    
    # covariates for balancing
    pol_interest,
    rile,
    starts_with("dem_eval"),
    starts_with("pol_trust"),
    starts_with("post_libdem"),
    post_dem_satis,
    post_dem_sup,
    byear,
    sex_rec,
    edu_3_rec,
    vote.recall_rec,
    geo_prov_rec,
    starts_with("Q13.emp.status"),
    Q38.income.category,
    
    # weights
    weight,
    
    # attention and manipulation
    starts_with("attention"),
    starts_with("dj_mc"),
    
    # meta info
    StartDate, EndDate, Duration__in_seconds_, RecordedDate,
    ResponseId) |> 
  
  # remove all variables about survey flow
  dplyr::select(-matches("_DO_")) |> 
  
  # add country identifier for merging
  mutate(country = "NL") |> 
  
  # rename variables for merging
  rename(sex = sex_rec,
         vote = vote.recall_rec,
         edu = edu_3_rec,
         geo = geo_prov_rec,
         income_nl = Q38.income.category) |> 
  
  # vote recall
  mutate(vote = case_when(
    vote == 1 ~ "VVD",
    vote == 2 ~ "D66",
    vote == 3 ~ "PVV",
    vote == 4 ~ "CDA",
    vote == 5 ~ "SP",
    vote == 6 ~ "PvdA",
    vote == 7 ~ "GL",
    vote == 8 ~ "FvD",
    vote == 9 ~ "PvdD",
    vote == 10 ~ "CU",
    vote == 11 ~ "JA21",
    vote == 12 ~ "SGP",
    vote == 13 ~ "Volt",
    vote == 14 ~ "DENK",
    vote == 15 ~ "50Plus",
    vote == 16 ~ "BBB",
    vote == 17 ~ "BIJ1",
    vote == 66 ~ "Other",
    vote == 77 ~ "None of the above",
    vote == 88 ~ "Not allowed to vote",
    vote == 98 ~ "Did not vote",
  )) |> 
  
  # region
  mutate(geo = case_when(
    geo == 1 ~ "Drenthe",
    geo == 2 ~ "Flevoland",
    geo == 3 ~ "Friesland",
    geo == 4 ~ "Gelderland",
    geo == 5 ~ "Groningen",
    geo == 6 ~ "Limburg",
    geo == 7 ~ "Noord-Brabant",
    geo == 8 ~ "Noord-Holland",
    geo == 9 ~ "Overijssel",
    geo == 10 ~ "Utrecht",
    geo == 11 ~ "Zeeland",
    geo == 12 ~ "Zuid-Holland",
    geo == NA ~ "NA",
  )) |> 
  
  # employment
  rename(
    emp_school = Q13.emp.status_1,
    emp_student = Q13.emp.status_2,
    emp_self = Q13.emp.status_3,
    emp_full = Q13.emp.status_4,
    emp_part = Q13.emp.status_5,
    emp_diff = Q13.emp.status_6,
    emp_parent = Q13.emp.status_7,
    emp_pension = Q13.emp.status_8,
    emp_unemp = Q13.emp.status_9,
    emp_diff2 = Q13.emp.status_10,
    emp_opn = Q13.emp.status_10_TEXT
  ) |> 
  
  # merge the extra income data
  left_join(more_income_nl, by = c("ResponseId")) |> 
  mutate(income_nl = case_when(
    is.na(income_nl) ~ Q38.income.category_Oktober, # give precedence to the July-data if respondent answered both surveys
    TRUE ~ income_nl)) |> 
  dplyr::select(-Q38.income.category_Oktober)

val_labels(df_nl) <- NULL
rm(df_raw_nl)

### Clean DE -------------------------------------------------------------------
df_raw_de <- read_sav("data/DE/DEOZ23-07_Veni.sav")

df_de <- df_raw_de |> 
  dplyr::select(
    
    # experimental conditions
    treatment_group,
    action,
    demdef,
    justification,
    
    # variables of interest
    dv_eval,
    dv_eval,
    dv_ambi,
    dv_cred,
    post_agree,
    starts_with("dv_protest"),
    
    # covariates for balancing
    pol_interest,
    rile,
    starts_with("dem_eval"),
    starts_with("pol_trust"),
    starts_with("post_libdem"),
    post_dem_satis,
    post_dem_sup,
    byear,
    sex_rec,
    edu_3_rec,
    vote,
    geo_rec,
    starts_with("Q13.emp.status"),
    Q38.income.category,
    
    # weights
    weight,
    
    # attention and manipulation
    starts_with("attention"),
    starts_with("dj_mc"),
    
    # meta info
    StartDate, EndDate, Duration__in_seconds_, RecordedDate,
    ResponseId) |> 
  
  # remove all variables about survey flow
  dplyr::select(-matches("_DO_")) |> 
  
  # add country identifier for merging
  mutate(country = "DE") |> 
  
  # rename variables for merging
  rename(sex = sex_rec,
         geo = geo_rec,
         edu = edu_3_rec,
         income_de = Q38.income.category) |> 
  
  # add labels to relevant variables
  # vote recall
  mutate(vote = case_when(
    vote == 1 ~ "CDU/CSU",
    vote == 2 ~ "SPD",
    vote == 3 ~ "Die Linke",
    vote == 4 ~ "Grüne",
    vote == 5 ~ "FDP",
    vote == 6 ~ "AfD",
    vote == 7 ~ "Other",
    vote == 8 ~ "None of the above",
    vote == 9 ~ "Did not vote",
    vote == 10 ~ "Not allowed to vote",
    vote == 11 ~ "Don't remember",
  )) |> 
  
  # region
  mutate(geo = case_when(
    geo == 1 ~ "Baden-Württemberg",
    geo == 2 ~ "Bayern",
    geo == 3 ~ "Berlin",
    geo == 4 ~ "Brandenbrug",
    geo == 5 ~ "Bremen",
    geo == 6 ~ "Hamburg",
    geo == 7 ~ "Hessen",
    geo == 8 ~ "Mecklenburg-Vorpommern",
    geo == 9 ~ "Niedersachsen",
    geo == 10 ~ "Nordrhein-Westfalen",
    geo == 11 ~ "Rheinland-Pfalz",
    geo == 12 ~ "Saarland",
    geo == 13 ~ "Sachsen",
    geo == 14 ~ "Sachsen-Anhalt",
    geo == 15 ~ "Schleswig-Holstein",
    geo == 16 ~ "Thüringen ",
    geo == 99 ~ "Ich lebe nicht in Deutschland",
  )) |> 
  
  # employment
  rename(
    emp_school = Q13.emp.status_1,
    emp_student = Q13.emp.status_2,
    emp_self = Q13.emp.status_3,
    emp_full = Q13.emp.status_4,
    emp_part = Q13.emp.status_5,
    emp_diff = Q13.emp.status_6,
    emp_parent = Q13.emp.status_7,
    emp_pension = Q13.emp.status_8,
    emp_unemp = Q13.emp.status_9,
    emp_diff2 = Q13.emp.status_10,
    emp_opn = Q13.emp.status_10_TEXT
  )

val_labels(df_de) <- NULL
rm(df_raw_de)

### Clean FR -------------------------------------------------------------------
df_raw_fr <- read_sav("data/FR/FROZ23-07_Veni_v1_toshare.sav")

df_fr <- df_raw_fr |> 
  dplyr::select(
    
    # experimental conditions
    treatment_group,
    action,
    demdef,
    justification,
    
    # variables of interest
    dv_eval,
    dv_eval,
    dv_ambi,
    dv_cred,
    post_agree,
    starts_with("dv_protest"),
    
    # covariates for balancing
    pol_interest,
    rile,
    starts_with("dem_eval"),
    starts_with("pol_trust"),
    starts_with("post_libdem"),
    post_dem_satis,
    post_dem_sup,
    byear,
    sex_rec,
    edu_3_rec,
    vote,
    geo,
    starts_with("Q13.emp.status"),
    Q38.income.category,
    
    # weights
    weight,
    
    # attention and manipulation
    starts_with("attention"),
    starts_with("dj_mc"),
    
    # meta info
    StartDate, EndDate, Duration__in_seconds_, RecordedDate,
    ResponseId) |> 
  
  # remove all variables about survey flow
  dplyr::select(-matches("_DO_")) |> 
  
  # add country identifier for merging
  mutate(country = "FR") |> 
  
  # rename variables for merging
  rename(sex = sex_rec,
         edu = edu_3_rec,
         income_fr = Q38.income.category) |> 
  
  # add labels to relevant variables
  # vote recall
  mutate(vote = case_when(
    vote == 1 ~ "Macron",
    vote == 2 ~ "Le Pen",
    vote == 3 ~ "Mélenchon",
    vote == 4 ~ "Zemmour",
    vote == 5 ~ "Pécresse",
    vote == 6 ~ "Jadot",
    vote == 7 ~ "Lassale",
    vote == 8 ~ "Roussel",
    vote == 9 ~ "Dupont-Aignan",
    vote == 10 ~ "Hidalgo",
    vote == 11 ~ "Poutou",
    vote == 12 ~ "Arthaud",
    vote == 13 ~ "Other",
    vote == 14 ~ "None of the above",
    vote == 15 ~ "Did not vote",
    vote == 16 ~ "Not allowed to vote",
    vote == 17 ~ "Don't remember",
  )) |> 
  
  # region
  mutate(geo = case_when(
    geo == 1 ~ "Auvergne-Rhône-Alpes",
    geo == 2 ~ "Bourgogne-France-Comté",
    geo == 3 ~ "Bretagne",
    geo == 4 ~ "Centre-Val de Loire",
    geo == 5 ~ "Corse",
    geo == 6 ~ "Grand Est",
    geo == 7 ~ "Hauts-de-France",
    geo == 8 ~ "Île-de-France",
    geo == 9 ~ "Normandie",
    geo == 10 ~ "Nouvelle-Aquitaine",
    geo == 11 ~ "Occitanie",
    geo == 12 ~ "Pays de la Loire",
    geo == 13 ~ "Provence-Alpes-Côte-d'Azur",
    geo == 14 ~ "Guadeloupe",
    geo == 15 ~ "Guyane",
    geo == 16 ~ "Martinique",
    geo == 17 ~ "La Réunion",
    geo == 18 ~ "Mayotte",
    geo == 19 ~ "Je n'habite ni en France métropolitaine ni dans les territoires d'outre-mer",
  )) |> 
  
  # employment
  rename(
    emp_school = Q13.emp.status_1,
    emp_student = Q13.emp.status_2,
    emp_self = Q13.emp.status_3,
    emp_full = Q13.emp.status_4,
    emp_part = Q13.emp.status_5,
    emp_diff = Q13.emp.status_6,
    emp_parent = Q13.emp.status_7,
    emp_pension = Q13.emp.status_8,
    emp_unemp = Q13.emp.status_9,
    emp_diff2 = Q13.emp.status_10,
    emp_opn = Q13.emp.status_10_TEXT
  )

val_labels(df_fr) <- NULL
rm(df_raw_fr)

##### Merging ------------------------------------------------------------------
demojudges <- bind_rows(df_nl, df_fr, df_de) |> 
  
  # add substantive labels
  mutate(
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
      treatment_group == 1 ~ "judiciary",
      treatment_group == 2 ~ "judiciary",
      treatment_group == 3 ~ "judiciary",
      treatment_group == 4 ~ "judiciary",
      treatment_group == 5 ~ "judiciary",
      treatment_group == 6 ~ "judiciary",
      treatment_group == 7 ~ "media",
      treatment_group == 8 ~ "media",
      treatment_group == 9 ~ "media",
      treatment_group == 10 ~ "media",
      treatment_group == 11 ~ "media",
      treatment_group == 12 ~ "media"
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
    demdef = as.factor(demdef)
  ) |> 
  
  # create protest battery
  mutate(dv_protest_vote = -(dv_protest_vote - 1)) |> # reverse coding because of question-phrasing
  mutate(dv_protest = dv_protest_vote + dv_protest_poster + dv_protest_pers +
           dv_protest_peti + dv_protest_lawpr + dv_protest_cont + dv_protest_unlaw,
         dv_protest_scaled = dv_protest / 7) |> 
  
  # create manipulation battery
  mutate(
    mc_action = case_when(
      action == "media" & dj_mc_1 == "1" ~ 1,
      action == "judiciary" & dj_mc_1 == "2" ~ 1,
      TRUE ~ 0),
    mc_justification = case_when(
      justification == "corruption" & dj_mc_2 == "1" ~ 1,
      justification == "self-serving"& dj_mc_2 == "2" ~ 1,
      justification == "none" & dj_mc_2 == "3" ~ 1,
      TRUE ~ 0),
    mc_demdef = case_when(
      demdef == "yes" & dj_mc_3 == "2" ~ 1,
      demdef == "no" & dj_mc_3 == "1" ~ 1,
      TRUE ~ 0)
  ) |> 
  
  # create attention and manipulation batteries
  mutate(attention = attention_4 + attention_6,
         attention = case_when(
           is.na(attention) ~ 0,
           TRUE ~ attention
         )) |> 
  mutate(manipulation = mc_action + mc_justification + mc_demdef) |> 
  mutate(attman = attention + manipulation) |> 
  
  # remove attention and manipulation base_variables
  dplyr::select(-attention_1, -attention_2, -attention_3, -attention_4, -attention_5, -attention_6, -attention_7, -attention_8) |> 
  #dplyr::select(-dj_mc_1, -dj_mc_2, -dj_mc_3) |> 
  
  # reverse credibility coding to match intuition: higher scores mean more credibility
  mutate(
    dv_cred = case_when(
      dv_cred == 1 ~ 6,
      dv_cred == 2 ~ 5,
      dv_cred == 3 ~ 4,
      dv_cred == 4 ~ 3,
      dv_cred == 5 ~ 2,
      dv_cred == 6 ~ 1)) |> 
  
  # reverse ambiguity coding to match intuition: higher scores mean more ambiguity
  mutate(
    dv_ambi = case_when(
      dv_ambi == 1 ~ 6,
      dv_ambi == 2 ~ 5,
      dv_ambi == 3 ~ 4,
      dv_ambi == 4 ~ 3,
      dv_ambi == 5 ~ 2,
      dv_ambi == 6 ~ 1)) |> 
  
  # clean up employment-variables
  mutate(
    emp_school = case_when(
      is.na(emp_school) ~ 0,
      TRUE ~ emp_school),
    emp_student = case_when(
      is.na(emp_student) ~ 0,
      TRUE ~ emp_student),
    emp_self = case_when(
      is.na(emp_self) ~ 0,
      TRUE ~ emp_self),
    emp_full = case_when(
      is.na(emp_full) ~ 0,
      TRUE ~ emp_full),
    emp_part = case_when(
      is.na(emp_part) ~ 0,
      TRUE ~ emp_part),
    emp_diff = case_when(
      is.na(emp_diff) ~ 0,
      TRUE ~ emp_diff),
    emp_parent = case_when(
      is.na(emp_parent) ~ 0,
      TRUE ~ emp_parent),
    emp_pension = case_when(
      is.na(emp_pension) ~ 0,
      TRUE ~ emp_pension),
    emp_unemp = case_when(
      is.na(emp_unemp) ~ 0,
      TRUE ~ emp_unemp),
    emp_diff2 = case_when(
      is.na(emp_diff2) ~ 0,
      TRUE ~ emp_diff2)) |> 
  
  # clean up all the missing
  filter(treatment_group != "") |> 
  mutate(pol_interest = ifelse(pol_interest == 99, NA, pol_interest),
         rile = ifelse(rile == 99, NA, rile),
         post_libdem_frexp = ifelse(post_libdem_frexp == 99, NA, post_libdem_frexp),
         post_libdem_frassc = ifelse(post_libdem_frassc == 99, NA, post_libdem_frassc),
         post_libdem_unisuff = ifelse(post_libdem_unisuff == 99, NA, post_libdem_unisuff),
         post_libdem_frelect = ifelse(post_libdem_frelect == 99, NA, post_libdem_frelect),
         post_libdem_judcnstr = ifelse(post_libdem_judcnstr == 99, NA, post_libdem_judcnstr),
         post_libdem_eqlaw = ifelse(post_libdem_eqlaw == 99, NA, post_libdem_eqlaw),
         post_dem_satis = ifelse(post_dem_satis == 99, NA, post_dem_satis),
         post_dem_sup = ifelse(post_dem_sup == 99, NA, post_dem_sup))

##### Export -------------------------------------------------------------------
write.csv(demojudges,
          file = "data/demojudges.csv",
          row.names = FALSE)

# /./ END OF CODE /./ #