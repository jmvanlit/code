### Libraries ------------------------------------------------------------------
library(dplyr) # for pipelines
library(haven) # to read .sav
library(cobalt) # for the balance test
library(tibble) # to convert rownames to a variable
library(knitr)
library(kableExtra)

##### Data import --------------------------------------------------------------
demojudges <- read.csv("data/demojudges.csv")

##### Balance checks -----------------------------------------------------------
covs <-  demojudges |>
  rename(treat = treatment_group) |> 
  dplyr::select(
    # outcome
    treat,
    
    # potential confounders
    pol_trust_crt, pol_trust_gov, pol_trust_med, pol_trust_pol, pol_trust_par, 
    dem_eval_no,
    post_libdem_frexp, post_libdem_frassc, post_libdem_unisuff, post_libdem_frelect, post_libdem_judcnstr, post_libdem_eqlaw, 
    post_dem_satis, post_dem_sup,
    sex, edu, byear, 
    vote, geo,
    income_nl, income_fr, income_de,
    emp_school, emp_student, emp_self, emp_full, emp_part, emp_diff, emp_parent, emp_pension, emp_unemp, emp_diff2,
    
    # weights
    weight
  )

### Unweighted data ------------------------------------------------------------
covs_unweighted <- covs

balance_unweighted <- bal.tab(data = covs_unweighted,
                              thresholds = c(m = 0.05)[[1]],
                              x = treat ~ pol_trust_crt + pol_trust_gov + pol_trust_med + pol_trust_pol + pol_trust_par +
                                dem_eval_no +
                                post_libdem_frexp + post_libdem_frassc + post_libdem_unisuff + post_libdem_frelect + post_libdem_judcnstr + post_libdem_eqlaw + 
                                post_dem_satis + post_dem_sup + 
                                sex + edu + byear + 
                                income_nl + income_fr + income_de +
                                vote + geo +
                                emp_school + emp_student + emp_self + emp_full + emp_part+ emp_diff + emp_parent + emp_pension + emp_unemp + emp_diff2)

balance_unweighted <- balance_unweighted$Balance |> 
  rownames_to_column(var = "variable") |> 
  dplyr::select(variable, Corr.Un, R.Threshold.Un) |> 
  rename(correlation = Corr.Un,
         balance = R.Threshold.Un) |> 
  mutate(correlation = round(correlation, 3)) |> 
  arrange(desc(abs(correlation))) |> 
  top_n(n = 10,
        wt = abs(correlation))

# Table 
kable(balance_unweighted,
      escape = TRUE,
      booktabs = TRUE, 
      format = "latex",
      caption = "Top-10 most unbalanced covariates for the unweighted data",
      label = "bal_unw")

### Weighted data --------------------------------------------------------------
covs_weighted <- covs |> 
  filter(!is.na(weight))

balance_weighted <- bal.tab(data = covs_weighted,
                            weights = "weight",
                            thresholds = c(m = 0.05)[[1]],
                            x = treat ~ pol_trust_crt + pol_trust_gov + pol_trust_med + pol_trust_pol + pol_trust_par +
                              dem_eval_no +
                              post_libdem_frexp + post_libdem_frassc + post_libdem_unisuff + post_libdem_frelect + post_libdem_judcnstr + post_libdem_eqlaw + 
                              post_dem_satis + post_dem_sup + 
                              sex + edu + byear + 
                              income_nl + income_fr + income_de +
                              vote + geo +
                              emp_school + emp_student + emp_self + emp_full + emp_part+ emp_diff + emp_parent + emp_pension + emp_unemp + emp_diff2)

balance_weighted <- balance_weighted$Balance |> 
  rownames_to_column(var = "variable") |> 
  dplyr::select(variable, Corr.Adj, R.Threshold) |> 
  rename(correlation = Corr.Adj,
         balance = R.Threshold) |> 
  mutate(correlation = round(correlation, 3)) |> 
  arrange(desc(abs(correlation))) |> 
  top_n(n = 10,
        wt = abs(correlation))

# Table 
kable(balance_weighted, 
      booktabs = TRUE, 
      format = "latex",
      caption = "Top-10 most unbalanced covariates for the weighted data",
      label = "bal_w",
      escape = TRUE)
