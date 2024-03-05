##### Libraries ----------------------------------------------------------------
library(dplyr)
library(haven)
library(ggplot2)
library(ggtext)
library(broom)
library(forcats)
library(texreg)
library(mediation)
library(tibble)
library(kableExtra)
library(stringr)
library(tidyr)
library(lavaan)
library(lavaanPlot)
library(purrr)

##### Data import --------------------------------------------------------------
demojudges <- read.csv("data/demojudges.csv") |> 
  as_tibble()

##### Data preparation  --------------------------------------------------------
demojudges <- demojudges |> 
  
  # convert to factor
  mutate(
    justification = as.factor(justification),
    action = as.factor(action),
    demdef = as.factor(demdef),
    treatment_group = as.factor(treatment_group))

### Set reference categories
demojudges$justification <- relevel(demojudges$justification, ref = "none")
demojudges$action <- relevel(demojudges$action, ref = "media")
demojudges$demdef <- relevel(demojudges$demdef, ref = "no")

################################################################################
##### MAIN RESULTS           #####
##################################

##### Simple models: H1, H3a, H3b ----------------------------------------------
# Simple Model
# Model 1
sm.eval <- lm(data = demojudges,
              weights = weight,
              
              dv_eval ~ 
                # treatments
                justification + action + demdef +
                
                # unbalanced covariates
                post_libdem_frelect)

# summary(sm.eval) for the exact p.values presented in the text

##### Interaction effect H2 ---------------------------------------------------- 
# Interaction Model Credibility
# Model 2
im.si.eval <- lm(data = demojudges,
                 weights = weight,
                 
                 dv_eval ~ 
                   #treatments
                   justification + action + demdef + (action * demdef) +
                   
                   # unbalanced covariates
                   post_libdem_frelect)

# summary(im.si.eval) for the exact p.values presented in the text

##### Interaction effect H4 ----------------------------------------------------
# Interaction Model Ambiguity
# Model 3
im.jd.eval <- lm(data = demojudges,
                 weights = weight,
                 
                 dv_eval ~ 
                   #treatments
                   justification + action + demdef + (justification * demdef) +
                   
                   # unbalanced covariates
                   post_libdem_frelect)

# summary(im.jd.eval) for the exact p.values presented in the text

##### Threeway Interaction H5 --------------------------------------------------
# Model 4
threeway <- demojudges |> 
  mutate(
    group = as.factor(case_when(
      treatment_group == 2 | treatment_group == 8 ~ 1, # no democratic defence against corruption
      treatment_group == 6 | treatment_group == 12 ~ 2, # no democratic defence against none
      treatment_group == 4 | treatment_group == 10 ~ 3, # no democratic defence against self-serving
      treatment_group == 1 ~ 4, # self-interested democratic defence against corruption
      treatment_group == 5 ~ 5, # self-interested democratic defence against none
      treatment_group == 3 ~ 6, # self-interested democratic defence against self-serving
      treatment_group == 7 ~ 7, # not-self-interested democratic defence against corruption
      treatment_group == 11 ~ 8, # not-self-interested democratic defence against none
      treatment_group == 9 ~ 9))) # not-self-interested democratic defence againstself-serving

threeway$group <- relevel(threeway$group, ref = "1")

# fit coefficients based on the aggregated treatment groups  
model_threeway <- lm(data = threeway,
                     weights = weight,
                     
                     dv_eval ~
                       
                       # treatments
                       group +
                       
                       # unbalanced covariate
                       post_libdem_frelect)

# fitted means calculated below

##### Figures ------------------------------------------------------------------

### Figure 2: Main Analysis ----
# Data preparation
sm.eval.plot <- sm.eval |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(model = "Model 1")

im.si.eval.plot <- im.si.eval |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(model = "Model 2")

im.jd.eval.plot <- im.jd.eval |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(model = "Model 3")

fig2_data <- bind_rows(sm.eval.plot, im.si.eval.plot, im.jd.eval.plot) |> 
  mutate(
    order = case_when(
      term == "justificationself-serving" ~ 5,
      term == "justificationcorruption" ~ 4,
      term == "actionjudiciary" ~ 1,
      term == "demdefyes" ~ 2,
      term == "actionjudiciary:demdefyes" ~ 3,
      term == "justificationself-serving:demdefyes" ~ 7,
      term == "justificationcorruption:demdefyes" ~ 6),
    term = case_when(
      term == "justificationself-serving" ~ "*H3b*: **Self-serving hypothesis**  \nJustification: power",
      term == "justificationcorruption" ~ "*H3a*: **Positive valence hypothesis**  \nJustification: corruption",
      term == "actionjudiciary" ~ "Target: judiciary",
      term == "demdefyes" ~ "*H1*: **Democratic defence hypothesis**",
      term == "actionjudiciary:demdefyes" ~ "*H2*: **Credibility hypothesis**  \nTarget: judiciary  \nDemocratic defence: present",
      term == "justificationself-serving:demdefyes" ~ "*H4*: **Ambiguity hypothesis**  \nJustification: power  \nDemocratic defence: present",
      term == "justificationcorruption:demdefyes" ~ "*H4*: **Ambiguity hypothesis**  \nJustification: corruption  \nDemocratic defence: present"))
      
# plot parameters
fig2_legend <- c("Model 1" = "Model 1  \n**Without** interactions",
                 "Model 2" = "Model 2  \n**Credibility** interaction",
                 "Model 3" = "Model 3  \n**Ambiguity** interaction")
fig2_colours <- c("Model 1" = "#e68619",
                  "Model 2" = "#5151d3",
                  "Model 3" = "#26c0c7")

# plot figure 2 ...
fig2 <- 
 ggplot(data = fig2_data,
        aes(x = estimate,
            y = reorder(term, -order))) +
   
   # zero-line
   geom_vline(xintercept = 0,
              linetype = "dashed",
              colour = "darkgrey") +
   
   # point estimates
   geom_point(aes(colour = model,
                  shape = model),
              size = 3) +
   
   # confidence intervals
   geom_errorbarh(aes(xmin = conf.low,
                      xmax = conf.high,
                      colour = model),
                  height = 0,
                  linewidth = 0.6) +
   
   # faceting
   facet_grid(cols = vars(model),
              labeller = as_labeller(c("Model 1" = "Model 1  \n**No interactions**",
                                       "Model 2" = "Model 2  \n**Credibility interaction**",
                                       "Model 3" = "Model 3  \n**Ambiguity interaction**"))) +
   
   # theme
   theme_classic() +
   scale_colour_manual(values = fig2_colours,
                       labels = fig2_legend) +
   scale_shape_manual(values = c(15, 17, 19),
                      labels = fig2_legend) +
   scale_linetype_manual(values = c("dashed", "solid", "dotdash"),
                         labels = fig2_legend) +
   
   # labels and legend
   labs(x = NULL,
        y = NULL,
        title = NULL) +
   theme(axis.text.y = ggtext::element_markdown(),
         legend.text = ggtext::element_markdown(),
         legend.position = "none",
         strip.text.x = ggtext::element_markdown())

# ... and save!
ggsave(filename  = "figures/fig2.png",
       plot = fig2,
       width = 20,
       height = 14,
       dpi = 300,
       units = "cm")
 

### Figure 3: Three way Interaction ----
# data preparation
threeway.intercept <- coef(model_threeway)[1]

threeway.plot <- model_threeway |> 
  tidy() |> 
  filter(term != "post_libdem_frelect") |> 
  mutate(
    fitted_mean = case_when(
      term != "(Intercept)" ~ threeway.intercept + estimate,
      TRUE ~ estimate), 
    fitted_low = fitted_mean - 1.96 * std.error,
    fitted_high = fitted_mean + 1.96 * std.error,
    demdef = case_when(
      term == "(Intercept)" | term == "group2" | term == "group3"  ~ "no",
      term == "group4" | term == "group5" | term == "group6"  ~ "self-interested",
      term == "group7" | term == "group8" | term == "group9"  ~ "selfless"),
    justification = case_when(
      term == "(Intercept)" | term == "group4" | term == "group7"  ~ "Positively valenced\njustification",
      term == "group2" | term == "group5" | term == "group8"  ~ "No\njustification",
      term == "group3" | term == "group6" | term == "group9"  ~ "Self-serving\njustification"))

# plot parameters
tw.colours <- c("no" = "#5151d3",
                "self-interested" = "#e68619",
                "selfless" = "#26c0c7")

tw.legend <- c("no" = "**No** democratic  \ndefence",
               "self-interested" = "**Self-interested**  \ndemocratic defence",
               "selfless" = "**Selfless**  \ndemocratic defence")

# plot figure 3 ...
fig3 <- 
  ggplot(data = threeway.plot,
         aes(x = justification,
             y = fitted_mean)) +
  
  # errorbar
  geom_errorbar(aes(ymin = fitted_low,
                    ymax = fitted_high,
                    colour = demdef),
                linewidth = 0.6,
                width = 0,
                position = position_dodge(width = 0.5)) +
  
  # line
  geom_line(aes(group = demdef,
                colour = demdef),
            position = position_dodge(width = 0.5)) +
  
  # points
  geom_point(aes(fill = demdef,
                 shape = demdef,
                 colour = demdef),
             size = 3,
             position = position_dodge(width = 0.5)) +
  
  # theme
  theme_classic() +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"))+
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  scale_colour_manual(values = tw.colours,
                      labels = tw.legend) +
  scale_fill_manual(values = tw.colours,
                    labels = tw.legend) +
  scale_shape_manual(values = c(15, 17, 19),
                     labels = tw.legend) +
  scale_x_discrete(limits=c("Self-serving\njustification", "No\njustification", "Positively valenced\njustification"))

# ... and save
ggsave(filename  = "figures/fig3.png",
       plot = fig3,
       width = 18,
       height = 14,
       dpi = 300,
       units = "cm")

##### Tables: Main Analysis ----------------------------------------------------

# table B.1
table1_models <- list(sm.eval, im.si.eval, im.jd.eval)

texreg(table1_models,
       caption = "The effects of two competing claims on respondent's democracy evaluations",
       caption.above = TRUE,
       label = "tab:main")

# table B.2
texreg(model_threeway,
       caption = "Three way interaction between the justification and democratic defence",
       caption.above = TRUE,
       label = "tab:tw-main")

# fitted means for the three way interaction
threeway.table <- threeway.plot |> 
  mutate(
    stars = case_when(
      p.value < 0.05 ~ "*",
      p.value < 0.01 ~ "**",
      p.value < 0.001 ~ "***",
      TRUE ~ ""),
    fitted_mean = round(fitted_mean, 3),
    fitted_low = round(fitted_low, 3),
    fitted_high = round(fitted_high, 3),
    fitted_mean2 =aste0(fitted_mean, " [", fitted_low, "; ", fitted_high, "]", sep = "")) |> 
  dplyr::select(term, fitted_mean2)

# table B.3
kable(threeway.table,
      col.names = c("", "Fitted mean"),
      caption = "Three way interaction between the justification and democratic defence (fitted means)",
      label = "tw-fitted",
      format = "latex",
      booktabs = TRUE)

################################################################################
##### MEDIATION              #####
##################################

# data preparation
tbl_mediation <- demojudges |> 
  mutate(
    corr = case_when(
      justification == "corruption" ~ 1,
      TRUE ~ 0),
    self = case_when(
      justification == "self-serving" ~ 1,
      TRUE ~ 0),
    demdef = case_when(
      demdef == "yes" ~ 1,
      TRUE ~ 0),
    judiciary = case_when(
      action == "judiciary" ~ 1,
      TRUE ~ 0)) |> 
  
  filter(!is.na(weight))

##### Credibility --------------------------------------------------------------
# these models are run without demdef as dv_cred was only shown when demdef == 1

# data preparation
tbl_cred <- tbl_mediation |> 
  filter(!is.na(dv_cred))

mm_cred <- '
# outcome model
dv_eval ~ b1*judiciary + b2*corr + b3*self + b4*post_libdem_frelect + m1*dv_cred

# mediator model
dv_cred ~ a1*judiciary

# total effect
total := a1 + b1 + m1
'

fit_mm_cred <- sem(mm_cred, 
                   data = tbl_cred,
                   estimator = "WLSMV",
                   sampling.weights = "weight")

# table B.4, panel A
summary(fit_mm_cred,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

##### Ambiguity ----------------------------------------------------------------

# data preparation
tbl_ambi <- tbl_mediation |> 
  filter(!is.na(dv_ambi))

### Positively valenced justification increases ambiguity ----------------------
mm_ambi_corr <- '
# outcome model
dv_eval ~ b1*corr + b2*demdef + b3*judiciary + b4*post_libdem_frelect + m1*dv_ambi

# mediator model
dv_ambi ~ a1*corr

# total effect
total := a1 + b1 + m1 
'

fit_mm_ambi_corr <- sem(mm_ambi_corr, 
                        data = tbl_ambi,
                        estimator = "WLSMV",
                        sampling.weights = "weight")

# table B.4, panel B
summary(fit_mm_ambi_corr,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

### Self-serving justification decreases ambiguity -----------------------------
mm_ambi_self <- '
# outcome model
dv_eval ~ b1*self + b2*demdef + b3*judiciary + b4*post_libdem_frelect + m1*dv_ambi

# mediator model
dv_ambi ~ a1*self

# total effect
total := a1 + b1 + m1
'

fit_mm_ambi_self <- sem(mm_ambi_self, 
                        data = tbl_ambi,
                        estimator = "WLSMV",
                        sampling.weights = "weight")
# table B.4, panel C
summary(fit_mm_ambi_self,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

### Figure 4: Mediation --------------------------------------------------------
# Figure 4 in the paper is made with an external illustrator program 
# Its elements can be replicated within R with the following code

# Panel A
lavaanPlot(model = fit_mm_cred, 
           coefs = TRUE,
           sig = 0.05,
           stars = "regress")

# Panel B
lavaanPlot(model = fit_mm_ambi_corr, 
           coefs = TRUE,
           sig = 0.05,
           stars = "regress")

# Panel C
lavaanPlot(model = fit_mm_ambi_self, 
           coefs = TRUE,
           sig = 0.05,
           stars = "regress")

################################################################################
##### PARTICIPATION          #####
##################################

# define new function to run multiple lm() for all protest items and the sum-index
run_multiple_lm <- function(dv){
  lm_formula <- as.formula(paste(dv, "~ action + demdef + justification + post_libdem_frelect"))
  model <- lm(lm_formula, data = demojudges, weights = weight)
  result <- tidy(model, conf.int = TRUE)
  result$dv <- dv
  return(result)
}

### some overall parameters
# define all dvs
protest.dvs <- c("dv_protest", "dv_protest_scaled",
                 "dv_protest_vote", "dv_protest_poster", "dv_protest_pers",
                 "dv_protest_peti", "dv_protest_lawpr", "dv_protest_cont", "dv_protest_unlaw")

# plot parameters
protest.colours <- c("dv_protest" = "#d83790", 
                     "dv_protest_vote" = "#00577C", 
                     "dv_protest_poster" = "#4D8F8D", 
                     "dv_protest_pers" = "#4C716E",
                     "dv_protest_peti" = "#6884C1",
                     "dv_protest_lawpr" = "#719FCE", 
                     "dv_protest_cont" = "#3A3D7E", 
                     "dv_protest_unlaw" = "#586174")

protest.legend <- c("dv_protest" = "**Full participation battery**", 
                    "dv_protest_vote" = "Vote", 
                    "dv_protest_poster" = "Poster", 
                    "dv_protest_pers" = "Persuade",
                    "dv_protest_peti" = "Petition",
                    "dv_protest_lawpr" = "Lawful protest", 
                    "dv_protest_cont" = "Contact", 
                    "dv_protest_unlaw" = "Unlawful protest")

### simple model
sm.protest <- map_df(protest.dvs, run_multiple_lm) |> 
  mutate(model = "simple") |> 
  mutate(dv = factor(dv, levels = c("dv_protest", "dv_protest_scaled", "dv_protest_vote", "dv_protest_poster", "dv_protest_pers",
                                    "dv_protest_peti", "dv_protest_lawpr", "dv_protest_cont", "dv_protest_unlaw")))

#### Table: Participation DVs ----
protest.table <- sm.protest |> 
  mutate(sig = case_when(p.value < 0.05 ~ "*",
                         p.value < 0.01 ~ "**",
                         p.value < 0.001 ~ "***",
                         TRUE ~ ""),
         stat = str_c(round(estimate, 2), " (", round(std.error,2 ), ")", sig)) |> 
  filter(dv != "dv_protest_scaled") |> 
  dplyr::select(term, dv, stat) |> 
  pivot_wider(names_from = dv, values_from = stat) |> 
  t()

# Table B.5
kable(protest.table, 
      booktabs = TRUE, 
      format = "latex",
      caption = "Does Democratic Defence Result in Political Participation?",
      label = "participation",
      escape = TRUE)

### Figure 5: Participation DVs ----
# prepare data
sm.protest.plot <- sm.protest |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect",
         dv != "dv_protest_scaled") |> 
  dplyr::mutate(term = case_when(
    term == "justificationself-serving" ~ "*H3b*: **Self-serving hypothesis**  \nJustification: power",
    term == "justificationcorruption" ~ "*H3a*: **Positive valence hypothesis**  \nJustification: corruption",
    term == "actionjudiciary" ~ "Target: judiciary",
    term == "demdefyes" ~ "*H1*: **Democratic defence hypothesis**"))

# plot figure 5 ...
fig5 <- 
  ggplot(data = sm.protest.plot,
         aes(x = estimate,
             y = reorder(term, desc(term)))) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # points, errorbars
  geom_point(aes(colour = dv,
                 shape = dv),
             size = 3,
             position = position_dodge(width = .5)) +
  
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = dv),
                 position = position_dodge(width = .5),
                 height = 0,
                 linewidth = 0.6) +
  
  # theme
  theme_classic() +
  scale_colour_manual(values = protest.colours,
                      labels = protest.legend) +
  scale_shape_manual(values = c(15, 1, 2, 5, 6, 7, 9, 13),
                     labels = protest.legend) +
  
  # labels and legends
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.justification = c(1,0))

# ... and save!
ggsave(filename  = "figures/fig5.png",
       plot = fig5,
       width = 20,
       height = 14,
       dpi = 300,
       units = "cm")

################################################################################
##### HTEs                       #####
######################################

##### Country effects ----------------------------------------------------------

### Netherlands
netherlands <- demojudges |> 
  filter(country == "NL")

france <- demojudges |> 
  filter(country == "FR")

germany <- demojudges |> 
  filter(country == "DE")

# H1, H3a, H3b
sm.eval.nl <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                 data = netherlands,
                 weights = weight) 

# summary(sm.eval.nl) # for exact p-values in text

sm.eval.fr <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                 data = france,
                 weights = weight) 

summary(sm.eval.fr) # for exact p-values in text

sm.eval.de <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                 data = germany,
                 weights = weight) 

# summary(sm.eval.de) # for exact p-values in text

models_eval_splitcntry <- list(sm.eval.nl, sm.eval.fr, sm.eval.de)

# table B.6
texreg(models_eval_splitcntry,
       caption = "Does democratic defence matter in different countries?",
       caption.above = TRUE,
       label = "tab:countries",
       custom.model.names = c("Netherlands", "France", "Germany"))

### Figure 6: Country effects ----
# data preparation
nl.plot <- sm.eval.nl |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(country = "Netherlands")

de.plot <- sm.eval.de |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(country = "Germany")

fr.plot <- sm.eval.fr |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(country = "France")

# number of observations
fr.n <- nobs(sm.eval.fr)
de.n <- nobs(sm.eval.de)
nl.n <- nobs(sm.eval.nl)

country.plot <- bind_rows(nl.plot, de.plot, fr.plot) |> 
  mutate(
    term = case_when(
      term == "justificationself-serving" ~ "*H3b*: **Self-serving hypothesis**  \nJustification: power",
      term == "justificationcorruption" ~ "*H3a*: **Positive valence hypothesis**  \nJustification: corruption",
      term == "actionjudiciary" ~ "Target: judiciary",
      term == "demdefyes" ~ "*H1*: **Democratic defence hypothesis**"),
    country2 = case_when(
      country == "France" ~ paste0(country, "  \n*n = ", fr.n, "*"),
      country == "Germany" ~ paste0(country, "  \n*n = ", de.n, "*"),
      country == "Netherlands" ~ paste0(country, "  \n*n = ", nl.n, "*")))

# plot parameters
country.colours <- c("France" = "#5151d3",
                     "Netherlands" = "#e68619",
                     "Germany" = "#26c0c7")

# plot figure 6...
fig6 <- 
  ggplot(data = country.plot,
         aes(x = estimate,
             y = reorder(term, desc(term)))) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # points and confidence intervals
  geom_point(aes(colour = country,
                 shape = country),
             size = 3) +
  
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = country),
                 height = 0,
                 linewidth = 0.6) +
  
  # faceting
  facet_grid(cols = vars(country2),
             scales = "free",
             space = "free_y") +
  
  # theme
  theme_classic() +
  scale_colour_manual(values = country.colours) +
  scale_shape_manual(values = c(15, 17, 19)) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text.x = ggtext::element_markdown()) +
  
  # labels and legend
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  guides(color = guide_legend(reverse = TRUE),
         shape = guide_legend(reverse = TRUE),
         linetype = guide_legend(reverse = TRUE))

# ... and save!
ggsave(filename  = "figures/fig6.png",
       plot = fig6,
       width = 20,
       height = 14,
       dpi = 300,
       units = "cm")

##### Party effects ------------------------------------------------------------

fr_parties_and_candidates <- tibble(
  candidate = c("Pécresse", "Le Pen", "Dupont-Aignan", "Roussel", "Jadot", 
                "Zemmour", "Macron", "Mélenchon", "Hidalgo", "Poutou"), # Lassalle is excluded as there are no observations
  party = c("LR", "RN", "DLF", "PCF", "EELV", 
            "R!", "RE (LREM)", "LFI", "PS", "NPA"))

poppa <- readRDS("data/poppa_integrated.rds") |> 
  filter(country %in% c("France", "Germany", "Netherlands")) |> 
  filter(wave == "Wave 2 - 2023") |> 
  dplyr::select(country, party_short, party_name_original, lroverall) |> 
  mutate(
    
    # create left-right categories
    LR = case_when(
      lroverall < 2.5 ~ "far-left",
      lroverall >= 2.5 & lroverall < 5 ~ "centre-left",
      lroverall == 5 ~ "centre", # does not exist, but added for completeness
      lroverall > 5 & lroverall <= 7.5 ~ "centre-right",
      lroverall > 7.5 ~ "far-right"),
    
    # change names so they match with demojudges
    party_short = case_when(
      party_short == "50+" ~ "50PLUS",
      party_short == "CDU" ~ "CDU/CSU",
      party_short == "CSU" ~ "CDU/CSU",
      party_short == "B90/Grüne" ~ "Grüne",
      party_short == "Die Linke" ~ "Linke",
      TRUE ~ party_short)) |> 
  
  # manually add Arthaud / LO
  add_row(country = "France",
          party_short = "LO",
          party_name_original = "Lutte Ouvrière",
          lroverall = NA,
          LR = "far-left") |> 
  
  # remove otherwise the left_join() below results in errors
  # both CSU and CDU are classed as centre-right
  #so does not matter for analysis which on the remove
  filter(party_name_original != "Christlich-Soziale Union in Bayern") 

parties <- demojudges |> 
  left_join(fr_parties_and_candidates,
            by = c("vote" = "candidate")) |> 
  mutate(vote = case_when(
    country == "FR" ~ party,
    TRUE ~ vote)) |> 
  left_join(poppa,
            by = c("vote" = "party_short"))

far_left <- parties |> 
  filter(LR == "far-left")

centre_left <- parties |> 
  filter(LR == "centre-left")

centre_right <- parties |> 
  filter(LR == "centre-right")

far_right <- parties |> 
  filter(LR == "far-right")

# H1, H3a, H3b
fl <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
         data = far_left,
         weights = weight) 

summary(fl) # for exact p-values in the text

cl <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
         data = centre_left,
         weights = weight) 

summary(cl) # for exact p-values in the text

cr <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
         data = centre_right,
         weights = weight) 

# summary(cr) # for exact p-values in the text

fr <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
         data = far_right,
         weights = weight) 

summary(fr) # for exact p-values in the text

models_eval_parties <- list(fl, cl, cr, fr)

# table B.7
texreg(models_eval_parties,
       caption = "Does democratic defence matter for different party-affiliations?",
       caption.above = TRUE,
       label = "tab:parties",
       custom.model.names = c("Far-left", "Centre-left", "Centre-right", "Far-right"))

### Figure 7: Party effects ----
# data preparation
fl.plot <- fl |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(type = "Far-left")

cl.plot <- cl |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(type = "Centre-left")

cr.plot <- cr |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(type = "Centre-right")

fright.plot <- fr |> # different name, because there is already a fr.plot for the country-split samples
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(type = "Far-right")

# number of observations
fl.n <- nobs(fl)
cl.n <- nobs(cl)
cr.n <- nobs(cr)
frright.n <- nobs(fr)

parties.plot <- bind_rows(fl.plot, cl.plot, cr.plot, fright.plot) |> 
  mutate(
    term = case_when(
      term == "justificationself-serving" ~ "H3b: **Self-serving hypothesis**  \n*Reference: no justification*",
      term == "justificationcorruption" ~ "H3a: **Positive valence hypothesis**  \n*Reference: no justification*",
      term == "actionjudiciary" ~ "Targeting the judiciary  \n*Reference: targeting the media*",
      term == "demdefyes" ~ "H1: **Democratic defence hypothesis**  \n*Reference: no democratic defence*"),
    type2 = case_when(
      type == "Far-left" ~ paste0(type, "  \n*n = ", fl.n, "*"),
      type == "Centre-left" ~ paste0(type, "  \n*n = ", cl.n, "*"),
      type == "Centre-right" ~ paste0(type, "  \n*n = ", cr.n, "*"),
      type == "Far-right" ~ paste0(type, "  \n*n = ", frright.n, "*")))

# plot parameters
party.colours <- c("Far-left" = "#4D8F8D",
                   "Centre-left" = "#4C716E",
                   "Centre-right" = "#6884C1",
                   "Far-right" =  "#719FCE")

# set order
parties.plot$type2 <- factor(parties.plot$type2, 
                             levels = c(paste0("Far-left", "  \n*n = ", fl.n, "*"), 
                                        paste0("Centre-left", "  \n*n = ", cl.n, "*"),
                                        paste0("Centre-right", "  \n*n = ", cr.n, "*"),
                                        paste0("Far-right", "  \n*n = ", frright.n, "*")))

# plot figure 7...
fig7 <- 
  ggplot(data = parties.plot,
         aes(x = estimate,
             y = reorder(term, desc(term)))) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # points and confidence intervals
  geom_point(aes(colour = type,
                 shape = type),
             size = 3) +
  
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = type),
                 height = 0,
                 linewidth = 0.6) +
  
  # faceting
  facet_grid(cols = vars(type2),
             scales = "free",
             space = "free_y") +
  
  # theme
  theme_classic() +
  scale_colour_manual(values = party.colours) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text.x = ggtext::element_markdown()) +
  
  # labels and legend
  labs(x = NULL,
       y = NULL,
       title = NULL)

# ... and save!
ggsave(filename  = "figures/fig7.png",
       plot = fig7,
       width = 20,
       height = 14,
       dpi = 300,
       units = "cm")

##### LiRe Self-evaluation effects ---------------------------------------------

left <- demojudges |> 
  filter(rile <= 3)

centre <- demojudges |> 
  filter(rile >= 4 & rile <= 6)

right <- demojudges |> 
  filter(rile >= 7)

# split samples
sm.left <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
              data = left,
              weights = weight) 

summary(sm.left) # for exact p-values in the text

sm.centre <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                data = centre,
                weights = weight)

summary(sm.centre) # for exact p-values in the text

sm.right <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
               data = right,
               weights = weight) 

summary(sm.right) # for exact p-values in the text

models_rile <- list(sm.left, sm.centre, sm.right)

# table B.8
texreg(models_rile,
       caption = "Does democratic defence matter if we split samples according to left-right self-placement?",
       caption.above = TRUE,
       label = "tab:rile",
       custom.model.names = c("Left", "Centre", "Right"))

### Figure 8: LiRe effects
# data preparation
left.plot <- sm.left |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(type = "Left")

centre.plot <- sm.centre |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(type = "Centre")

right.plot <- sm.right |> 
  tidy(conf.int = TRUE) |> 
  filter(!term %in% c("(Intercept)", "post_libdem_frelect")) |> 
  mutate(type = "Right")

# number of observations
left.n <- nobs(sm.left)
centre.n <- nobs(sm.centre)
right.n <- nobs(sm.right)

rileplot <- bind_rows(left.plot, centre.plot, right.plot) |> 
  mutate(
    term = case_when(
      term == "justificationself-serving" ~ "H3b: **Self-serving hypothesis**  \n*Reference: no justification*",
      term == "justificationcorruption" ~ "H3a: **Positive valence hypothesis**  \n*Reference: no justification*",
      term == "actionjudiciary" ~ "Targeting the judiciary  \n*Reference: targeting the media*",
      term == "demdefyes" ~ "H1: **Democratic defence hypothesis**  \n*Reference: no democratic defence*"),
    type2 = case_when(
      type == "Left" ~ paste0(type, "  \n*n = ", left.n, "*"),
      type == "Centre" ~ paste0(type, "  \n*n = ", centre.n, "*"),
      type == "Right" ~ paste0(type, "  \n*n = ", right.n, "*")))

# plot parameters
rile.colours <- c("Left" = "#FF5154",
                  "Centre" = "#84DCCF",
                  "Right" = "#2A1E5C")

# set order
rile.plot$type2 <- factor(lire.plot$type2, 
                          levels = c(paste0("Left", "  \n*n = ", left.n, "*"), 
                                     paste0("Centre", "  \n*n = ", centre.n, "*"),
                                     paste0("Right", "  \n*n = ", right.n, "*")))

# plot figure 8 ...
fig8 <- 
  ggplot(data = rile.plot,
         aes(x = estimate,
             y = reorder(term, desc(term)))) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # points and confidence intervals
  geom_point(aes(colour = type,
                 shape = type),
             size = 3) +
  
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = type),
                 height = 0,
                 linewidth = 0.6) +
  
  # faceting
  facet_grid(cols = vars(type2),
             scales = "free",
             space = "free_y") +
  
  # theme
  theme_classic() +
  scale_colour_manual(values = rile.colours) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text.x = ggtext::element_markdown()) +
  
  # labels and legend
  labs(x = NULL,
       y = NULL,
       title = NULL)

# ... and save!
ggsave(filename  = "figures/fig8.png",
       plot = fig8,
       width = 20,
       height = 14,
       dpi = 300,
       units = "cm")

# /./ End of Code /./ #