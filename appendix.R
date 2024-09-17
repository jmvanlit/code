### Libraries ------------------------------------------------------------------
library(dplyr)
library(haven)
library(ggplot2)
library(ggtext)
library(broom)
library(forcats)
library(purrr)
library(texreg)
library(kableExtra)
library(tidyr)
library(stringr)
library(marginaleffects)

##### Data import --------------------------------------------------------------
demojudges_raw <- read.csv("data/demojudges.csv") |> 
  as_tibble()

##### Data preparation  --------------------------------------------------------
demojudges <- demojudges_raw |> 
  mutate(
    # Convert to factor
    justification = as.factor(justification),
    action = as.factor(action),
    demdef = as.factor(demdef),
    treatment_group = as.factor(treatment_group))

### Set reference categories
demojudges$justification <- relevel(demojudges$justification, ref = "none")
demojudges$action <- relevel(demojudges$action, ref = "media")
demojudges$demdef <- relevel(demojudges$demdef, ref = "no")
demojudges$treatment_group <- relevel(demojudges$treatment_group, ref = "1")

################################################################################
##### ON THE MAIN ANALYSIS   #####
##################################

##### Hypotheses H2 and H4 testing according to PAP ----------------------------
# Model 5
model5 <- lm(data = demojudges,
             weights = weight,
             
             dv_eval ~
             #treatments
             justification + action + demdef + (action * demdef) + (demdef * justification) +
             
             # unbalanced covariates
             post_libdem_frelect)

# summary(model5) for the exact p.values presented in the text

# Table B.9
texreg(model5,
       caption = "Model specification for H2 and H4 according to PAP",
       caption.above = TRUE,
       label = "tab:m5",
       custom.model.names = c("Model 5"))

##### Three way Interaction H5 according to PAP --------------------------------
# Model 6
model6 <- lm(data = demojudges,
                 weights = weight,
                 
                 dv_eval ~
                   
                   #treatments
                   justification + action + demdef +
                   (justification * demdef) + (justification * action) + (demdef * action) +
                   (justification * demdef * action) +
                   
                   # unbalanced covariates
                   post_libdem_frelect)

# Table B.10
texreg(model6,
       caption = "Full threeway interaction according to PAP",
       caption.above = TRUE,
       label = "tab:m6",
       custom.model.names = c("Model 6"))

# Marginal means
model6_mm <- avg_predictions(model6, by = c("action", "demdef", "justification")) |> 
  mutate(
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    label = paste0(estimate, " [", conf.low, "; ", conf.high, "]"),
    justification = case_when(
      justification == "none" ~ "**No**  \njustification",
      justification == "corruption" ~ "**Positively valenced**  \njustification",
      justification == "self-serving" ~ "**Self-serving**  \njustification"))

# Plot parameters
im.colours <- c("yes" = "#26c0c7",
                "no" = "#d83790")

im.legend <- c("yes" = "Democratic defence **present**",
               "no" = "Democratic defence **absent**")

# text.im <- data.frame(
#   label = c("Democratic defence can  \nbe **successful**", "There is a risk of **backlash**  \nagainst democratic defence"),
#   action = c("media", "judiciary"),
#   x = c(2.4, 0.8),
#   y = c(1.25, 1.85),
#   hjust = c(1, 0),
#   angle = c(0, 0)
# )

# arrows.im.media <-
#   tibble(
#     x = c(2.45),
#     xend = c(3.15),
#     y = c(1.25),
#     yend = c(1.4),
#     action = c("media")
#   )
# 
# arrows.im.judic <-
#   tibble(
#     x = c(1.8),
#     xend = c(3),
#     y = c(1.88),
#     yend = c(1.92),
#     action = c("judiciary")
  # )
 
# plot figure B.1 ...
ggplot(data = model6_mm,
         aes(x = justification,
             y = estimate)) +

  # errorbar
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high,
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

  # facets
  facet_wrap(~ action,
             labeller = as_labeller(c(media = "Autocratic action targeting **the media**",
                                      judiciary = "Autocratic action targeting **the judiciary**"))) +
  
  # geom_text(aes(label = round(estimate, 2)),
  #           size = 3,
  #           position = position_dodge(width = 0.5)) +

  # # annotations
  # geom_richtext(data = text.im,
  #               label.colour = "white",
  #               text.colour = "darkgrey",
  #               size = 3,
  #               aes(x = x,
  #                   y = y,
  #                   label = label,
  #                   hjust = hjust,
  #                   angle = angle)) +
  # 
  # geom_curve(data = arrows.im.media,
  #            aes(x = x,
  #                y = y,
  #                xend = xend,
  #                yend = yend),
  #            arrow = arrow(length = unit(0.2, "cm")),
  #            linewidth = 0.3,
  #            color = "darkgrey") +
  # 
  # geom_curve(data = arrows.im.judic,
  #            aes(x = x,
  #                y = y,
  #                xend = xend,
  #                yend = yend),
  #            arrow = arrow(length = unit(0.2, "cm")),
  #            linewidth = 0.3,
  #            color = "darkgrey",
  #            curvature = -0.5) +

  # theme
  theme_classic() +
  theme(axis.text.x = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        strip.text.x = ggtext::element_markdown())+
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  scale_colour_manual(values = im.colours,
                      labels = im.legend) +
  scale_fill_manual(values = im.colours,
                    labels = im.legend) +
  scale_shape_manual(values = c(22, 23),
                     labels = im.legend) +
  scale_x_discrete(limits=c("**Self-serving**  \njustification", 
                            "**No**  \njustification", 
                            "**Positively valenced**  \njustification"))

# ... and save
ggsave(filename  = "figures/figb.1.png",
       width = 18,
       height = 14,
       dpi = 300,
       units = "cm")

################################################################################
##### ON THE MEDIATION ANALYSIS   #####
#######################################

### Modifications checks for the mediation models ------------------------------

# Ambiguity: corruption
modificationindices(fit_mm_ambi_corr, 
                    standardized = TRUE, 
                    power = TRUE, 
                    delta = 0.1, 
                    alpha = 0.05, 
                    high.power = 0.75, 
                    sort. = TRUE)

mm_ambi_corr_mod <- '
# outcome model
dv_eval ~ b1*corr + b2*demdef + b3*action + b4*post_libdem_frelect + m1*dv_ambi

# mediator model
dv_ambi ~ a1*corr + a2*post_libdem_frelect
'

fit_mm_ambi_corr_mod <- sem(mm_ambi_corr_mod,
                            data = tbl_ambi,
                            estimator = "MLR",
                            sampling.weights = "weight")

summary(fit_mm_ambi_corr_mod,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

# Ambiguity: self-serving
modificationindices(fit_mm_ambi_self, 
                    standardized = TRUE, 
                    power = TRUE, 
                    delta = 0.1, 
                    alpha = 0.05, 
                    high.power = 0.75, 
                    sort. = TRUE)

mm_ambi_self_mod <- '
# outcome model
dv_eval ~ b1*self + b2*demdef + b3*action + b4*post_libdem_frelect + m1*dv_ambi

# mediator model
dv_ambi ~ a1*self + a2*post_libdem_frelect
'

fit_mm_ambi_corr_mod <- sem(mm_ambi_self_mod,
                            data = tbl_ambi,
                            estimator = "MLR",
                            sampling.weights = "weight")

summary(fit_mm_ambi_corr_mod,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

# Credibility
modificationindices(fit_mm_cred, 
                    standardized = TRUE, 
                    power = TRUE, 
                    delta = 0.1, 
                    alpha = 0.05, 
                    high.power = 0.75, 
                    sort. = TRUE)

fit_mm_cred_mod <- '
# outcome model
dv_eval ~ b1*selfinterest + b2*corr + b3*self + b4*post_libdem_frelect + m1*dv_cred

# mediator model
dv_cred ~ a1*selfinterest + a2*post_libdem_frelect
'

fit_mm_cred_corr_mod <- sem(fit_mm_cred_mod,
                            data = tbl_cred,
                            estimator = "MLR",
                            sampling.weights = "weight")

summary(fit_mm_cred_corr_mod,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

### Simple OLS instead of mediation --------------------------------------------

# Credibility as DV
model_cred <- lm(data = tbl_cred,
                 dv_cred ~ action + justification + post_libdem_frelect) 
# exclude demdef as question is only shown to respondents who saw a democratic

# Ambiguity as DV
model_ambi <- lm(data = tbl_ambi,
                 dv_ambi ~ action + justification + demdef + post_libdem_frelect)

# Table
texreg(list(model_cred, model_ambi),
       caption = "The effect of treatments on the mediators as dependent variable",
       custom.model.names = c("DV: Credibility", "DV: Ambiguity"),
       caption.above = TRUE,
       label = "tab:med_as_dv")

# Ambiguity and Credibility as IVs
cred_as_iv <- lm(data = tbl_cred,
                 dv_eval ~ dv_cred + action + justification + post_libdem_frelect)

ambi_as_iv <- lm(data = tbl_ambi,
                 dv_eval ~ dv_ambi + action + demdef + justification + post_libdem_frelect)

# Table
texreg(list(cred_as_iv, ambi_as_iv),
       caption = "The effect of mediators on democracy evaluation",
       custom.model.names = c("IV: Credibility", "IV: Ambiguity"),
       caption.above = TRUE,
       label = "tab:med_as_iv")

################################################################################
##### MANIPULATION AND ATTENTION   #####
########################################

### Counts ----
att <- demojudges |> 
  filter(!is.na(post_libdem_frelect)) |> 
  group_by(attention) |> 
  summarise(count = n()) |> 
  mutate(attention = case_when(
    attention == 0 ~ "Failed",
    attention == 1 ~ "1 correct",
    TRUE ~ "Passed"
  ))

man <- demojudges |> 
  filter(!is.na(post_libdem_frelect)) |> 
  group_by(manipulation) |> 
  summarise(count = n()) |> 
  mutate(manipulation = case_when(
    manipulation == 0 ~ "All incorrect",
    manipulation == 1 ~ "1 correct",
    manipulation == 2 ~ "2 correct",
    manipulation == 3 ~ "3 correct"
  ))

man.act <- demojudges |> 
  filter(!is.na(post_libdem_frelect)) |> 
  group_by(mc_action) |> 
  summarise(count = n()) |> 
  mutate(mc_action = case_when(
    mc_action == 0 ~ "Incorrect",
    TRUE ~ "Correct")) |> 
  rename("Manipulation Check" = mc_action,
         "Autocratic Action"= count)

man.dem <- demojudges |> 
  filter(!is.na(post_libdem_frelect)) |> 
  group_by(mc_demdef) |> 
  summarise(count = n()) |> 
  mutate(mc_demdef = case_when(
    mc_demdef == 0 ~ "Incorrect",
    TRUE ~ "Correct")) |> 
  rename("Manipulation Check" = mc_demdef,
         "Democratic Defence"= count)

man.jus <- demojudges |> 
  filter(!is.na(post_libdem_frelect)) |> 
  group_by(mc_justification) |> 
  summarise(count = n()) |> 
  mutate(mc_justification = case_when(
    mc_justification == 0 ~ "Incorrect",
    TRUE ~ "Correct")) |> 
  rename("Manipulation Check" = mc_justification,
         "Justification"= count)

man.spec <- man.act |> 
  left_join(man.dem) |> 
  left_join(man.jus)

# table B.14
kable(att,
      col.names = c("Attention check", "Count"),
      caption = "Attention Check",
      label = "attention",
      format = "latex",
      booktabs = TRUE)

# table B.15
kable(man,
      col.names = c("Manipulation check", "Count"),
      caption = "Manipulation Check",
      label = "manipulation",
      format = "latex",
      booktabs = TRUE)

# table B.16
kable(man.spec,
      caption = "Manipulation Checks per Treatment",
      label = "manipulation2",
      format = "latex",
      booktabs = TRUE)

### Manipulation checks as predictors ----
attention <- demojudges |> 
  filter(!is.na(post_libdem_frelect)) |> 
  # create specific manipulation items
  mutate(
    mc_corruption = case_when(
      justification =="corruption" & dj_mc_2 == "1" ~ 1,
      TRUE ~ 0),
    mc_selfserving = case_when(
      justification == "self-serving" & dj_mc_2 == "2" ~ 1,
      TRUE ~ 0),
    mc_none = case_when(
      justification == "none" & dj_mc_2 == "3" ~ 1,
      TRUE ~ 0),
    mc_yes = case_when(
      demdef == "yes" &  dj_mc_3 == "2" ~ 1,
      TRUE ~ 0),
    mc_judiciary = case_when(
      action == "judiciary" & dj_mc_1 == "2" ~ 1,
      TRUE ~ 0)) |> 
  
  # create integers from treatments
  mutate(
    action = case_when(
      action == "judiciary" ~ 1,
      action == "media" ~ 0),
    demdef = case_when(
      demdef == "yes" ~ 1,
      demdef == "no" ~ 0),
    corruption = case_when(
      justification == "corruption" ~ 1,
      TRUE ~ 0),
    self_serving = case_when(
      justification == "self-serving" ~ 1,
      TRUE ~ 0),
    none = case_when(
      justification == "none" ~ 1,
      TRUE ~ 0))

m.man.demdef <- lm(demdef ~ mc_yes,
                   data = attention,
                   weights = weight)

m.man.action <- lm(action ~ mc_judiciary,
                   data = attention,
                   weights = weight)

m.man.corruption <- lm(corruption ~ mc_corruption,
                       data = attention,
                       weights = weight)

m.man.selfserving <- lm(self_serving ~ mc_selfserving,
                        data = attention,
                        weights = weight)

m.man.none <- lm(none ~ mc_none,
                 data = attention,
                 weights = weight)

m.man <- list(m.man.demdef, m.man.action, m.man.corruption, m.man.selfserving, m.man.none)

# table B.17
texreg(m.man,
       caption = "Manipulation Checks",
       label = "tab:man",
       caption.above = TRUE,
       sideways = TRUE,
       custom.model.names = c("Democratic defence", "Action against judiciary", "Positively valenced justification", "Self-serving justification", "No justification"))

### Controlling for attention and manipulation ----
sm.att  <- lm(data = demojudges,
              weights = weight,
              
              dv_eval ~
                
                # attention
                as.factor(attention) +
                
                # treatments
                justification + action + demdef +
                
                # unbalanced covariates
                post_libdem_frelect)


sm.man <- lm(data = demojudges,
             weights = weight,
             
             dv_eval ~
               
               # manipulation
               as.factor(manipulation) +
               
               # treatments
               justification + action + demdef +
               
               # unbalanced covariates
               post_libdem_frelect)

attman.cntrl <- list(sm.att, sm.man)

# table B.18
texreg(attman.cntrl,
       caption = "Controlling for attention and manipulation",
       label = "tab:attman",
       caption.above = TRUE,
       custom.model.names = c("Attention", "Manipulation"))

### Split sample on attention ----
lo_att <- demojudges |> 
  filter(attention == 0)

hi_att <- demojudges |> 
  filter(attention == 2)

sm.lo_att <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                 data = lo_att,
                 weights = weight) 

sm.hi_att <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                 data = hi_att,
                 weights = weight) 

att.models <- list(sm.lo_att, sm.hi_att)

# table B.19
texreg(att.models,
       caption = "Does democratic defence matter if we split samples according to attention?",
       caption.above = TRUE,
       label = "tab:att.split",
       custom.model.names = c("No attention", "Full attention"))

### Split sample on manipulation ----
no_man <- demojudges |> 
  filter(manipulation == 0)

lo_man <- demojudges |> 
  filter(manipulation == 1)

med_man <- demojudges |> 
  filter(manipulation == 2)

hi_man <- demojudges |> 
  filter(manipulation == 3)

sm.no_man <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                data = no_man,
                weights = weight) 

sm.lo_man <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                 data = lo_man,
                 weights = weight) 

sm.med_man <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                 data = med_man,
                 weights = weight) 

sm.hi_man <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                  data = hi_man,
                  weights = weight) 

man.models <- list(sm.no_man, sm.lo_man, sm.med_man, sm.hi_man)

# table B. 20
texreg(man.models,
       caption = "Does democratic defence matter if we split samples according to manipulation?",
       caption.above = TRUE,
       label = "tab:man.split",
       custom.model.names = c("Failed all", "Passed 1", "Passed 2", "Passed all 3"))

#################################################################################
##### DESCRIPTIVES           #####
##################################

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

# table A.1
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

# Table C.1
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

# table C.2
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

# table C.3
kable(interval,
      col.names = c("Variable", "Min", "Max", "Median", "Mean", "SD", "N", "Missing"),
      caption = "Descriptives: covariates of interest",
      label = "desc-interval",
      format = "latex",
      booktabs = TRUE)

################################################################################
##### ALTERNATIVE OUTCOME    #####
##################################

##### Alternative dependent variable: dv_agree ---------------------------------
# Model 1
sm.agree <- lm(post_agree ~ justification + action + demdef + post_libdem_frelect,
               data = demojudges,
               weights = weight) 

# Model 2
im.agree.ad <- lm(post_agree ~ justification + action + demdef + (action * demdef) + post_libdem_frelect,
                  data = demojudges,
                  weights = weight)

# Model 3
im.agree.jd  <- lm(post_agree ~ justification + action + demdef + (justification * demdef) + post_libdem_frelect,
                   data = demojudges,
                   weights = weight)

# Model 6
im.agree.tw <- lm(post_agree ~ justification + action + demdef + 
                    (justification * demdef) + (action * demdef) + (justification * action) +
                    (justification * action * demdef) + post_libdem_frelect,
                  data = demojudges,
                  weights = weight)


models_agree <- list(sm.agree, im.agree.ad, im.agree.jd)

# table B.11
texreg(models_agree,
       caption = "Does democratic defence matter if we ask about agreement?",
       caption.above = TRUE,
       label = "tab:agree")

# table B.12
texreg(im.agree.tw,
       caption = "Does democratic defence matter if we ask about agreement?",
       caption.above = TRUE,
       label = "tab:agree.tw",
       custom.model.names = c("Model 6"))

### And what if we split the sample?
lo_agree <- demojudges |> 
  filter(post_agree <= 4)

hi_agree <- demojudges |> 
  filter(post_agree > 4)

sm.lo_agree <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                  data = lo_agree,
                  weights = weight)

summary(sm.lo_agree) # for exact p-values in the text

sm.hi_agree <- lm(dv_eval ~ justification + action + demdef + post_libdem_frelect,
                  data = hi_agree,
                  weights = weight)

summary(sm.hi_agree) # for exact p-values in the text

models_agree2 <- list(sm.lo_agree, sm.hi_agree)

# table B.13
texreg(models_agree2,
       caption = "Does democratic defence matter if we split samples according to agreement?",
       caption.above = TRUE,
       label = "tab:agree.split",
       custom.model.names = c("Low agreement", "High agreement"))

################################################################################
##### UNWEIGHTED DATA        #####
##################################

# as there are no unbalanced covariates in the unweighted data, post_libdem_frelect is not included

##### Main analysis ------------------------------------------------------------
sm.eval.unw <- lm(data = demojudges,
              
              dv_eval ~ 
                # treatments
                justification + action + demdef)
              
im.si.eval.unw <- lm(data = demojudges,
                 
                 dv_eval ~ 
                   #treatments
                   justification + action + demdef + (action * demdef))

im.jd.eval.unw <- lm(data = demojudges,
                 
                 dv_eval ~ 
                   #treatments
                   justification + action + demdef + (justification * demdef))

im.tw.eval.unw <- lm(data = demojudges,
                 
                 dv_eval ~ 
                   
                   #treatments
                   justification + action + demdef +
                   (justification * demdef) + (justification * action) + (demdef * action) +
                   (justification * demdef * action))

unw_models <- list(sm.eval.unw, im.si.eval.unw, im.jd.eval.unw, im.tw.eval.unw)

# table D.4
texreg(unw_models,
       caption = "Does Democratic Defence Matter (Unweighted Data)?",
       caption.above = TRUE,
       label = "tab:unwmodels",
       custom.model.names = c("Model 1", "Model 2", "Model 3", "Model 6"))

# Model 4
threeway_unw <- demojudges |> 
  mutate(
    treatment_group = as.factor(case_when(
      treatment_group == 2 | treatment_group == 8 ~ 1, # no democratic defence against corruption
      treatment_group == 6 | treatment_group == 12 ~ 2, # no democratic defence against none
      treatment_group == 4 | treatment_group == 10 ~ 3, # no democratic defence against self-serving
      treatment_group == 1 ~ 4, # self-interested democratic defence against corruption
      treatment_group == 5 ~ 5, # self-interested democratic defence against none
      treatment_group == 3 ~ 6, # self-interested democratic defence against self-serving
      treatment_group == 7 ~ 7, # not-self-interested democratic defence against corruption
      treatment_group == 11 ~ 8, # not-self-interested democratic defence against none
      treatment_group == 9 ~ 9))) # not-self-interested democratic defence againstself-serving

threeway_unw$treatment_group <- relevel(threeway_unw$treatment_group, ref = "1")

# fit coefficients based on the aggregated treatment groups
model_threeway_unw <- lm(data = threeway_unw,

                     dv_eval ~

                       # treatments
                       treatment_group)

# Marginal means
threeway_unw.table <- avg_predictions(model_threeway_unw, by = "treatment_group") |> 
  mutate(
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    demdef = case_when(
      treatment_group == "1" | treatment_group == "2" | treatment_group == "3"  ~ "no",
      treatment_group == "4" | treatment_group == "5" | treatment_group == "6"  ~ "self-interested",
      treatment_group == "7" | treatment_group == "8" | treatment_group == "9"  ~ "selfless"),
    justification = case_when(
      treatment_group == "1" | treatment_group == "4" | treatment_group == "7"  ~ "Positively valenced justification",
      treatment_group == "2" | treatment_group == "5" | treatment_group == "8"  ~ "No justification",
      treatment_group == "3" | treatment_group == "6" | treatment_group == "9"  ~ "Self-serving justification"),
    label = paste0(estimate, " [", conf.low, "; ", conf.high, "]")) |> 
  dplyr::select(demdef, justification, label)

# table D.5
kable(threeway_unw.table,
      col.names = c("", "", "Marginal mean [with 95% CIs]"),
      caption = "Three way interaction between the justification and democratic defence (Unweighted data)",
      label = "tw-appendix",
      format = "latex",
      booktabs = TRUE)

################################################################################
##### UNWEIGHTED MEDIATION   #####
##################################

# data preparation
tbl_mediation_unw <- demojudges |> 
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
      TRUE ~ 0))

##### Credibility --------------------------------------------------------------
# these models are run without demdef as dv_cred was only shown when demdef == 1

# data preparation
tbl_cred_unw <- tbl_mediation_unw |> 
  filter(!is.na(dv_cred))

mm_cred_unw <- '
# outcome model
dv_eval ~ b1*judiciary + b2*corr + b3*self + m1*dv_cred

# mediator model
dv_cred ~ a1*judiciary
'

fit_mm_cred_unw <- sem(mm_cred_unw, 
                       data = tbl_cred_unw,
                       estimator = "WLSMV")

# table D.6, Panel A
summary(fit_mm_cred_unw,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

lavaanPlot(model = fit_mm_cred_unw, 
           coefs = TRUE,
           sig = 0.05,
           stars = "regress")

# MLR estimator
fit_mm_cred_unw_mlr <- sem(mm_cred_unw, 
                           data = tbl_cred_unw,
                           estimator = "MLR")

# table D.7, Panel A
summary(fit_mm_cred_unw_mlr,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)


##### Ambiguity ----------------------------------------------------------------

# data preparation
tbl_ambi_unw <- tbl_mediation_unw |> 
  filter(!is.na(dv_ambi))

### Positively valenced justification increases ambiguity ----------------------
mm_ambi_corr_unw <- '
# outcome model
dv_eval ~ b1*corr + b2*demdef + b3*judiciary + m1*dv_ambi

# mediator model
dv_ambi ~ a1*corr
'

fit_mm_ambi_corr_unw <- sem(mm_ambi_corr_unw,
                            data = tbl_ambi_unw,
                            estimator = "WLSMV")

# table D.6, Panel B
summary(fit_mm_ambi_corr_unw,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

lavaanPlot(model = fit_mm_ambi_corr_unw, 
           coefs = TRUE,
           sig = 0.05,
           stars = "regress")

# MLR estimator
fit_mm_ambi_corr_unw_mlr <- sem(mm_ambi_corr_unw,
                                data = tbl_ambi_unw,
                                estimator = "MLR")

# table D.7, Panel B
summary(fit_mm_ambi_corr_unw_mlr,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

### Self-serving justification decreases ambiguity -----------------------------
mm_ambi_self_unw <- '
# outcome model
dv_eval ~ b1*self + b2*demdef + b3*judiciary + m1*dv_ambi

# mediator model
dv_ambi ~ a1*self
'

fit_mm_ambi_self_unw <- sem(mm_ambi_self_unw, 
                            data = tbl_ambi_unw,
                            estimator = "WLSMV")

# table D.6 Panel C
summary(fit_mm_ambi_self_unw,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

# MLR estimator
fit_mm_ambi_self_unw_mlr <- sem(mm_ambi_self_unw, 
                                data = tbl_ambi_unw,
                                estimator = "MLR")

# table D.7, Panel C
summary(fit_mm_ambi_self_unw_mlr,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

################################################################################
##### UNWEIGHTED PARTICIPATION #####
####################################

# define new function to run multiple lm() for all protest items and the sum-index
run_multiple_lm_unw <- function(dv){
  lm_formula <- as.formula(paste(dv, "~ action + demdef + justification"))
  model <- lm(lm_formula, data = demojudges)
  result <- tidy(model, conf.int = TRUE)
  result$dv <- dv
  return(result)
}

protest.dvs.unw <- c("dv_protest",
                 "dv_protest_vote", "dv_protest_poster", "dv_protest_pers",
                 "dv_protest_peti", "dv_protest_lawpr", "dv_protest_cont", "dv_protest_unlaw")

### simple model
sm.protest.unw <- map_df(protest.dvs.unw, run_multiple_lm_unw) |> 
  mutate(model = "simple") |> 
  mutate(dv = factor(dv, levels = c("dv_protest", "dv_protest_vote", "dv_protest_poster", "dv_protest_pers",
                                    "dv_protest_peti", "dv_protest_lawpr", "dv_protest_cont", "dv_protest_unlaw")))

protest.table.unw <- sm.protest.unw |> 
  mutate(sig = case_when(p.value < 0.05 ~ "*",
                         p.value < 0.01 ~ "**",
                         p.value < 0.001 ~ "***",
                         TRUE ~ ""),
         stat = str_c(round(estimate, 2), " (", round(std.error,2 ), ")", sig)) |> 
  dplyr::select(term, dv, stat) |> 
  pivot_wider(names_from = dv, values_from = stat) |> 
  t()

# table D.8
kable(protest.table.unw, 
      booktabs = TRUE, 
      format = "latex",
      caption = "Does Democratic Defence Result in Political Participation? (Unweighted Data)",
      label = "protest_unw",
      escape = TRUE)

# /./ End of Code /./
