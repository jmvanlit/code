##### Libraries ----------------------------------------------------------------
library(dplyr)
library(haven)
library(ggplot2)
library(ggtext)
library(broom)
library(forcats)
library(purrr)
library(texreg)
library(mediation)
library(tibble)
library(kableExtra)
library(stringr)
library(tidyr)
library(lavaan)
library(lavaanPlot)

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

##### Simple models: H1a, H1b, H2 ----------------------------------------------
# Simple Model
sm.eval <- lm(data = demojudges,
              weights = weight,
              
              dv_eval ~ 
                # treatments
                justification + action + demdef +
                
                # unbalanced covariates
                post_libdem_frelect)

##### Interaction effect H3 ---------------------------------------------------- 
# Interaction Model Self Interest
im.si.eval <- lm(data = demojudges,
                 weights = weight,
                 
                 dv_eval ~ 
                   #treatments
                   justification + action + demdef + (action * demdef) +
                   
                   # unbalanced covariates
                   post_libdem_frelect)

##### Interaction effect H4 ----------------------------------------------------
im.jd.eval <- lm(data = demojudges,
                 weights = weight,
                 
                 dv_eval ~ 
                   #treatments
                   justification + action + demdef + (justification * demdef) +
                   
                   # unbalanced covariates
                   post_libdem_frelect)

##### Threeway Interaction H5 --------------------------------------------------
im.tw.eval <- lm(data = demojudges,
                 weights = weight,
                 
                 dv_eval ~
                   
                   #treatments
                   justification + action + demdef +
                   (justification * demdef) + (justification * action) + (demdef * action) +
                   (justification * demdef * action) +
                   
                   # unbalanced covariates
                   post_libdem_frelect)

### Figure: Main Analysis ------------------------------------------------------

# Plot parameters
fig2_legend <- c("Model 3  \n**Ambiguity** interaction", "Model 2  \n**Credibility** interaction", "Model 1  \n**Without** interaction")
fig2_colours <- c("#26c0c7", "#5151d3", "#e68619")

# Data preparation
sm.eval.plot <- sm.eval |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(model = "simple")

im.si.eval.plot <- im.si.eval |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(model = "credibility")

im.jd.eval.plot <- im.jd.eval |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(model = "ambiguity")

fig2_data <- bind_rows(sm.eval.plot, im.si.eval.plot, im.jd.eval.plot) |> 
  mutate(term = case_when(
    term == "justificationself-serving" ~ "H1a: **Self-serving hypothesis**  \nJustification: self-serving  \n*Reference: no justification*",
    term == "justificationcorruption" ~ "H1b: **Positive valence hypothesis**  \n*Reference: no justification*",
    term == "actionjudiciary" ~ "Targeting the judiciary  \n*Reference: targeting the media*",
    term == "demdefyes" ~ "H2: **Democratic defence hypothesis**  \n*Reference: no democratic defence*",
    term == "actionjudiciary:demdefyes" ~ "H3: **Credibility hypothesis**  \nDemocratic defence * Action against judiciary",
    term == "justificationself-serving:demdefyes" ~ "H4: **Ambiguity hypothesis**  \nSelf-serving justification * Democratic defence  \n*Reference: no justification*",
    term == "justificationcorruption:demdefyes" ~ "H4: **Ambiguity hypothesis**  \nCorruption justification * Democratic defence  \n*Reference: no justification*"
  ))

# create plot
fig2 <- 
  ggplot(data = fig2_data,
         aes(x = estimate,
             y = reorder(term, desc(term)))) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # points and confidence intervals
  geom_point(aes(colour = model,
                 shape = model),
             position = position_dodge(width = .5),
             size = 3) +
  
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = model,
                     linetype = model),
                 position = position_dodge(width = .5),
                 height = 0,
                 linewidth = 0.6) +

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
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.justification = c(1,0)) +
  guides(color = guide_legend(reverse = TRUE),
         shape = guide_legend(reverse = TRUE),
         linetype = guide_legend(reverse = TRUE))

# ... and save!
ggsave(filename  = "figures/fig2.png",
       plot = fig2,
       width = 18,
       height = 14,
       dpi = 300,
       units = "cm")

##### Table: Main Analysis -----------------------------------------------------
main_models <- list(sm.eval, im.si.eval, im.jd.eval, im.tw.eval)

texreg(main_models,
       caption = "Does Democratic Defence Matter?",
       caption.above = TRUE,
       label = "tab:mainmodels")

### Figure 3: Three way Interaction --------------------------------------------
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
    treatment_group == 9 ~ 9))) |>  # not-self-interested democratic defence againstself-serving
  dplyr::select(dv_eval, group, weight, post_libdem_frelect)

# fit means based on the aggregated treatment groups  
model_threeway <- lm(data = threeway,
                     weights = weight,
                     
                     dv_eval ~
                  
                     # treatments
                     group +
                      
                     # unbalanced covariate
                     post_libdem_frelect)

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
      term == "group7" | term == "group8" | term == "group9"  ~ "not-self"),
    justification = case_when(
      term == "(Intercept)" | term == "group4" | term == "group7"  ~ "Corruption",
      term == "group2" | term == "group5" | term == "group8"  ~ "None",
      term == "group3" | term == "group6" | term == "group9"  ~ "Self-serving"))

# plot parameters
tw.colours <- c("no" = "#5151d3",
                "self-interested" = "#e68619",
                "not-self" = "#26c0c7")

tw.legend <- c("no" = "**No** democratic  \ndefence",
               "self-interested" = "**Self-interested**  \ndemocratic defence",
               "not-self" = "**Genuine**  \ndemocratic defence")

# plot ...
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
  scale_x_discrete(limits=c("Self-serving", "None", "Corruption"))

# ... and save
ggsave(filename  = "figures/fig3.png",
       plot = fig3,
       width = 18,
       height = 14,
       dpi = 300,
       units = "cm")

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
'

fit_mm_ambi_corr <- sem(mm_ambi_corr, 
                        data = tbl_ambi,
                        estimator = "WLSMV",
                        sampling.weights = "weight")

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
'

fit_mm_ambi_self <- sem(mm_ambi_self, 
                        data = tbl_ambi,
                        estimator = "WLSMV",
                        sampling.weights = "weight")

summary(fit_mm_ambi_self,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

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
'

fit_mm_cred <- sem(mm_cred, 
                   data = tbl_cred,
                   estimator = "WLSMV",
                   sampling.weights = "weight")

summary(fit_mm_cred,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

### Figure 4 in the paper is made with an external illustrator program ---------
### Its elements can be replicated within R with the following code

# Panel A
lavaanPlot(model = fit_mm_ambi_corr, 
           coefs = TRUE,
           sig = 0.05,
           stars = "regress")

# Panel B
lavaanPlot(model = fit_mm_ambi_self, 
           coefs = TRUE,
           sig = 0.05,
           stars = "regress")

# Panel C
lavaanPlot(model = fit_mm_cred, 
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

#### Table: Participation DVs --------------------------------------------------
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
  
# Table 
kable(protest.table, 
      booktabs = TRUE, 
      format = "latex",
      caption = "Does Democratic Defence Result in Political Participation?",
      label = "protest",
      escape = TRUE)

# create a scaled protest battery for appendix figure to more easily compare effect sized
sm.scaled.protest <- sm.protest |> 
  filter(dv != "dv_protest") |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  dplyr::mutate(term = case_when(
    term == "justificationself-serving" ~ "H1a: **Self-serving hypothesis**  \nJustification: self-serving  \n*Reference: no justification*",
    term == "justificationcorruption" ~ "H1b: **Positive valence hypothesis**  \n*Reference: no justification*",
    term == "actionjudiciary" ~ "Targeting the judiciary  \n*Reference: targeting the media*",
    term == "demdefyes" ~ "H2: **Democratic defence hypothesis**  \n*Reference: no democratic defence*",
  ))

### Figure: Participation DVs -------------------------------------------------- 
# prepare data
sm.protest.plot <- sm.protest |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect",
         dv != "dv_protest_scaled") |> 
  dplyr::mutate(term = case_when(
    term == "justificationself-serving" ~ "H1a: **Self-serving hypothesis**  \nJustification: self-serving  \n*Reference: no justification*",
    term == "justificationcorruption" ~ "H1b: **Positive valence hypothesis**  \n*Reference: no justification*",
    term == "actionjudiciary" ~ "Targeting the judiciary  \n*Reference: targeting the media*",
    term == "demdefyes" ~ "H2: **Democratic defence hypothesis**  \n*Reference: no democratic defence*",
  ))

# plot
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
                 size = 0.6) +
  
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

# and save!
ggsave(filename  = "figures/fig5.png",
       plot = fig5,
       width = 18,
       height = 10,
       dpi = 300,
       units = "cm")

# /./ End of Code /./ #