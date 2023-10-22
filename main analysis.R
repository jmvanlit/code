# define new function to run multiple lm() for all protest items and the sum-index
run_multiple_lm <- function(dv){
  lm_formula <- as.formula(paste(dv, "~ action + demdef + justification + post_libdem_frelect"))
  model <- lm(lm_formula, data = demojudges, weights = weight)
  result <- tidy(model, conf.int = TRUE)
  result$dv <- dv
  return(result)
}

### Libraries ------------------------------------------------------------------
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

##### Data import --------------------------------------------------------------
demojudges <- read.csv("data/demojudges.csv") |> 
  as_tibble()

##### Data preparation  --------------------------------------------------------
demojudges <- demojudges |> 
  
  # convert to factor
  mutate(
    justification = as.factor(justification),
    action = as.factor(action),
    demdef = as.factor(demdef))

### Set reference categories
demojudges$justification <- relevel(demojudges$justification, ref = "none")
demojudges$action <- relevel(demojudges$action, ref = "media")
demojudges$demdef <- relevel(demojudges$demdef, ref = "no")

##### Simple models: H1a, H1b, H2 ----------------------------------------------
### DV = evaluation
# Simple Model
sm.eval <- lm(data = demojudges,
              weights = weight,
              
              dv_eval ~ 
                # treatments
                justification + action + demdef +
                
                # unbalanced covariates
                # income_nl left out for now as it is very incomplete and skews the analysis heavily
                post_libdem_frelect)

##### Interaction effect H3 ---------------------------------------------------- 
### DV = evaluation
# Interaction Model Self Interest
im.si.eval <- lm(data = demojudges,
                 weights = weight,
                 
                 dv_eval ~ 
                   #treatments
                   justification + action + demdef + (action * demdef) +
                   
                   # unbalanced covariates
                   # income_nl left out for now as it is very incomplete and skews the analysis heavily
                   post_libdem_frelect)

##### Interaction effect H4 ----------------------------------------------------

im.jd.eval <- lm(data = demojudges,
                 weights = weight,
                 
                 dv_eval ~ 
                   #treatments
                   justification + action + demdef + (justification * demdef) +
                   
                   # unbalanced covariates
                   # income_nl left out for now as it is very incomplete and skews the analysis heavily
                   post_libdem_frelect)

### Figure 2 -------------------------------------------------------------------

# Plot parameters
fig2_legend <- c("Model 3  \n**ambiguity** interaction", "Model 2  \n**credibility** interaction", "Model 1  \n**without** interaction")
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
    term == "justificationcorruption:demdefyes" ~ "H4: **Ambiguity hypothesis**  \nCorruption justification * Democratic defence  \n*Reference: no justification"
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
                 size = 0.6) +
  
  # figure out if we want to display the point-estimates in the plot
  # point-estimates
  #geom_label(aes(label = round(estimate, 2),
  #               colour = model),
  #           position = position_dodge(width = .5),
  #           show.legend = FALSE) +
  
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

ggsave(filename  = "figures/main_analysis.png",
       plot = fig2,
       width = 18,
       height = 14,
       dpi = 300,
       units = "cm")

##### Three-Way interaction H5 -------------------------------------------------
im.tw.eval <- lm(data = demojudges,
                 weights = weight,
                 
                 dv_eval ~ 
                   
                   #treatments
                   justification + action + demdef +
                   (justification * demdef) + (justification * action) + (demdef * action) +
                   (justification * demdef * action) +
                   
                   # unbalanced covariates
                   # income_nl left out for now as it is very incomplete and skews the analysis heavily
                   post_libdem_frelect)

### Figure 3 ----

# plot parameters
fig3_legend <- c("**Successful**  \ndemocratic defence", "**Unsuccessful**  \ndemocratic defence", "**Backlash**  \nagainst democratic defence")
fig3_colours <- c("#26c0c7", "#5151d3", "#d83790")

# prepare data
im.tw.eval.plot <- im.tw.eval |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(term = case_when(
    term == "justificationcorruption" ~ "Action: media  \nJustification: corruption  \nDemocratic defence: no",
    term == "demdefyes" ~ "Action: media  \nJustification: none  \nDemocratic defence: yes",
    term == "justificationself-serving" ~ "Action: media  \nJustification: self-serving  \nDemocratic defence: no",
    term == "actionjudiciary" ~ "Action: judiciary   \nJustification: none  \nDemocratic defence: no",
    term == "justificationcorruption:demdefyes" ~ "Action: media  \nJustification: corruption  \nDemocratic defence: yes",
    term == "justificationcorruption:actionjudiciary" ~ "Action: judiciary  \nJustification: corruption  \nDemocratic defence: no",
    term == "justificationself-serving:demdefyes" ~ "Action: media  \nJustification: self-serving  \nDemocratic defence: yes",
    term == "justificationself-serving:actionjudiciary" ~ "Action: judiciary  \nJustification: self-serving  \nDemocratic defence: no",
    term == "actionjudiciary:demdefyes" ~ "Action: judiciary   \nJustification: none  \nDemocratic defence: yes",
    term == "justificationcorruption:actionjudiciary:demdefyes" ~ "Action: judiciary  \nJustification: corruption  \nDemocratic defence: yes",
    term == "justificationself-serving:actionjudiciary:demdefyes" ~ "Action: judiciary  \nJustification: self-serving  \nDemocratic defence: yes",
  )) |> 
  mutate(direction = case_when(
    estimate > 0 & p.value < 0.05 ~ "positive",
    estimate < 0 & p.value < 0.05 ~ "negative",
    TRUE ~ "null"))

# create plot
fig3 <- 
  ggplot(data = im.tw.eval.plot,
         aes(x = estimate,
             y = reorder(term, desc(estimate)))) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # errorbars
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = direction,
                     linetype = direction),
                 height = 0,
                 size = 0.6) +
  
  # points
  geom_point(aes(colour = direction,
                 shape = direction),
             size = 3) + 
  
  # theme
  theme_classic() +
  scale_colour_manual(values = fig3_colours,
                      labels = fig3_legend) +
  scale_shape_manual(values = c(15, 17, 19),
                     labels = fig3_legend) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash"),
                        labels = fig3_legend) +
  
  # labels and legend
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.justification = c(1,0))

ggsave(filename  = "figures/threeway.png",
       plot = fig3,
       width = 18,
       height = 14,
       dpi = 300,
       units = "cm")

##### Table Main Models --------------------------------------------------------
main_models <- list(sm.eval, im.si.eval, im.jd.eval, im.tw.eval)

texreg(main_models,
       caption = "Does Democratic Defence Matter?",
       caption.above = TRUE,
       label = "tab:mainmodels")

##### Mediation analysis with ambiguity and credibility ------------------------
# ambiguity lies in the justification, so that is the main explanation for that mediation
# credibility lies in the type of actor (self-interest or not), which we simulate with the action-treatment

### Ambiguity ----
tbl_ambi <- demojudges |> 
  filter(dv_ambi != "NA") |> 
  
  # reverse coding to match intuition: higher scores mean more ambiguity
  mutate(dv_ambi = case_when(
    dv_ambi == 1 ~ 6,
    dv_ambi == 2 ~ 5,
    dv_ambi == 3 ~ 4,
    dv_ambi == 4 ~ 3,
    dv_ambi == 5 ~ 2,
    dv_ambi == 6 ~ 1,
  ))

cm.ambi.xy <- lm(dv_eval ~ justification + action + demdef,
                 data = tbl_ambi,
                 weights = weight)

cm.ambi.xm <- lm(dv_ambi ~ justification + action + demdef,
                 data = tbl_ambi,
                 weights = weight)

cm.ambi.xmy <- lm(dv_eval ~ justification + dv_ambi + action + demdef,
                  data = tbl_ambi,
                  weights = weight)

texreg(list(cm.ambi.xy, cm.ambi.xm, cm.ambi.xmy),
       custom.header = list("Dependent Variable:" = 1:3),
       custom.model.names = c("Democracy Evaluation", "Ambiguity", "Democracy Evaluation"),
       label = "tab:ambi",
       caption = "Ambiguity Mediation Models",
       caption.above = TRUE)

cm.ambi.cor <- mediation::mediate(cm.ambi.xm, cm.ambi.xmy,
                                  treat = "justification",
                                  treat.value = "corruption",
                                  control.value = "none",
                                  mediator = "dv_ambi")

summary(cm.ambi.cor)

cm.ambi.sel <- mediation::mediate(cm.ambi.xm, cm.ambi.xmy,
                                  treat = "justification",
                                  treat.value = "self-serving",
                                  control.value = "none",
                                  mediator = "dv_ambi")

summary(cm.ambi.sel)

### Credibility ----
# these models are run without demdef as dv_cred was only shown when demdef == 1

tbl_cred <- demojudges |> 
  filter(dv_cred != "NA") |> 
  
  # reverse coding to match intuition: higher scores mean more credibility
  mutate(dv_cred = case_when(
    dv_cred == 1 ~ 6,
    dv_cred == 2 ~ 5,
    dv_cred == 3 ~ 4,
    dv_cred == 4 ~ 3,
    dv_cred == 5 ~ 2,
    dv_cred == 6 ~ 1,
  ))

cm.cred.xy <- lm(dv_eval ~ justification + action,
                 data = tbl_cred,
                 weights = weight)

cm.cred.xm <- lm(dv_cred ~ justification + action,
                 data = tbl_cred, 
                 weights = weight)

cm.cred.xmy <- lm(dv_eval ~ justification + dv_cred + action,
                  data = tbl_cred,
                  weights = weight)

texreg(list(cm.cred.xy, cm.cred.xm, cm.cred.xmy),
       custom.header = list("Dependent Variable:" = 1:3),
       custom.model.names = c("Democracy Evaluation", "Credibility", "Democracy Evaluation"),
       label = "tab:cred",
       caption = "Credibility Mediation Models",
       caption.above = TRUE)

cm.cred <- mediation::mediate(cm.cred.xm, cm.cred.xmy,
                              treat = "action",
                              treat.value = "judiciary",
                              control.value = "media",
                              mediator = "dv_cred")

summary(cm.cred)

#####  Protest Effects ---------------------------------------------------------

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

protest.legend <- c("dv_protest" = "**Full protest battery**", 
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

### Figure 7 ----  
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
fig7 <- 
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
ggsave(filename  = "figures/simple-protest.png",
       plot = fig7,
       width = 18,
       height = 10,
       dpi = 300,
       units = "cm")

### Figure 8 ----
# plot parameters
scaled.colours <- c("dv_protest_scaled" = "#d83790", 
                    "dv_protest_vote" = "#00577C", 
                    "dv_protest_poster" = "#4D8F8D", 
                    "dv_protest_pers" = "#4C716E",
                    "dv_protest_peti" = "#6884C1",
                    "dv_protest_lawpr" = "#719FCE", 
                    "dv_protest_cont" = "#3A3D7E", 
                    "dv_protest_unlaw" = "#586174")

scaled.legend <- c("dv_protest_scaled" = "**Full protest battery**", 
                   "dv_protest_vote" = "Vote", 
                   "dv_protest_poster" = "Poster", 
                   "dv_protest_pers" = "Persuade",
                   "dv_protest_peti" = "Petition",
                   "dv_protest_lawpr" = "Lawful protest", 
                   "dv_protest_cont" = "Contact", 
                   "dv_protest_unlaw" = "Unlawful protest")
# plot
fig8 <- 
  ggplot(data = sm.scaled.protest,
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
  facet_wrap(~ dv, 
             ncol = 4,
             labeller = as_labeller(c(dv_protest_scaled = "**Full protest battery**",
                                      dv_protest_vote = "Vote",
                                      dv_protest_poster = "Poster",
                                      dv_protest_pers= "Persuade",
                                      dv_protest_peti = "Petition",
                                      dv_protest_lawpr = "Lawful protest",
                                      dv_protest_cont = "Contact", 
                                      dv_protest_unlaw = "Unlawful protest"))) +
  scale_colour_manual(values = scaled.colours) +
  scale_shape_manual(values = c(15, 1, 2, 5, 6, 7, 9, 13)) +
  
  # labels and legends
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        strip.text.x = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.key.width = unit(1.5, "cm"),
        legend.justification = c(1,0))

# and save!
ggsave(filename  = "figures/simple-protest2.png",
       plot = fig8,
       width = 18,
       height = 10,
       dpi = 300,
       units = "cm")

##### KLAD NOT INCLUDED ---------------------
### interaction models
# just on the full protest battery

# interaction credibility H3
im.cred.protest <- lm(data = demojudges,
                      weights = weight,
                      dv_protest ~ action * demdef + justification + post_libdem_frelect) |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(model = "credibility")

# interaction ambiguity H4
im.ambi.protest <- lm(data = demojudges,
                      weights = weight,
                      dv_protest ~ action + demdef * justification + post_libdem_frelect) |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(model = "ambiguity")

# threeway interaction H5
im.tw.protest <- lm(data = demojudges,
                    weights = weight,
                    dv_protest ~ action  *demdef * justification + post_libdem_frelect) |> 
  tidy(conf.int = TRUE) |> 
  filter(term != "(Intercept)",
         term != "post_libdem_frelect") |> 
  mutate(model = "threeway")

### Figure 9 ----
# create plot data
im.protest.plot <- bind_rows(im.cred.protest, im.ambi.protest, im.tw.protest)

# plot parameters
fig9_colours <- c("#26c0c7", "#5151d3", "#d83790")
fig9_legend <- c("**Ambiguity** interactions", "**Credibility** interaction", "**Threeway** interaction")

# plot
#fig9 <- 
ggplot(data = im.protest.plot,
       aes(x = estimate,
           y = reorder(term, desc(estimate)))) +
  
  # zero-line
  geom_vline(xintercept = 0,
             linetype = "dashed",
             colour = "darkgrey") +
  
  # points, errorbars
  geom_point(aes(colour = model,
                 shape = model),
             size = 3,
             position = position_dodge(width = .5)) +
  
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     colour = model,
                     linetype = model),
                 position = position_dodge(width = .5),
                 height = 0,
                 size = 0.6) +
  
  # theme
  theme_classic() +
  scale_colour_manual(values = fig9_colours,
                      labels = fig9_legend) +
  scale_shape_manual(values = c(15, 17, 19),
                     labels = fig9_legend) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash"),
                        labels = fig9_legend) +
  
  # labels and legend
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(axis.text.y = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.justification = c(1,0))

# and save

### interaction model
# define new function to run multiple lm() for all protest items and the sum-index
run_multiple_lm_interaction <- function(dv){
  lm_formula <- as.formula(paste(dv, "~ action + demdef + justification + action * demdef"))
  model <- lm(lm_formula, data = demojudges, weights = weight)
  result <- tidy(model, conf.int = TRUE)
  result$dv <- dv
  return(result)
}

im.protest <- map_df(protest.dvs, run_multiple_lm_interaction) |> 
  mutate(model = "interaction")

# prepare data
im.protest.plot <- bind_rows(sm.protest, im.protest) |>
  filter( term != "(Intercept)") |> 
  mutate(term = case_when(
    term == "justificationself-serving" ~ "H1a: **Self-serving hypothesis**  \nJustification: self-serving  \n*Reference: no justification*",
    term == "justificationcorruption" ~ "H1b: **Positive valence hypothesis**  \n*Reference: no justification*",
    term == "actionjudiciary" ~ "Targeting the judiciary  \n*Reference: targeting the media*",
    term == "demdefyes" ~ "H2: **Democratic defence hypothesis**  \n*Reference: no democratic defence*",
    term == "actionjudiciary:demdefyes" ~ "H3: **Credibility hypothesis**  \nDemocratic defence * Action against judiciary",
  ))

# table
sm.protest.1 <- sm.protest |> 
  mutate(star = case_when(
    p.value < 0.05 & p.value >= 0.01 ~ "*",
    p.value < 0.01 & p.value >= 0.001 ~ "**",
    p.value < 0.001 ~ "***"
  )) |> 
  dplyr::select(term, estimate, std.error, star, dv) |> 
  mutate(estimate = round(estimate, 2),
         std.error = round(std.error, 2))

# table
im.protest.1 <- im.protest |> 
  mutate(star = case_when(
    p.value < 0.05 & p.value >= 0.01 ~ "*",
    p.value < 0.01 & p.value >= 0.001 ~ "**",
    p.value < 0.001 ~ "***"
  )) |> 
  dplyr::select(term, estimate, std.error, star, dv) |> 
  mutate(estimate = round(estimate, 2),
         std.error = round(std.error, 2))


