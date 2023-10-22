##### SET UP ---------------------------------------------------------------------------------------

### packages
library(DeclareDesign)
library(ggplot2)
library(dplyr)

##### POWER ANALYSIS ------------------------------------------------------------------------------

### Base Parameters (all can be varied later)
N <- 120
b <- 0.1
n_cond <- 6

### Declare Design
DemoJudges_design <- 
  
  # Model
  declare_population(N = N,
                     u = rnorm(N)) +
  declare_potential_outcomes(Y ~ b * Z + u) +
  
  # Inquiry
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  
  # Data Strategy
  declare_assignment(Z = complete_ra(N,
                                     prob = 1 / n_cond),
                     Y = reveal_outcomes(Y ~ Z)) +
  
  # Answer Strategy
  declare_estimator(Y ~ Z,
                    inquiry = "ATE",
                    .method = lm)

### Incorporate multiple designs
DemoJudges_multiple_designs <- 
  DemoJudges_design |> 
  redesign(b = c(0.1, 0.15, 0.2, 0.3), # vary effect size
           N = seq(from = 3000, # vary number of respondents
                   to = 9000,
                   by = 500),
           n_cond = c(2, 3, 6, 12)) # vary number of conditions to be compared

### Diagnose
DemoJudges_power <-
  diagnose_design(DemoJudges_multiple_designs,
                  sims = 1000) |>
  tidy() |> 
  filter(diagnosand == "power")

DemoJudges_power <- # rename to show relevant Hs in plot
  DemoJudges_power |> 
  mutate(n_cond = recode(n_cond,
                         "2" = "H2: Democratic Defence",
                         "3" = "H1a and H1b: Justification",
                         "6" = "H3: Credibility and H4: Ambiguity",
                         "12" = "H5: Three-Way Interaction"))

### Plot
p1 <-
  ggplot(data = DemoJudges_power,
         aes(x = N,
             y = estimate)) +
  
  # Criteria
  geom_hline(yintercept = 0.8,
             linetype = "dotdash",
             color = "darkgrey") +
  geom_vline(xintercept = 4000,
             linetype = "dotdash",
             color = "darkgrey") +
  geom_vline(xintercept = 8000,
             linetype = "dotdash",
             color = "darkgrey") +
  
  # CIs
  geom_ribbon(aes(ymin = conf.low,
                  ymax = conf.high,
                  group = factor(b)),
              alpha = 0.2) +
  
  # Power lines
  geom_point(aes(shape = factor(b),
                 color = factor(b)),
             size = 2) +
  geom_line(aes(color = factor(b),
                group = factor(b)),
            linewidth = 0.8) +
  
  # Facet per Hypothesis
  facet_wrap(~n_cond) +
  
  # Annotations
  labs(x = "Number of Respondents",
       y = "Power (One-Tailed)",
       color = "Effect size",
       shape = "Effect size") +
  
  # Theme
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  scale_colour_manual(values = c("#16607a", "#09bb9f", "#ffb55f", "#a63716"))

p1

ggsave(plot = p1,
       filename = "figures/power_analysis.png",
       dpi = 1200,
       width = 7,
       height = 6)

# END OF CODE /./
