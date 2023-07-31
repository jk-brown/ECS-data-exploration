# In this script: Exploring ways to assess ECS values
library(matilda)

ini <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini)

# set params
set.seed(1)
params1 <- generate_params(core, 50)
params_ECS_baseline$ECS <- 3.2

params_ECS_no_paleo <- params1[1:5]
params_ECS_no_paleo$ECS <- 3.8

params_ECS_emergen_emergent_constraint <- params1[1:5]
params_ECS_emergen_emergent_constraint$ECS <- 3.4

params_list <- list(params_ECS_baseline,
                    params_ECS_no_paleo,
                    params_ECS_emergen_emergent_constraint)

# iterate
result_list <-  lapply(params_list, function(params) {
  iterate_hector(core, params, save_years = 1850:2100)})

result_list

# Criterion
criterion = criterion_co2_obs()

# Compute weights and merge with hector result
weight_list <- lapply(result_list, function(x) {
  score_hruns(x, criterion, score_ramp, w1 = 3, w2 = 12)
})

# Merge results so each Hector run in h_result is assigned its corresponding 
# weight. 
wt_result_list <- Map(merge, result_list, weight_list, by = "run_number")

# bind results
wt_result <- do.call(rbind, wt_result_list)

# median warming calculation
# extra libraries
library(spatstat)
library(tidyverse)
library(ggplot2)

# subset data to include temp
temp_proj <- subset(wt_result,
                    variable == GLOBAL_TAS())
# add evidence identifier
temp_proj$evidence <- rep(c("baseline, ECS = 3.2", "no_paleo, ECS = 3.8", 
                            "emergent constraint, ECS = 3.4"), each = 12550)

#compute median warming and quantiles
median_warming <- temp_proj %>% 
  group_by(year, evidence) %>% 
  reframe(median_warming = weighted.median(value, w = weights),
          CI_5 = weighted.quantile(value, w = weights, probs = 0.95),
          CI_95 = weighted.quantile(value, w = weights, probs = 0.05))
median_warming

# plotting 
ecs_plot <- ggplot(data = median_warming,
                   aes(x = year, y = median_warming,
                       color = evidence,
                       fill = evidence)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(x = year, ymin = CI_5, ymax = CI_95),
              alpha = 0.2,
              color = NA) +
  theme_light() +
  ggtitle("Weighted Median Global Temperature (5-95% CI)") +
  labs(y = "Global Temperature Anomaly")
ecs_plot

