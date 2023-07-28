# In this script: Exploring ways to assess ECS values
library(matilda)

ini <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini)

# set params
set.seed(1)
params1 <- generate_params(core, 50)
params_ECS_baseline <- params1[1:5]
params_ECS_baseline$ECS <- 3.2

params_ECS_no_paleo <- params1[1:5]
params_ECS_no_paleo$ECS <- 3.8

params_list <- list(params_ECS_baseline, params_ECS_no_paleo)

# iterate
result_list <-  lapply(params_list, function(params) {
  iterate_hector(core, params, save_years = 1850:2100)})

result_baseline <- iterate_hector(core, params_ECS_baseline, save_years = 1850:2100)
result_no_paleo <- iterate_hector(core, params_ECS_no_paleo, save_years = 1850:2100)

result_list

# Criterion
criterion = criterion_co2_obs()

# Compute weights and merge with hector result
weights_baseline <- score_hruns(result_baseline, criterion, score_ramp, w1 = 0, w2 = 12)
weights_no_paleo <- score_hruns(result_no_paleo, criterion, score_ramp, w1= 0, w2 = 12)

# Merge results so each Hector run in h_result is assigned its corresponding 
# weight. 
wt_result_baseline <- merge(result_baseline, weights_baseline, by = "run_number")
wt_result_no_paleo <- merge(result_no_paleo, weights_no_paleo, by = "run_number")

# median wamring calculation
# extra libraries
library(spatstat)
library(tidyverse)
library(ggplot2)

temp_proj_baseline <- subset(wt_result_baseline,
                             variable == GLOBAL_TAS())
temp_proj_nopaleo <- subset(wt_result_no_paleo,
                            variable == GLOBAL_TAS())

median_warming_baseline <- temp_proj_baseline %>% 
  group_by(year) %>% 
  reframe(median_warming = weighted.median(value, w = weights),
          CI_5 = weighted.quantile(value, w = weights, probs = 0.95),
          CI_95 = weighted.quantile(value, w = weights, probs = 0.05))
median_warming_baseline

median_warming_nopaleo <- temp_proj_nopaleo %>% 
  group_by(year) %>% 
  reframe(median_warming = weighted.median(value, w = weights),
          CI_5 = weighted.quantile(value, w = weights, probs = 0.95),
          CI_95 = weighted.quantile(value, w = weights, probs = 0.05))
median_warming_nopaleo

# plotting 
baseline <- ggplot(data = median_warming_baseline,
                   aes(x = year, y = median_warming)) +
  geom_line(color = "salmon") +
  geom_ribbon(aes(x = year, ymin = CI_5, ymax = CI_95),
              fill = "salmon", alpha = 0.2)

baseline +
  geom_line(data = median_warming_nopaleo,
            aes(x = year, y = median_warming),
            color = "dodgerblue") +
  geom_ribbon(data = median_warming_nopaleo, 
              aes(x = year, ymin = CI_5, ymax = CI_95),
              fill = "dodgerblue", alpha = 0.2) +
  theme_light()

