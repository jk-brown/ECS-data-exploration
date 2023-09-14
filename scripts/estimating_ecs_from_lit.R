library(ggplot2)

lognorm <- function(m, sd){
  
  # re-parameterization of supplied mean value
  mn <- log(m^2 / sqrt(sd^2 + m^2))
  
  # re-parameterization of supplied sd value
  stdev <- sqrt(log(1 + (sd^2 / m^2)))
  
  # stores new value in list - when pushed to rlnorm(), will provide normal distribution
  # of arithmetic mean (m) and standard deviation (sd)
  c(mn, stdev)
  
}

# estimatin sigma from confidence intervals assuming sufficiently large sample size

estimate_sigma <- function(ci_5_95, ci_17_83) {
  
  # Calculate the length of the CIs
  percentile_length_5_95 <- ci_5_95[2] - ci_5_95[1]
  percentile_length_17_83 <- ci_17_83[2] - ci_17_83[1]
  
  # Estimate sigma using the average of the CI lengths
  sigma_5_95 <- percentile_length_5_95 / (2 * qnorm(0.95)) # z-value for 95%
  sigma_17_83 <- percentile_length_17_83 / (2 * qnorm(0.83)) # z-value for 84%
  
  # Average the two estimates assuming they are equally important for the estimated
  # sigma.
  # Goal to provide a single values of the sd.
  # Trying to account for uncertainty associated with both confidence intervals.
  estimated_sigma <- (sigma_5_95 + sigma_17_83) / 2
  
  return(estimated_sigma)
  
}

# sampling lognormal distribution
generate_lognormal_samples <- function(mu, sigma, n_samples) {
  
  # Generate normally distributed samples in log-space
  samples <- rlnorm(n_samples, lognorm(mu, sigma) [1], lognorm(mu, sigma) [2])
  
  return(samples)
}


# Example usage:
# Baseline - the synthesis of basic assumptions as outlined in Sherwood et al 2020
# Not necessarily a consensus estimate, but is based on transparent assumptions. 
# The sensitivity can be tested by: 
# 1) choice of prior 
# 2) the exclusion of each line of evidence 
# 3) allowance of uncertainties that are not present in other calculations 
baseline <- 3.2
baseline_5_95 <- c(2.3, 4.7)
baseline_17_83 <- c(2.6, 3.9)

# This is Baseline (UL) excluding Historical line of evidence 
no_hist <- 3.1
no_hist_5_95 <- c(2.0, 4.6)
no_hist_17_83 <- c(2.3, 3.7)

# This is the Baseline (UL) excluding paleo cold line of evidence
no_paleo_cold <- 3.4
no_paleo_cold_5_95 <- c(2.3, 5.1)
no_paleo_cold_17_83 <- c(2.6, 4.1)

n_samples <- 1000000

# estimate sigma
sigma_baseline <- estimate_sigma(baseline_5_95, baseline_17_83)
sigma_baseline

sigma_no_hist <- estimate_sigma(no_hist_5_95, no_hist_17_83)
sigma_no_hist

sigma_no_paleo_cold <- estimate_sigma(no_paleo_cold_5_95, no_paleo_cold_17_83)
sigma_no_paleo_cold

# Generate the Monte Carlo sample
baseline_samples <- generate_lognormal_samples(baseline, sigma_baseline, n_samples)
# how does this estimation do matching Table 10 in Sherwood et al.
mean(baseline_samples)
median(baseline_samples)

no_hist_samples <- generate_lognormal_samples(no_hist, sigma_no_hist, n_samples)
# how does this estimation do matching Table 10 in Sherwood et al.
mean(no_hist_samples)
median(no_hist_samples)

no_paleo_cold_samples <- generate_lognormal_samples(no_paleo_cold, sigma_no_paleo_cold, n_samples)
# how does this estimation do matching Table 10 in Sherwood et al.
mean(no_paleo_cold_samples)
median(no_paleo_cold)

# Plot the density plot
# compare plot with the PDFs in Figure 24 of Sherwood
ggplot() +
  geom_density(data = data.frame(baseline_samples), aes(x = baseline_samples),
               fill = NA, color = "black", linewidth = 1.0) +
  geom_density(data = data.frame(no_hist_samples), aes(x = no_hist_samples),
               fill = NA, color = "orange", linewidth = 1.0) +
  geom_density(data = data.frame(no_paleo_cold_samples), aes(x = no_paleo_cold_samples),
               fill = NA, color = "blue", linewidth = 1.0) +
  xlim(1, 8) +
  labs(title = "Density Plot of Monte Carlo Samples",
       x = "ECS Value",
       y = "Density")
