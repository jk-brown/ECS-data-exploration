# reparameterizing rlnorm() parameters
lognorm <- function(m, sd){
  
  # re-parameterization of supplied mean value
  mn <- log(m^2 / sqrt(sd^2 + m^2))
  
  # re-parameterization of supplied sd value
  stdev <- sqrt(log(1 + (sd^2 / m^2)))
  
  # stores new value in list - when pushed to rlnorm(), will provide normal distribution
  # of arithmetic mean (m) and standard deviation (sd)
  c(mn, stdev)
  
}

# generates lognormal distirbutions
generate_lognormal_samples <- function(mu, sigma, n_samples) {
  
  # Generate normally distributed samples in log-space
  samples <- rlnorm(n_samples, lognorm(mu, sigma) [1], lognorm(mu, sigma) [2])
  
  return(samples)
}


# Compute meanlog and sdlog
mean_UL <- mean(UL)
sd_UL <- sd(UL)
meanlog <- log(mean_UL^2 / sqrt(sd_UL^2 + mean_UL^2))
sdlog <- sqrt(log(1 + (sd_UL^2 / mean_UL^2)))