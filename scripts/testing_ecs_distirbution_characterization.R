#library 
library(ggplot2)

# Define the median/mean, min, and max values
median_val <- 2
min_val <- 1
max_val <- 9.3

# Estimate the log-scale parameters
mu <- log(median_val)
sigma <- (log(max_val) - log(min_val)) / (2 * qnorm(0.75))

# Generate lognormal distribution object
lognormal_dist <- function(n) rlnorm(n, meanlog = mu, sdlog = sigma)

# Set the desired number of samples
num_samples <- 1000

# Sample from the lognormal distribution
samples <- data.frame( values = lognormal_dist(num_samples))

# Print the sampled values
print(samples)

# Plot histogram of the sampled lognormal distribution
ggplot(data = samples, aes(x = values)) +
  geom_density(color = 'black', 
                 fill = 'lightgrey') +
  theme_light() +
  labs(title = "Historam of Log-Normal Distribution",
       x = "Values")

# Plot density curve of the sampled lognormal distribution
density_obj <- density(samples)
plot(density_obj, main = "Lognormal Distribution",
     xlab = "Value", ylab = "Density")
