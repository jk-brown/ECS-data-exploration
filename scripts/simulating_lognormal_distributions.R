# library
library(triangle)
library(ggplot2)
library(ggridges)

# Read the dataset into a dataframe (assuming the dataset is in a CSV file)
df1 <- read.csv("data/combined_df.csv")
df1 <- na.omit(df1)

# Split the dataframe by type
df1_list <- split(df1, df1$type)

# Perform log-normal simulations for each type
simulation_list <- lapply(df1_list, function(df1_type) {
  simulation <- rlnorm(1000, meanlog = mean(log(df1_type$values)),
                       sdlog = sd(log(df1_type$values)))
  return(simulation)
})

# Combine all simulation samples into a single dataframe
simulation_df <- data.frame(
  type = rep(names(simulation_list), each = 1000),
  simulation = unlist(simulation_list)
)

# Create a ggridges plot to visualize the density plots for each type
ggplot(simulation_df, aes(x = simulation, y = type, fill = type)) +
  geom_density_ridges(scale = 3, alpha = 0.6) +
  theme_ridges() +
  theme_light() +
  labs(x = "ECS", y = "", title = "Density Plot of ECS Distribution by Type") +
  guides(fill = "none")


ggsave("figures/triangle_dist_from_simulation.png",
       width = 12,
       height = 8,
       units = "in",
       dpi = 300)
