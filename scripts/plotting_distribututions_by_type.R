# Library 
library(ggplot2)

# Read the dataset into a dataframe (assuming the dataset is in a CSV file)
df <- read.csv("data/ecs_test.csv")
df <- na.omit(df)

# Split the dataframe by study
df_list <- split(df, df$study_name)

# Initialize an empty list to store stacked dataframes
combined_df_list <- list()

# Iterate over each study and stack the min, max, and mean values
for (study_name in names(df_list)) {
  study_df <- df_list[[study_name]]
  combined_df <- data.frame(values = c(study_df$min, study_df$max, study_df$mean),
                           study_name = rep(study_df$study_name, each = 3),
                           type = rep(study_df$type, each = 3),
                           year = rep(study_df$year, each = 3))
  combined_df_list[[study_name]] <- combined_df
}

# Combine all stacked dataframes into a single dataframe
combined_df <- do.call(rbind, combined_df_list)
rownames(combined_df) <- NULL
write.csv(combined_df, "data/combined_df.csv")

# Plot the histograms by type
ggplot(combined_df, aes(x = values)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 0.7, fill = "grey", position = "identity") +
  geom_density(lwd = 1.2, linetype = 2, colour = "red") +
  facet_wrap(~type, nrow = 1) +
  theme_light() +
  labs(x = "ECS", y = "Frequency", title = "Histogram of ECS Values by Type") +
  scale_fill_discrete(name = "Type")

ggsave("figures/distributions_from_ECS_data.png",
       width = 8,
       units = "in")
