# Load data
df <- read.csv("Faccini_news_index(1).csv")

# Convert date
df$date <- as.Date(df$date)

# Select the four indices
news_indexes <- df[, c("US.climate.policy",
                       "International.summits",
                       "Global.warming",
                       "Natural.disasters")]

# Run PCA
pca_result <- prcomp(news_indexes,
                     center = TRUE,
                     scale. = TRUE)

# Create plotting dataframe (does NOT modify your original data)
pca_plot_data <- data.frame(
  date = df$date,
  Aggregate_Climate_Index = pca_result$x[, 1]
)

# Plot
library(ggplot2)

ggplot(pca_plot_data, aes(x = date, y = Aggregate_Climate_Index)) +
  geom_line(linewidth = 0.4, colour = "steelblue") +
  theme_minimal() +
  labs(
    title = "Aggregate Climate News Index (First Principal Component)",
    x = "Date",
    y = "PC1"
  )

summary(pca_result)



library(ggplot2)
library(tidyr)
library(dplyr)

# Put the four news indices into long format
plot_data <- df %>%
  select(date,
         Global.warming,
         International.summits,
         Natural.disasters,
         US.climate.policy) %>%
  pivot_longer(
    cols = -date,
    names_to = "Index_type",
    values_to = "Index"
  )

# Keep panels in the same order as your screenshot
plot_data$Index_type <- factor(
  plot_data$Index_type,
  levels = c(
    "Global.warming",
    "International.summits",
    "Natural.disasters",
    "US.climate.policy"
  )
)

# Plot the four indices
ggplot(plot_data, aes(x = date, y = Index)) +
  geom_line(linewidth = 0.25, colour = "black") +
  facet_wrap(
    ~ Index_type,
    ncol = 2,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Index"
  ) +
  theme(
    strip.text = element_text(size = 9),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )
library(dplyr)
library(moments)

# Function to calculate summary statistics for one index
summary_stats <- function(x) {
  
  x <- na.omit(x)
  
  data.frame(
    Mean = mean(x),
    `1st` = quantile(x, 0.01, names = FALSE),
    Median = median(x),
    `99th` = quantile(x, 0.99, names = FALSE),
    `Std. Dev.` = sd(x),
    Skew = skewness(x),
    `rho(1)` = acf(x, lag.max = 1, plot = FALSE)$acf[2],
    check.names = FALSE
  )
}

# Calculate statistics for all four news indices
summary_table <- bind_rows(
  
  "US Climate Policy" =
    summary_stats(df$US.climate.policy),
  
  "International Summits" =
    summary_stats(df$International.summits),
  
  "Global Warming" =
    summary_stats(df$Global.warming),
  
  "Natural Disasters" =
    summary_stats(df$Natural.disasters),
  
  .id = "Index"
)

# Round to 2 decimal places
summary_table[, -1] <- round(summary_table[, -1], 2)

# Display table
summary_table
