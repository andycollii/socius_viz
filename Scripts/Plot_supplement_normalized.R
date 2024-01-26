library(tidyverse)

# Filepaths
net_degree_paths <- list.files(path = "/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/netDegree", pattern = "*.csv", full.names = TRUE)
civl_degree_paths <- list.files(path = "/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/civlDegree", pattern = "*.csv", full.names = TRUE)
demg_degree_paths <- list.files(path = "/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/demgDegree", pattern = "*.csv", full.names = TRUE)
rol_degree_paths <- list.files(path = "/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/rolDegree", pattern = "*.csv", full.names = TRUE)

# Function to read, process, and normalize data
load_and_process <- function(filepaths, source) {
  df_list <- lapply(filepaths, function(fp) {
    df <- read.csv(fp, col.names=c("Country", "Degree"))
    year <- str_extract(basename(fp), "\\d{4}")
    df$Year <- year
    df
  })
  
  df_combined <- bind_rows(df_list)
  df_combined$Year <- as.factor(df_combined$Year)
  
  df_avg <- df_combined %>%
    group_by(Year) %>%
    summarise(AvgDegree = mean(Degree, na.rm = TRUE), .groups = 'drop')
  
  # Normalize degree values (e.g., between 0 and 1)
  max_degree <- max(df_combined$Degree, na.rm = TRUE)
  min_degree <- min(df_combined$Degree, na.rm = TRUE)
  df_avg$NormalizedDegree <- (df_avg$AvgDegree - min_degree) / (max_degree - min_degree)
  
  df_avg$source <- source
  df_avg
}

# Load all datasets
df_avg1 <- load_and_process(net_degree_paths, "All categories")
df_avg2 <- load_and_process(civl_degree_paths, "Civil liberties")
df_avg3 <- load_and_process(demg_degree_paths, "Democracy, general")
df_avg4 <- load_and_process(rol_degree_paths, "Rule of law")

# Combine data frames
df_combined <- rbind(df_avg1, df_avg2, df_avg3, df_avg4)

# Plot
ggplot(df_combined, aes(x = Year, y = NormalizedDegree, color = source, group = source)) +
  geom_line(size = 1.5) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5),  # Center the plot title
    plot.margin = margin(t = 15, r = 10, b = 15, l = 15)  # Adjust the plot margins: top, right, bottom, left
  ) +
  labs(title = "Average Normalized Degree over Time", y = "Degree (normalized)")