# Load Packages
library(tidyverse)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(grid)
library(patchwork)

##################################################################################################################
#Civil liberties
##################################################################################################################

# loading graph density file
Density <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Figures/Figure 1/Robust_Densities/Density_CIV.csv")
# Change 'Year' to a factor variable
Density$Year <- as.factor(Density$Year)

# Loading dataframes for full network without header
df1981 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/civlDegree/civl1981.csv", header = FALSE)
df1985 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/civlDegree/civl1985.csv", header = FALSE)
df1990 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/civlDegree/civl1990.csv", header = FALSE)
df1995 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/civlDegree/civl1995.csv", header = FALSE)
df2000 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/civlDegree/civl2000.csv", header = FALSE)
df2005 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/civlDegree/civl2005.csv", header = FALSE)
df2010 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/civlDegree/civl2010.csv", header = FALSE)
df2015 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/civlDegree/civl2015.csv", header = FALSE)

# Set column names for all data frames
colnames(df1981) <- c("country", "degree")
colnames(df1985) <- c("country", "degree")
colnames(df1990) <- c("country", "degree")
colnames(df1995) <- c("country", "degree")
colnames(df2000) <- c("country", "degree")
colnames(df2005) <- c("country", "degree")
colnames(df2010) <- c("country", "degree")
colnames(df2015) <- c("country", "degree")

# Add year variable
df1981$Year <- '1981'
df1985$Year <- '1985'
df1990$Year <- '1990'
df1995$Year <- '1995'
df2000$Year <- '2000'
df2005$Year <- '2005'
df2010$Year <- '2010'
df2015$Year <- '2015'

# Convert Year to factor
df1981$Year <- as.factor(df1981$Year)
df1985$Year <- as.factor(df1985$Year)
df1990$Year <- as.factor(df1990$Year)
df1995$Year <- as.factor(df1995$Year)
df2000$Year <- as.factor(df2000$Year)
df2005$Year <- as.factor(df2005$Year)
df2010$Year <- as.factor(df2010$Year)
df2015$Year <- as.factor(df2015$Year)

# Normalize function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# For Civil liberties

# Combine all dataframes
df <- rbind(df1981, df1985, df1990, df1995, df2000, df2005, df2010, df2015)

# Convert Year to factor
df$Year <- as.factor(df$Year)

# Normalize the degree values
df$degree_normalized <- normalize(df$degree)

df_avg <- df %>%
  group_by(Year) %>%
  summarise(Avgdegree = mean(degree_normalized))

# Create the plot with aggregated data and remove the legend
g1 <- ggplot(df, aes(x = Year, y = degree_normalized, fill = Year)) +
  geom_point(position = position_jitter(width = 0.25), size = 1, alpha = 0.5, aes(color = Year)) +  # For the points
  geom_line(data = df_avg, aes(x = as.numeric(Year), y = Avgdegree, group = 1), color = "red", size = 1) +
  geom_violin() +    # For the violin plot
  labs(x = "Year", y = "Degree (normalized)") +
  ggtitle("Civil liberties") +
  theme_minimal() +
  expand_limits(y = 0) +  # Ensure y-axis starts at 0
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +  # Updated y breaks for normalized data
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))  # Center the title
g1

##################################################################################################################
#Rule of law
##################################################################################################################
# loading graph density file
Density <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Figures/Figure 1/Robust_Densities/Density_ROL.csv")
# Change 'Year' to a factor variable
Density$Year <- as.factor(Density$Year)

# Loading dataframes for full network without header
df1981 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/rolDegree/rol1981.csv", header = FALSE)
df1985 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/rolDegree/rol1985.csv", header = FALSE)
df1990 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/rolDegree/rol1990.csv", header = FALSE)
df1995 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/rolDegree/rol1995.csv", header = FALSE)
df2000 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/rolDegree/rol2000.csv", header = FALSE)
df2005 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/rolDegree/rol2005.csv", header = FALSE)
df2010 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/rolDegree/rol2010.csv", header = FALSE)
df2015 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/rolDegree/rol2015.csv", header = FALSE)

# Set column names for all data frames
colnames(df1981) <- c("country", "degree")
colnames(df1985) <- c("country", "degree")
colnames(df1990) <- c("country", "degree")
colnames(df1995) <- c("country", "degree")
colnames(df2000) <- c("country", "degree")
colnames(df2005) <- c("country", "degree")
colnames(df2010) <- c("country", "degree")
colnames(df2015) <- c("country", "degree")

# Add year variable
df1981$Year <- '1981'
df1985$Year <- '1985'
df1990$Year <- '1990'
df1995$Year <- '1995'
df2000$Year <- '2000'
df2005$Year <- '2005'
df2010$Year <- '2010'
df2015$Year <- '2015'

# Convert Year to factor
df1981$Year <- as.factor(df1981$Year)
df1985$Year <- as.factor(df1985$Year)
df1990$Year <- as.factor(df1990$Year)
df1995$Year <- as.factor(df1995$Year)
df2000$Year <- as.factor(df2000$Year)
df2005$Year <- as.factor(df2005$Year)
df2010$Year <- as.factor(df2010$Year)
df2015$Year <- as.factor(df2015$Year)

# Normalize function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# For Civil liberties

# Combine all dataframes
df <- rbind(df1981, df1985, df1990, df1995, df2000, df2005, df2010, df2015)

# Convert Year to factor
df$Year <- as.factor(df$Year)

# Normalize the degree values
df$degree_normalized <- normalize(df$degree)

df_avg <- df %>%
  group_by(Year) %>%
  summarise(Avgdegree = mean(degree_normalized))

# Create the plot with aggregated data and remove the legend
g2 <- ggplot(df, aes(x = Year, y = degree_normalized, fill = Year)) +
  geom_point(position = position_jitter(width = 0.25), size = 1, alpha = 0.5, aes(color = Year)) +  # For the points
  geom_line(data = df_avg, aes(x = as.numeric(Year), y = Avgdegree, group = 1), color = "red", size = 1) +
  geom_violin() +    # For the violin plot
  labs(x = "Year", y = "Degree (normalized)") +
  ggtitle("Rule of law") +
  theme_minimal() +
  expand_limits(y = 0) +  # Ensure y-axis starts at 0
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +  # Updated y breaks for normalized data
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))  # Center the title
g2

##################################################################################################################
#Democracy, general
##################################################################################################################
# loading graph density file
Density <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Figures/Figure 1/Robust_Densities/Density_DEM.csv")
# Change 'Year' to a factor variable
Density$Year <- as.factor(Density$Year)

# Loading dataframes for full network without header
df1981 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/demgDegree/demg1981.csv", header = FALSE)
df1985 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/demgDegree/demg1985.csv", header = FALSE)
df1990 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/demgDegree/demg1990.csv", header = FALSE)
df1995 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/demgDegree/demg1995.csv", header = FALSE)
df2000 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/demgDegree/demg2000.csv", header = FALSE)
df2005 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/demgDegree/demg2005.csv", header = FALSE)
df2010 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/demgDegree/demg2010.csv", header = FALSE)
df2015 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/demgDegree/demg2015.csv", header = FALSE)

# Set column names for all data frames
colnames(df1981) <- c("country", "degree")
colnames(df1985) <- c("country", "degree")
colnames(df1990) <- c("country", "degree")
colnames(df1995) <- c("country", "degree")
colnames(df2000) <- c("country", "degree")
colnames(df2005) <- c("country", "degree")
colnames(df2010) <- c("country", "degree")
colnames(df2015) <- c("country", "degree")

# Add year variable
df1981$Year <- '1981'
df1985$Year <- '1985'
df1990$Year <- '1990'
df1995$Year <- '1995'
df2000$Year <- '2000'
df2005$Year <- '2005'
df2010$Year <- '2010'
df2015$Year <- '2015'

# Convert Year to factor
df1981$Year <- as.factor(df1981$Year)
df1985$Year <- as.factor(df1985$Year)
df1990$Year <- as.factor(df1990$Year)
df1995$Year <- as.factor(df1995$Year)
df2000$Year <- as.factor(df2000$Year)
df2005$Year <- as.factor(df2005$Year)
df2010$Year <- as.factor(df2010$Year)
df2015$Year <- as.factor(df2015$Year)

# Normalize function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# For Civil liberties

# Combine all dataframes
df <- rbind(df1981, df1985, df1990, df1995, df2000, df2005, df2010, df2015)

# Convert Year to factor
df$Year <- as.factor(df$Year)

# Normalize the degree values
df$degree_normalized <- normalize(df$degree)

df_avg <- df %>%
  group_by(Year) %>%
  summarise(Avgdegree = mean(degree_normalized))

# Create the plot with aggregated data and remove the legend
g3 <- ggplot(df, aes(x = Year, y = degree_normalized, fill = Year)) +
  geom_point(position = position_jitter(width = 0.25), size = 1, alpha = 0.5, aes(color = Year)) +  # For the points
  geom_line(data = df_avg, aes(x = as.numeric(Year), y = Avgdegree, group = 1), color = "red", size = 1) +
  geom_violin() +    # For the violin plot
  labs(x = "Year", y = "Degree (normalized)") +
  ggtitle("Democracy, general") +
  theme_minimal() +
  expand_limits(y = 0) +  # Ensure y-axis starts at 0
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +  # Updated y breaks for normalized data
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))  # Center the title
g3

##################################################################################################################
#Overall network
##################################################################################################################
# loading graph density file
Density <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Figures/Figure 1/Robust_Densities/Density_FULL.csv")
# Change 'Year' to a factor variable
Density$Year <- as.factor(Density$Year)

# Loading dataframes for full network without header
df1981 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/netDegree/net1981.csv", header = FALSE)
df1985 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/netDegree/net1985.csv", header = FALSE)
df1990 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/netDegree/net1990.csv", header = FALSE)
df1995 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/netDegree/net1995.csv", header = FALSE)
df2000 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/netDegree/net2000.csv", header = FALSE)
df2005 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/netDegree/net2005.csv", header = FALSE)
df2010 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/netDegree/net2010.csv", header = FALSE)
df2015 <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGO_Materials/Degree/netDegree/net2015.csv", header = FALSE)

# Set column names for all data frames
colnames(df1981) <- c("country", "degree")
colnames(df1985) <- c("country", "degree")
colnames(df1990) <- c("country", "degree")
colnames(df1995) <- c("country", "degree")
colnames(df2000) <- c("country", "degree")
colnames(df2005) <- c("country", "degree")
colnames(df2010) <- c("country", "degree")
colnames(df2015) <- c("country", "degree")

# Add year variable
df1981$Year <- '1981'
df1985$Year <- '1985'
df1990$Year <- '1990'
df1995$Year <- '1995'
df2000$Year <- '2000'
df2005$Year <- '2005'
df2010$Year <- '2010'
df2015$Year <- '2015'

# Convert Year to factor
df1981$Year <- as.factor(df1981$Year)
df1985$Year <- as.factor(df1985$Year)
df1990$Year <- as.factor(df1990$Year)
df1995$Year <- as.factor(df1995$Year)
df2000$Year <- as.factor(df2000$Year)
df2005$Year <- as.factor(df2005$Year)
df2010$Year <- as.factor(df2010$Year)
df2015$Year <- as.factor(df2015$Year)

# Normalize function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# For Civil liberties

# Combine all dataframes
df <- rbind(df1981, df1985, df1990, df1995, df2000, df2005, df2010, df2015)

# Convert Year to factor
df$Year <- as.factor(df$Year)

# Normalize the degree values
df$degree_normalized <- normalize(df$degree)

df_avg <- df %>%
  group_by(Year) %>%
  summarise(Avgdegree = mean(degree_normalized))

# Create the plot with aggregated data and remove the legend
g4 <- ggplot(df, aes(x = Year, y = degree_normalized, fill = Year)) +
  geom_point(position = position_jitter(width = 0.25), size = 1, alpha = 0.5, aes(color = Year)) +  # For the points
  geom_line(data = df_avg, aes(x = as.numeric(Year), y = Avgdegree, group = 1), color = "red", size = 1) +
  geom_violin() +    # For the violin plot
  labs(x = "Year", y = "Degree (normalized)") +
  ggtitle("All") +
  theme_minimal() +
  expand_limits(y = 0) +  # Ensure y-axis starts at 0
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +  # Updated y breaks for normalized data
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))  # Center the title
g4
#####################################################################################
# Assuming g1, g2, g3, and g4 are your ggplot objects
combined_plot <- (g1 + g2 + g3 + g4) + 
  plot_layout(ncol = 2) + 
  plot_annotation(title = "Degree Distribution within DINGO Networks, 1981-2015", 
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))  # Centering the title and making it bold

print(combined_plot)

