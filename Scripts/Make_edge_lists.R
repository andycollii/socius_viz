########################################################################
# Load the required library for data manipulation
########################################################################

library(dplyr)
library(igraph)

########################################################################
# Load and format the main dataframe
########################################################################

df <- read.csv("/Users/ac/Desktop/Academic/JessKim/DINGOs_org_032023.csv")

#Subset the dataframe
df <- subset(df, demcat1 %in% c("Rule of law"))

# Drop the specified columns from the dataframe, we don't need them
df <- df %>%
  select(-c(cties1, cties2, cties3, dirdem, id, direct_promotion, demcat1, democracy, name, yrfound, plfound, aims, activities, orgtype, ltrcode, cat1, demcat_rob, demsocialism, christdem))

#Drop rows containing ALL NA values
df <- df %>%
  filter(rowSums(is.na(.)) != ncol(.))

########################################################################
#Break into year so that we can make a graph of each year separately
########################################################################

df1981 <- df %>%
  filter(year == 1981) %>%
  select(-year) %>%
  filter(rowSums(is.na(.)) != ncol(.))

df1985 <- df %>%
  filter(year == 1985) %>%
  select(-year) %>%
  filter(rowSums(is.na(.)) != ncol(.))

df1990 <- df %>%
  filter(year == 1990) %>%
  select(-year) %>%
  filter(rowSums(is.na(.)) != ncol(.))

df1995 <- df %>%
  filter(year == 1995) %>%
  select(-year) %>%
  filter(rowSums(is.na(.)) != ncol(.))

df2000 <- df %>%
  filter(year == 2000) %>%
  select(-year) %>%
  filter(rowSums(is.na(.)) != ncol(.))

df2005 <- df %>%
  filter(year == 2005) %>%
  select(-year) %>%
  filter(rowSums(is.na(.)) != ncol(.))

df2010 <- df %>%
  filter(year == 2010) %>%
  select(-year) %>%
  filter(rowSums(is.na(.)) != ncol(.))

df2015 <- df %>%
  filter(year == 2015) %>%
  select(-year) %>%
  filter(rowSums(is.na(.)) != ncol(.))

########################################################################
#Remove and COLUMNS (countries) containing ONLY NA values
#This should drop countries that didn't exist in a given year

########################################################################

# Remove and COLUMNS (countries) containing ONLY NA values
# This should drop countries that didn't exist in a given year
df1981 <- df1981[, colSums(is.na(df1981)) != nrow(df1981)]
df1985 <- df1985[, colSums(is.na(df1985)) != nrow(df1985)]
df1990 <- df1990[, colSums(is.na(df1990)) != nrow(df1990)]
df1995 <- df1995[, colSums(is.na(df1995)) != nrow(df1995)]
df2000 <- df2000[, colSums(is.na(df2000)) != nrow(df2000)]
df2005 <- df2005[, colSums(is.na(df2005)) != nrow(df2005)]
df2010 <- df2010[, colSums(is.na(df2010)) != nrow(df2010)]
df2015 <- df2015[, colSums(is.na(df2015)) != nrow(df2015)]

#Transpose
df1981 <- t(df1981)
df1985 <- t(df1985)
df1990 <- t(df1990)
df1995 <- t(df1995)
df2000 <- t(df2000)
df2005 <- t(df2005)
df2010 <- t(df2010)
df2015 <- t(df2015)

########################################################################
#Looking for any more NA values
########################################################################

check_NA_values <- function(df) {
  any_na <- any(is.na(df))
  if (any_na) {
    cat("The dataframe contains", sum(is.na(df)), "missing values.\n")
  } else {
    cat("The dataframe does not contain any missing values.\n")
  }
}
check_NA_values(df1981)
check_NA_values(df1985)
check_NA_values(df1990)
check_NA_values(df1995)
check_NA_values(df2000)
check_NA_values(df2005)
check_NA_values(df2010)
check_NA_values(df2015)

#######################################################################
# Get list of all countries from each year
#######################################################################

# Convert matrices to dataframes
df1981 <- as.data.frame(df1981)
df1985 <- as.data.frame(df1985)
df1990 <- as.data.frame(df1990)
df1995 <- as.data.frame(df1995)
df2000 <- as.data.frame(df2000)
df2005 <- as.data.frame(df2005)
df2010 <- as.data.frame(df2010)
df2015 <- as.data.frame(df2015)

# Get the list of countries from each dataframe
country_list_1981 <- rownames(df1981)
country_list_1985 <- rownames(df1985)
country_list_1990 <- rownames(df1990)
country_list_1995 <- rownames(df1995)
country_list_2000 <- rownames(df2000)
country_list_2005 <- rownames(df2005)
country_list_2010 <- rownames(df2010)
country_list_2015 <- rownames(df2015)

# Find the common countries across all dataframes
common_countries <- Reduce(intersect, list(country_list_1981, country_list_1985, country_list_1990, 
                                           country_list_1995, country_list_2000, country_list_2005, 
                                           country_list_2010, country_list_2015))

# Subset each dataframe to include only common countries
df1981 <- df1981[rownames(df1981) %in% common_countries, ]
df1985 <- df1985[rownames(df1985) %in% common_countries, ]
df1990 <- df1990[rownames(df1990) %in% common_countries, ]
df1995 <- df1995[rownames(df1995) %in% common_countries, ]
df2000 <- df2000[rownames(df2000) %in% common_countries, ]
df2005 <- df2005[rownames(df2005) %in% common_countries, ]
df2010 <- df2010[rownames(df2010) %in% common_countries, ]
df2015 <- df2015[rownames(df2015) %in% common_countries, ]

# Identify rows in each dataframe that contain all 0s
zero_rows_1981 <- rownames(df1981)[rowSums(df1981 != 0) == 0]
zero_rows_1985 <- rownames(df1985)[rowSums(df1985 != 0) == 0]
zero_rows_1990 <- rownames(df1990)[rowSums(df1990 != 0) == 0]
zero_rows_1995 <- rownames(df1995)[rowSums(df1995 != 0) == 0]
zero_rows_2000 <- rownames(df2000)[rowSums(df2000 != 0) == 0]
zero_rows_2005 <- rownames(df2005)[rowSums(df2005 != 0) == 0]
zero_rows_2010 <- rownames(df2010)[rowSums(df2010 != 0) == 0]
zero_rows_2015 <- rownames(df2015)[rowSums(df2015 != 0) == 0]

# Combine all zero rows
all_zero_rows <- unique(c(zero_rows_1981, zero_rows_1985, zero_rows_1990, zero_rows_1995, zero_rows_2000, zero_rows_2005, zero_rows_2010, zero_rows_2015))

# Remove these rows from each dataframe
df1981 <- df1981[!rownames(df1981) %in% all_zero_rows, ]
df1985 <- df1985[!rownames(df1985) %in% all_zero_rows, ]
df1990 <- df1990[!rownames(df1990) %in% all_zero_rows, ]
df1995 <- df1995[!rownames(df1995) %in% all_zero_rows, ]
df2000 <- df2000[!rownames(df2000) %in% all_zero_rows, ]
df2005 <- df2005[!rownames(df2005) %in% all_zero_rows, ]
df2010 <- df2010[!rownames(df2010) %in% all_zero_rows, ]
df2015 <- df2015[!rownames(df2015) %in% all_zero_rows, ]


########################################################################
#Get countries for each year
########################################################################

# Create a list to store column names (country names) for each year
country_names <- list()

# Store column names for each year in the list
country_names[[1]] <- colnames(df1981)
country_names[[2]] <- colnames(df1985)
country_names[[3]] <- colnames(df1990)
country_names[[4]] <- colnames(df1995)
country_names[[5]] <- colnames(df2000)
country_names[[6]] <- colnames(df2005)
country_names[[7]] <- colnames(df2010)
country_names[[8]] <- colnames(df2015)

########################################################################
#Create square adjacency matrix for each year
########################################################################

df1981_adj <- as.matrix(df1981) %*% t(as.matrix(df1981))
diag(df1981_adj) <- 0

df1985_adj <- as.matrix(df1985) %*% t(as.matrix(df1985))
diag(df1985_adj) <- 0

df1990_adj <- as.matrix(df1990) %*% t(as.matrix(df1990))
diag(df1990_adj) <- 0

df1995_adj <- as.matrix(df1995) %*% t(as.matrix(df1995))
diag(df1995_adj) <- 0

df2000_adj <- as.matrix(df2000) %*% t(as.matrix(df2000))
diag(df2000_adj) <- 0

df2005_adj <- as.matrix(df2005) %*% t(as.matrix(df2005))
diag(df2005_adj) <- 0

df2010_adj <- as.matrix(df2010) %*% t(as.matrix(df2010))
diag(df2010_adj) <- 0

df2015_adj <- as.matrix(df2015) %*% t(as.matrix(df2015))
diag(df2015_adj) <- 0

########################################################################
#Create a function to get edge lists
########################################################################

create_edge_list <- function(adj_matrix) {
  # Extract the row and column names from the adjacency matrix
  row_names <- rownames(adj_matrix)
  col_names <- colnames(adj_matrix)
  
  # Create an empty edge list as a data frame
  edge_list <- data.frame(from = character(),
                          to = character(),
                          weight = numeric(),
                          stringsAsFactors = FALSE)
  
  # Loop through the adjacency matrix and add edges to the edge list
  for (i in 1:nrow(adj_matrix)) {
    for (j in 1:ncol(adj_matrix)) {
      # Extract the weight at the current position in the adjacency matrix
      weight <- adj_matrix[i, j]
      
      # If the weight is not zero, add an edge to the edge list
      if (weight != 0) {
        # Create a new row for the edge list
        new_row <- data.frame(source = row_names[i],
                              target = col_names[j],
                              weight = weight)
        
        # Append the new row to the edge list
        edge_list <- rbind(edge_list, new_row)
      }
    }
  }
  
  # Return the edge list
  return(edge_list)
}

########################################################################
#Get edge lists
########################################################################

dir_path <- "/Users/ac/Desktop/Academic/JessKim/ElectionsSubset"

# Create edge lists and save as CSV files
edge_list1981 <- create_edge_list(df1981_adj)
write.csv(edge_list1981, file = file.path(dir_path, "edge_list1981.csv"), row.names = FALSE)

edge_list1985 <- create_edge_list(df1985_adj)
write.csv(edge_list1985, file = file.path(dir_path, "edge_list1985.csv"), row.names = FALSE)

edge_list1990 <- create_edge_list(df1990_adj)
write.csv(edge_list1990, file = file.path(dir_path, "edge_list1990.csv"), row.names = FALSE)

edge_list1995 <- create_edge_list(df1995_adj)
write.csv(edge_list1995, file = file.path(dir_path, "edge_list1995.csv"), row.names = FALSE)

edge_list2000 <- create_edge_list(df2000_adj)
write.csv(edge_list2000, file = file.path(dir_path, "edge_list2000.csv"), row.names = FALSE)

edge_list2005 <- create_edge_list(df2005_adj)
write.csv(edge_list2005, file = file.path(dir_path, "edge_list2005.csv"), row.names = FALSE)

edge_list2010 <- create_edge_list(df2010_adj)
write.csv(edge_list2010, file = file.path(dir_path, "edge_list2010.csv"), row.names = FALSE)

edge_list2015 <- create_edge_list(df2015_adj)
write.csv(edge_list2015, file = file.path(dir_path, "edge_list2015.csv"), row.names = FALSE)




