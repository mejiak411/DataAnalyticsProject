# Set the working directory to the folder containing Excel files
setwd("C:/Users/Kaily Mejia/Dropbox/Graduate Courses/Data Analytics/Project/Datasets/Spotify")  # Replace with your actual directory path

# List CSV files in the directory
csv_files <- list.files(pattern = "\\.csv$")

# Load necessary packages
library(dplyr)

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Read and combine the CSV files
for (file in csv_files) {
  file_data <- read.csv(file)
  combined_data <- rbind(combined_data, file_data)
}

# Write the combined data to a new CSV file
write.csv(combined_data, "spotify_combined_data.csv", row.names = FALSE)