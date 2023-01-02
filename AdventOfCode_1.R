library(tidyverse)

# Read in raw data with empty rows
input <- read_csv("input.txt", skip_empty_rows = F, col_names = F)

# Create a key to indicate which rows divide the data (indicated by NA)
key <- is.na(input)

# Calculate the total number of elves (i.e. number of divisions + 1)
elves <- sum(is.na(input)) + 1


# Create a key for which rows are NA (i.e. where the breaks in the list are)
key_vec <- which(key)

key_vec_first <- c(0, key_vec + 1)
key_vec_last <- c(key_vec - 1, (nrow(input) - 1))

# Create an empty vector to store results
result_vec <- rep(NA, elves)

# Convert raw data from data frame to numeric vector
input_2 <- as.vector(unlist(input), mode = "numeric")


for (j in 1:elves) {
  # Divide each group based on where the NA rows are
  group <- input_2[key_vec_first[j]:key_vec_last[j]]
  # Calculate the number of calories consumed by each group
  calories <- sum(group)
  # Store the total number of calories for the group in the result vector
  result_vec[j] <- calories
}


# Take the maximum value of the result vector for answer
max(result_vec) ## Answer: 74711




## Part II:

# Find the 3 elves with the most calories & sum their amounts:
sum((sort(result_vec, decreasing = T))[1:3]) ## Answer: 209481
