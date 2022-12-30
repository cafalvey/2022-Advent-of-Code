library(stringr)
setwd("/Users/carolinefalvey/Desktop/Duke")

# Read in data from website:
data <- read_csv("compartments.txt", col_names = F)

# Convert data to vector & initialize matrix for split data
data <- as.vector(unlist(data))
split <- matrix(NA, nrow = length(data), ncol = 2)

# Split strings in half to create 1st and 2nd compartments
for (i in seq_along(data)) {
  split[i, 1] <- substr(data[i], 1, nchar(data[i]) / 2)
  split[i, 2] <- substr(data[i], nchar(data[i]) / 2 + 1, nchar(data[i]))
}

# View original and split data
view(cbind.data.frame(data, split))

# Initialize vector to store repeated letters
letter <- rep(NA, nrow(split))


for (j in 1:nrow(split)) {
  # Split strings into separate letters
  test1 <- unlist(strsplit(split[j, 1], split = ""))
  test2 <- unlist(strsplit(split[j, 2], split = ""))

  # Confirm which letter is repeated in each string
  key <- test1 %in% test2 
  
  # Store repeated letter into letter vector
  letter[j] <- test1[which(key)]
}

# Combine original data, split data, and repeated letter
full_set <- cbind.data.frame(data, split, letter)

## Instructions: ##
# To help prioritize item rearrangement, every item type can be converted to a priority:
# - Lowercase item types a through z have priorities 1 through 26.
# - Uppercase item types A through Z have priorities 27 through 52.

# Combine lower case and upper case letter sets A-Z
ids <- c(letters, LETTERS)

for (k in 1:nrow(full_set)) {
  # Calculate priority for each compartment based off repeated letter
  full_set$priority[k] <- which(full_set$letter[k] == ids)
}


# Sum priorities
sum(full_set$priority) ## Answer 7428



#### Part II ####

# Convert original data into a 3x100 matrix
matrix <- matrix(data, nrow = 3, ncol = length(data) / 3, byrow = F)
# Initialize vector to store new repeated letters
new_letters <- rep(NA, ncol(matrix))


for (n in 1:ncol(matrix)) {
  # Split strings into separate letters
  set1 <- unlist(strsplit(matrix[1, n], split = ""))
  set2 <- unlist(strsplit(matrix[2, n], split = ""))
  set3 <- unlist(strsplit(matrix[3, n], split = ""))

  # Confirm which letters are repeated in 1st string and 2nd string
  key1 <- unique(set1[which(set1 %in% set2)])
  # Confirm which letters from above are repeated in 3rd string
  key2 <- key1[which(key1 %in% set3)]
  # Store repeated letter in new letter vector
  new_letters[n] <- key2
}

# Initialize new priority vector
new_priority <- rep(NA, length(new_letters))


for (m in seq_along(new_letters)) {
  # Calculate priority for each compartment based off repeated letter
  new_priority[m] <- which(new_letters[m] == ids)
}


# Sum priorities
sum(new_priority) ## Answer: 2650

