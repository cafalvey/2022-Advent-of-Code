## Part I:

# Read in raw data
sections <- read_csv("sections.txt", col_names = F)
# Rename the columns for clarity
colnames(sections) <- c("elf1", "elf2")


# Split the assignments by the dash to get the first and last
# section assignment for each elf
elf1_split <- strsplit(sections$elf1, "-")
elf2_split <- strsplit(sections$elf2, "-")

# Create new variables for first & last sections for elf 1
elf1_low <- as.numeric(unlist(lapply(elf1_split, `[[`, 1)))
elf1_upp <- as.numeric(unlist(lapply(elf1_split, `[[`, 2)))

# Create new variable for first & last sections for elf 2
elf2_low <- as.numeric(unlist(lapply(elf2_split, `[[`, 1)))
elf2_upp <- as.numeric(unlist(lapply(elf2_split, `[[`, 2)))


# Create a new data frame with the initial assignments &
# the upper and lower sections for both elves
sections_clean <- cbind.data.frame(
  sections,
  elf1_low, elf1_upp,
  elf2_low, elf2_upp
)

# Initialize a result vector
result_vec <- rep(NA, nrow(sections_clean))



for (i in 1:nrow(sections_clean)) {
  if (sections_clean$elf1_low[i] <= sections_clean$elf2_low[i] &&
    sections_clean$elf1_upp[i] >= sections_clean$elf2_upp[i]) {
    # Determine if the 2nd elf's sections are contained in the 1st elf's sections
    result_vec[i] <- "2 in 1"
  } else {
    if (sections_clean$elf2_low[i] <= sections_clean$elf1_low[i] &&
      sections_clean$elf2_upp[i] >= sections_clean$elf1_upp[i]) {
      # Determine if the 1st elf's sections are contained in the 2nd elf's sections
      result_vec[i] <- "1 in 2"
    } else {
      # If neither of the above, indicate that the assignment was done correctly
      result_vec[i] <- "correct"
    }
  }
}


# Subtract the number of correct assignments from the total number of
# assignments to get the total number of incorrect assignments
length(result_vec) - sum(result_vec == "correct")
## Answer 462


## Part II:

# Initialize a second result vector
result_vec_2 <- rep(NA, nrow(sections_clean))

for (j in 1:nrow(sections_clean)) {
  # Create a vector of all sections sections assigned to elf 1
  elf1_total <- elf1_low[j]:elf1_upp[j]
  # Create a vector of all sections assigned to elf 2
  elf2_total <- elf2_low[j]:elf2_upp[j]

  # Determine if there are any overlapping sections
  duplicates <- sum(elf1_total %in% elf2_total)

  # If there are any duplicates, result = T
  # If there are not duplicates, result = F
  result_vec_2[j] <- ifelse(duplicates > 0, T, F)
}

# Sum the result vector to calculate the number of assignments w duplicates
sum(result_vec_2)
## Answer: 835



