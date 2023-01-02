# Read in raw data
strategy <- read_csv("rps_2.txt", col_names = F)

# Convert the raw data from a data frame to a vector
strategy_2 <- as.vector(unlist(strategy))
# Split the two characters from the raw data into separate columns
split <- strsplit(strategy_2, " ")

# Designate the first row as the opponent's choice
opp <- unlist(lapply(split, `[[`, 1))
# Designate the second row as our choice
me <- unlist(lapply(split, `[[`, 2))

# Create a data frame with the opponent's choice vs our choice
strategy_clean <- cbind.data.frame(opp, me)

# Rename the opponent's move so it matches X, Y, Z format
# A = X (rock), B = Y (paper), C = Z (scissors)
strategy_clean$opp[strategy_clean$opp == "A"] <- "X"
strategy_clean$opp[strategy_clean$opp == "B"] <- "Y"
strategy_clean$opp[strategy_clean$opp == "C"] <- "Z"

# Designate points based on our choice in the round
# rock = 1 pt, paper = 2 pts, scissors = 3 pts
strategy_clean$initial_pts <- ifelse(strategy_clean$me == "X", 1,
  ifelse(strategy_clean$me == "Y", 2, 3)
)



# Determine if we win, lose, or draw vs opponent for all games played
for (i in 1:nrow(strategy_clean)) {
  if (strategy_clean$opp[i] == strategy_clean$me[i]) {
    strategy_clean$outcome[i] <- 3 # draw game (3 pts)
  } else {
    if (strategy_clean$opp[i] == "X" && strategy_clean$me[i] == "Z") {
      strategy_clean$outcome[i] <- 0 # rock beats scissors (0 pts)
    } else {
      if (strategy_clean$opp[i] == "Y" && strategy_clean$me[i] == "X") {
        strategy_clean$outcome[i] <- 0 # paper beats rock (0 pts)
      } else {
        if (strategy_clean$opp[i] == "Z" && strategy_clean$me[i] == "Y") {
          strategy_clean$outcome[i] <- 0 # scissors beat paper (0 pts)
        } else {
          strategy_clean$outcome[i] <- 6 # all other options are a win (6 pts)
        }
      }
    }
  }
}

# Calculate the total points from each round (i.e. initial pts + match outcome)
strategy_clean <- strategy_clean |>
  mutate(total = initial_pts + outcome)

# Sum the total points
sum(strategy_clean$total) ## Answer = 10941


## Part II: ##

## Second Column:
# X means we need to lose,
# Y means we need to end the round in a draw, and
# Z means we need to win

# Create new data frame for the new rules
strategy_new <- strategy_clean |>
  select(opp, me)

# X beats Z
# Y beats X
# Z beats Y

for (j in 1:nrow(strategy_new)) {
  if (strategy_new$me[j] == "Y") { 
    # Game must end in draw. Our move will be the same as opponent's move
    strategy_new$turn[j] <- strategy_new$opp[j]
    # Draw game (3 pts)
    strategy_new$match_pt[j] <- 3 
  } else {
    if (strategy_new$me[j] == "X" && strategy_new$opp[j] == "X") {
      # Game must end in L. Opponent chooses rock, we choose scissors
      strategy_new$turn[j] <- "Z"
      # Loss (0 pts)
      strategy_new$match_pt[j] <- 0
    } else {
      if (strategy_new$me[j] == "X" && strategy_new$opp[j] == "Y") {
        # Game must end in L. Opponent chooses paper, we choose rock
        strategy_new$turn[j] <- "X"
        # Loss (0 pts)
        strategy_new$match_pt[j] <- 0
      } else {
        if (strategy_new$me[j] == "X" && strategy_new$opp[j] == "Z") {
          # Game must end in L. Opponent chooses scissors, we choose paper
          strategy_new$turn[j] <- "Y"
          # Loss (0 pts)
          strategy_new$match_pt[j] <- 0
        } else {
          if (strategy_new$me[j] == "Z" && strategy_new$opp[j] == "X") {
            # Game must end in W. Opponent chooses rock, we choose paper
            strategy_new$turn[j] <- "Y"
            # Win (6 pts)
            strategy_new$match_pt[j] <- 6
          } else {
            if (strategy_new$me[j] == "Z" && strategy_new$opp[j] == "Y") {
              # Game must end in W. Opponent chooses paper, we choose scissors
              strategy_new$turn[j] <- "Z"
              # Win (6 pts)
              strategy_new$match_pt[j] <- 6
            } else {
              # Game must end in W. Opponent chooses scissors, we choose rock
              strategy_new$turn[j] <- "X"
              # Win (6 pts)
              strategy_new$match_pt[j] <- 6
            }
          }
        }
      }
    }
  }
}

# Check data frame to make sure data is printing as expected
head(strategy_new)

# Designate points based on our choice in the round
# rock = 1 pt, paper = 2 pts, scissors = 3 pts
strategy_new$turn_pts <- ifelse(strategy_new$turn == "X", 1,
  ifelse(strategy_new$turn == "Y", 2, 3)
)

# Calculate the total points from each round (i.e. initial pts + match outcome)
strategy_new <- strategy_new |>
  mutate(total_pts = match_pt + turn_pts)


# Sum the total points
sum(strategy_new$total_pts) ## Answer = 13071

