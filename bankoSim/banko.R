library(ggplot2)
library(dplyr)
library(magrittr)
ggplot2::theme_set(ggplot2::theme_bw())

# Generate bankoplate
generateBankoPlate <- function(){
  # Generate 27 numbers three within each group of tens
  card <- sort(
    sapply(
    split(1:90, cut(1:90,9)),sample,3
    )
    )
  dim(card) <- c(3,9)
  
  # Remove vals so we have a valid banko card
  # First row
  firstRemoveIdx <- sample(1:9, size = 4)
  card[1, firstRemoveIdx] <- NA
  # Second row
  secondRemoveIdx <- sample(1:9, size = 4)
  card[2, secondRemoveIdx] <- NA
  # Third row - make sure that there are atleast one value in each tens
  thirdRemoveIdx <- sample(dplyr::setdiff(1:9, dplyr::intersect(firstRemoveIdx, secondRemoveIdx)), size = 4)
  card[3, thirdRemoveIdx] <- NA
  card
}

# Simulation of how well the average plate performs

simulateBingo <- function(N){
# Initialize vector with information about the filling of rows
numForFirst <- numeric(N) # When have one row been filled
numForSecond <- numeric(N) # When are two rows filled
numForBingo <- numeric(N) # When are all three rows filled

# Initialtize tibble for storing information about match between rows
#and draws along with the index of each draw

matchTibble <- tibble(drawNum = 1:90,
                      matchRowOne = logical(length = 90),
                      matchRowTwo = logical(length = 90),
                      matchRowThree = logical(length = 90))

for (i in 1:N){
# Generate banko plate
plate <- generateBankoPlate()
# Generate draws
draws <- sample(1:90, 90, replace = FALSE)

# Vectorized search over each row - save in matchtibble
matchTibble$matchRowOne <- (draws %in% plate[1, ])
matchTibble$matchRowTwo <- (draws %in% plate[2, ])
matchTibble$matchRowThree <- (draws %in% plate[3, ])

# Find latest value that has a match for each row
numForRowOne <- matchTibble %>% filter(matchRowOne == TRUE) %>% .$drawNum %>% tail(n = 1)
numForRowTwo <- matchTibble %>% filter(matchRowTwo == TRUE) %>% .$drawNum %>% tail(n = 1)
numForRowThree <- matchTibble %>% filter(matchRowThree == TRUE) %>% .$drawNum %>% tail(n = 1)

# Collect the indices
idxOfRow <- c(numForRowOne, numForRowTwo, numForRowThree)

# Find the index for when the last and first row was found
# and then use that to find out when the second was found
numForBingo[i] <- max(idxOfRow)
numForFirst[i] <- min(idxOfRow)
numForSecond[i] <- dplyr::setdiff(idxOfRow, c(numForBingo[i], numForFirst[i]))
}
# Gather information from numerical vectors in tibble
tibble(oneRow = numForFirst, twoRows = numForSecond, bingo = numForBingo)
}

result <- simulateBingo(2500)

# Bingo distribution and mean
ggplot(result, aes(x = bingo)) +
  geom_bar(aes(y=..count../sum(..count..)), fill = "dodgerblue3", col = "black") +
  xlab("Number of samples") + ylab("Probability")

mean(result$bingo)

# First row distribution and mean
ggplot(result, aes(x = oneRow)) +
  geom_bar(aes(y=..count../sum(..count..)), fill = "dodgerblue3", col = "black") +
  xlab("Number of samples") + ylab("Probability")

mean(result$oneRow)

# Second row distribution and mean
ggplot(result, aes(x = twoRows)) +
  geom_bar(aes(y=..count../sum(..count..)), fill = "dodgerblue3", col = "black") +
  xlab("Number of samples") + ylab("Probability")

mean(result$twoRows)

# Simulate games of bingo with one winner and looks of distribution of winner
gameNum <- 200 # Number of games 

numForFirstWin <- numeric(gameNum) # When have one row been filled winner
numForSecondWin <- numeric(gameNum) # When are two rows filled winner
numForBingoWin <- numeric(gameNum) # When are all three rows filled winner

for (j in 1:gameNum){
print(j)
plateNum <- 25 # Number of players
draws <- sample(1:90, 90, replace = FALSE)
matchTibble <- tibble(drawNum = 1:90,
                      matchRowOne = logical(length = 90),
                      matchRowTwo = logical(length = 90),
                      matchRowThree = logical(length = 90))

numForFirst <- numeric(plateNum) # When have one row been filled
numForSecond <- numeric(plateNum) # When are two rows filled
numForBingo <- numeric(plateNum) # When are all three rows filled

for (i in 1:plateNum){
  plate <- generateBankoPlate()
  matchTibble$matchRowOne <-  (draws %in% plate[1, ])
  matchTibble$matchRowTwo <- (draws %in% plate[2, ])
  matchTibble$matchRowThree <- (draws %in% plate[3, ])
  
  numForRowOne <- matchTibble %>% filter(matchRowOne == TRUE) %>% .$drawNum %>% tail(n = 1)
  numForRowTwo <- matchTibble %>% filter(matchRowTwo == TRUE) %>% .$drawNum %>% tail(n = 1)
  numForRowThree <- matchTibble %>% filter(matchRowThree == TRUE) %>% .$drawNum %>% tail(n = 1)
  
  # Collect the indices
  idxOfRow <- c(numForRowOne, numForRowTwo, numForRowThree)
  
  # Find the index for when the last and first row was found
  # and then use that to find out when the second was found
  numForBingo[i] <- max(idxOfRow)
  numForFirst[i] <- min(idxOfRow)
  numForSecond[i] <- dplyr::setdiff(idxOfRow, c(numForBingo[i], numForFirst[i]))
}

numForBingoWin[j] <- min(numForBingo)
numForSecondWin[j] <- min(numForSecond)
numForFirstWin[j] <- min(numForFirst)
}

# Gather information from numerical vectors in tibble
resultWin <- tibble(oneRowWinner = numForFirstWin,
                    twoRowsWinner = numForSecondWin,
                    bingoWinner = numForBingoWin)

# Winner information

# Bingo distribution and mean
ggplot(resultWin, aes(x = bingoWinner)) +
  geom_bar(aes(y=..count../sum(..count..)), fill = "dodgerblue3", col = "black") +
  xlab("Number of samples") + ylab("Probability")

mean(resultWin$bingoWinner)

# First row distribution and mean
ggplot(resultWin, aes(x = oneRowWinner)) +
  geom_bar(aes(y=..count../sum(..count..)), fill = "dodgerblue3", col = "black") +
  xlab("Number of samples") + ylab("Probability")

mean(resultWin$oneRowWinner)

# Second row distribution and mean
ggplot(resultWin, aes(x = twoRowsWinner)) +
  geom_bar(aes(y=..count../sum(..count..)), fill = "dodgerblue3", col = "black") +
  xlab("Number of samples") + ylab("Probability")

mean(resultWin$twoRowsWinner)
