# Assignment 2
# "I pledge my honor I have abided by the Stevens Honor System." -YHK
# FE 515 R in Finance
# - Yoohan Ko
# Fall 2019, Stevens Institute of Technology

# =============================================================================

# Question 1: Geometric distribution (20 points)

# 1. Explain what is dgeom() and pgeom(). 
# Create a simple question by yourself and answer it with these two functions

# Q:  There is a box that contains marbles that are black and white. 
#     There are 3 black marbles for each white marble.
#     What is the probability of picking 4 marbles in a row that are not white?

dgeom(x = 4, prob = 0.25)
# A:   0.07910156
#     The dgeom() function calculates the probability mass function (p.m.f.) 
#     of a geometric distribution. It is the probability of x failures prior 
#     to the first success


# Q:  What is the probability to choose a black marble in 4 or less picks 
#     before choosing a white marble?

pgeom(q = 4, prob = .25)
# A:  0.7626953
#     The pgeom() function calculates the cumulative distribution function
#     (c.d.f.) of a geometric distribution. It returns p in F(x) = p
#     In this case, the result of pgeom() is telling us that the probability to
#     have a non-white marble in 4 or less picks is ~0.762

# 2.  Explain what is qgeom(). Plug-in two numbers and explain the answer.
#     The qgeom() provides the quantile function. 
#     It returns the closest X such that F(x) >= p. This is the inverse of pgeom

qgeom(p = 0.7626, prob = 0.25)
# A:  In this case, we are plugging in p = 0.7626 and prob = 0.25 , which is 
#     the result of the pgeom() from the question above. This qgeom function
#     is used where to find: how many trials does it take to choose a non-white
#     marble with a white marble probability 0.25 given a resulting 
#     probability of 0.7626.

# 3. Explain what is rgeom(). 
# Generate a sequence of result using this function and explain what is the 
# meaning for them. (Requirement: show me at least 10 outputs)

temp <- rgeom(n = 100, prob = 0.5)  # 50% probability of heads
temp[1:10]
summary(temp)
# a histogram showing the number of flips before heads
hist(temp, xlab = "number of flips")

# A:  rgeom() generates a random sample from the geometric distribution
#     It calculates the time to first success in a series of independent trials
#     In this case, this is similar to flipping a coin. We are asking how many
#     repeated tries it take to get a head.

# =============================================================================

# Question 2: Data Visualization (50 points)
#   For this question, you need to download the “BA.csv”. 
#   This is a sample data set from high frequency trading. 
#   It contains two type of data: “Trade” and “Quote”. 
#   Trade data shows the historical transaction price. 
#   Quote data shows the desired price from traders.

rm(list = ls())

# Importing data set provided
BA = read.csv("HW2/BA.csv", header = TRUE)
is(BA)
head(BA)

# 1. Use subset() function to get a sub-table which contains only “Quote” data

?subset
# https://www.r-bloggers.com/5-ways-to-subset-a-data-frame-in-r/
# Making of subset that only contains rows in which the Type matches "Quote"
BA.quote <- subset(BA, Type == "Quote")
head(BA.quote) # Display the first few rows of BA.quote
is(BA.quote)
# 2. Combine the “Date.L.” column and “Time.L.” column. 
# Then, use strptime() to convert it into a time object.

# paste is used to combine two character strings
BA.quote$combinedtime <- paste(BA.quote$Date.L., BA.quote$Time.L.)

?strptime
head(BA.quote$combinedtime)

# set the number of digits in seconds to 6 to not lose any information
options(digits.secs = 6)

# specify the format of the string input to be year, month, time, HM, 
# seconds (6 decimals)
BA.quote$combinedtime <- strptime(BA.quote$combinedtime, "%Y%m%d %H:%M:%OS")

# 3. Find out the last bid price and ask price in each minute. 
# (e.x., from 9:30.000 to 9:30.999 you observe 200 records in total. 
# The last record contains the last bid/ask price in this minute.)

library("lubridate")

BA.quote2 <- BA.quote[0, ]
# iterate over the length of the list using a for loop. If the minute changes 
# from one entry to the next, add the previous line to BA.quote2 (which stores
# only the last bid price & ask price bid in each minute) 
for (iterate in 2:nrow(BA.quote)) {
  
  # checking to see if minute value has changed from the previous line
  if (minute(BA.quote$combinedtime[iterate]) != 
      minute(BA.quote$combinedtime[iterate - 1])) {
    
    # if the minute is found to have changed add the previous line to BA.quote2
    BA.quote2 <- rbind(BA.quote2, BA.quote[iterate-1,])
    
    # print(BA.quote[iterate-1,])
  }
}

# preview of BA.quote2
head(BA.quote2)
  

# 4. Plot the bid price and ask price line using black color and fill the 
# bid-ask spread using blue color. Make sure your plot has the right X-label, 
# Y-label. Legend is not required.

par(mfrow=c(1,1))
plot(1:nrow(BA.quote2), BA.quote2$Bid.Price, 
     xlab = "Minute", ylab = "Last price",
     type = "l")
lines(BA.quote2$Ask.Price)

polygon(x = c(c(1:nrow(BA.quote2)), rev(c(1:nrow(BA.quote2)))),
        y = c(BA.quote2$Ask.Price, rev(BA.quote2$Bid.Price)),
        col = "blue",
        border = NA)

# polygon(x = BA.quote2$Ask.Price,
#         # y = c(BA.quote2$Ask.Price, rev(BA.quote2$Bid.Price)),
#         col = "blue", 
#         border = NA)
# polygon(x = BA.quote2$Bid.Price,
#         # y = c(BA.quote2$Ask.Price, rev(BA.quote2$Bid.Price)),
#         col = "white", 
#         border = NA)

# =============================================================================

# Question 3: Simulation and For loop (30 points)
# Assuming there are two boxes, box A containing one black and one white marble
# The other contains two black and one white marble. 
# Now, I will give you one of the box randomly and you need to take one marble
# outside the box. What is the probability that the marble is a black one?
# Please simulate this game for 1000 times and calculate the probability based 
# on your observations.

# Blacks stores the number of Black marbles found in the 1000 trials
Blacks <- 0
result.Vec <- NULL
for (picks in 1:1000) {
  # box = c(1, 0), 1 means box A, 0 means box B
  box.choice <- sample(x = c(1, 0), size = 1, replace = T, prob = c(0.5, 0.5))

  # 1 mean black marble, 2 means white marble
  if (box.choice == 1) {
    tmp <- sample(x = c(1, 0), size = 1, replace = T, prob = c(0.5, 0.5))
  } else {
    tmp <- sample(x = c(1, 0), size = 1, replace = T, prob = c(2/3, 1/3))
  }
  # add tmp to number of black marble, if tmp = 1..., if tmp = 0, ...
  Blacks <- Blacks + tmp
  result.Vec <- c(result.Vec,Blacks/picks)
  
# theoretical probability of white: 
#   1/2 (box) * 1/2 (prob.) + 1/2 (box) * 1/3 (prob.) = 0.417
# theoretical probability of black: 
#   1/2 (box) * 1/2 (prob.) + 1/2 (box) * 2/3 (prob.) = 0.583
}
plot(1:1000, result.Vec, type = "l")

# dividing the number of Black marbles by the number of total picks
prob.test <- Blacks / picks

# resulting probability of simulating this game 1000 times ~0.59
prob.test

# =============================================================================

# Bonus Question (15 points)
# - Calculate theoretical value of the probability that the marble is a black 
# one based on the setting from Question 3. (5 points)

# theoretical probability of black: 
#   1/2 (box) * 1/2 (prob.) + 1/2 (box) * 2/3 (prob.) = 0.583

# - Given condition that the marble you obtained is a white one. 
# What is the probability that you take this marble from box A? 
# The theoretical value is 0.6, please verify this answer using R code. 
# (10 points)
# (Hint: A good simulation requires a large sample observation)

Blacks <- 0
result.Vec <- NULL
for (picks in 1:10000) {
  # box = c(1, 0), 1 means box A, 0 means box B
  box.choice <- sample(x = c(1, 0), size = 1, replace = T, prob = c(0.5, 0.5))
  
  # 1 mean black marble, 2 means white marble
  if (box.choice == 1) {
    tmp <- sample(x = c(1, 0), size = 1, replace = T, prob = c(0.5, 0.5))
  } else {
    tmp <- sample(x = c(1, 0), size = 1, replace = T, prob = c(2/3, 1/3))
  }
  # add tmp to number of black marble, if tmp = 1..., if tmp = 0, ...
  Blacks <- Blacks + tmp
  result.Vec <- c(result.Vec,Blacks/picks)

}
# plot(1:1000, result.Vec, type="l")

# dividing the number of White marbles by the number of total picks
prob.test <- (1 - Blacks / picks)

# resulting probability of simulating this game 1000 times
prob.test

prob.boxA <- (0.5 * 0.5) / test.prob
prob.boxA
# by testing, we can see that the value we obtain in 0.59
# This is very close to the theoretical value of 0.6
