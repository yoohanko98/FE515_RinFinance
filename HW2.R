# Assignment 2
# Yoohan Ko
# "I pledge my honor I have abided by the Stevens Honor System." -YHK
# FE 515 R in Finance
# Fall 2019, Stevens Institute of Technology

# Question 1: Geometric distribution (20 points)

# 1. Explain what is dgeom() and pgeom(). 
  # Create a simple question by yourself and answer it with these two functions

# the dgenom function calculates the probability mass function (p.m.f.) of a geometric distribution


# 2. Explain what is qgeom(). Plug-in two numbers and explain the answer.


# 3. Explain what is rgeom(). 
  # Generate a sequence of result using this function and explain what is the meaning for them. 
  # (Requirement: show me at least 10 outputs)




# Question 2: Data Visualization (50 points)
  # For this question, you need to download the “BA.csv”. 
  # This is a sample data set from high frequency trading. 
  # It contains two type of data: “Trade” and “Quote”. 
  # Trade data shows the historical transaction price. 
  # Quote data shows the desired price from traders.

# Importing data set provided
# setwd(paste("/Users/yoohanko98/OneDrive - stevens.edu/Stevens/Semester 7/
#             FE 515 (R in Finance)/HW/FE515_RinFinance/HW2", sep=""))
BA = read.csv("HW2/BA.csv", header = TRUE)
is(BA)
head(BA)

# 1. Use subset() function to get a sub-table which contains only “Quote” data.

?subset
# https://www.r-bloggers.com/5-ways-to-subset-a-data-frame-in-r/
BA.quote <- subset(BA, Type == "Quote")
head(BA.quote)

# 2. Combine the “Date.L.” column and “Time.L.” column. 
  # Then, use strptime() to convert it into a time object.

# paste is used to combine two character strings
BA.quote$combinedtime <- paste(BA.quote$Date.L., BA.quote$Time.L.)

?strptime
head(BA.quote$combinedtime)

# set the number of digits in seconds to 6 to not lose any information
options(digits.secs = 6)

BA.quote$combinedtime <- strptime(BA.quote$combinedtime, "%Y%m%d %H:%M:%OS")


# 3. Find out the last bid price and ask price in each minute. 
  # (e.x., from 9:30.000 to 9:30.999 you observe 200 records in total. The last record contains the last bid/ask price in this minute.)


# 4. Plot the bid price and ask price line using black color and fill the bid-ask spread using blue color. 
  # Make sure your plot has the right X-label, Y-label. Legend is not required.





# Question 3: Simulation and For loop (30 points)
  # Assuming there are two boxes, box A containing one black and one white marble. The other contains two black and one white marble. 
  # Now, I will give you one of the box randomly and you need to take one marble outside the box. What is the probability that the marble is a black one?
  # Please simulate this game for 1000 times and calculate the probability based on your observa- tions.




# Bonus Question (15 points)
# • Calculate theoretical value of the probability that the marble is a black one based on the setting from Question 3. (5 points)


# • Given condition that the marble you obtained is a white one. What is the probability that you take this marble from box A? The theoretical value is 0.6, please verify this answer using R code. (10 points)
# (Hint: A good simulation requires a large sample observation)


