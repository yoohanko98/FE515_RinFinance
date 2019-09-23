# Assignment 1
# Yoohan Ko
# "I pledge my honor I have abided by the Stevens Honor System."
# FE 515 R in Finance
# Fall 2019, Stevens Institute of Technology

# Question 1: 
# This is the first assignment, the idea is helping students get familiar with 
# coding in R. In this assignment, please do following things:
# - You have to apply ”Google R style” in this assignment
# - You have to explain your code for Q2 and Q3 (When leaving comments in the R
# code, please use #)



# Question 2: Self-defined function
# 2520 is the smallest number that can be divided by each of the numbers from 
# 1 to 10 without any remainder. Now you need to create a function to find the 
# smallest positive number that is evenly divisible by two numbers you input 
# into the function. (For example, your input is 6 and 9, you need to find 
# the smallest number which can be divided by 6, 7, 8 and 9)

# This is essentially a question asking us to find LCM of these numbers
# I decided that the easiest way to implement this is to recursively call LCM

# This is a function that takes two inputs and returns the LCM
#https://www.datamentor.io/r-programming/examples/least-common-multiple/
TwoLCM <- function(x, y){
  if (x > y) {
    greater = x
  } else {
    greater = y
  }
  while(TRUE) {
    # print("running two LCM function")
    # print(x, y)
    if ((greater %% x == 0) && (greater %% y == 0)) {
      num.lcm = greater
      break
    }
    greater = greater + 1
  }
  return (num.lcm)
}

# this function runs LCM recursively of temp and i + 1
MultipleLCM <- function(low, high){
  temp <- 0
  for(i in low:high - 1){
    # print(i)
    if(i == low){
      temp <- TwoLCM(i, i + 1)
    }
    if(temp != 0){
      temp <- TwoLCM(temp, i + 1)
    }
    # print("Running multiple LCM")
    # print(temp)
    # print(i+1)
  }
  return(temp)
}

# VectorMultiple.LCM <- function()

# testing the function
MultipleLCM(1, 2) # correct answer is 2

MultipleLCM(6, 9) # correct answer is 504

MultipleLCM(1, 10) #correct answer is 2520

MultipleLCM(6, 12) # correct answer is 27720

# Question 3: “apply” function
# Download JPM.csv from canvas and read this table in R using command. 

# Importing data set provided
setwd(paste("/Users/yoohanko98/OneDrive - stevens.edu/Stevens/Semester 7/
            FE 515 (R in Finance)/HW/FE515_RinFinance/HW1", sep=""))
jpm2018 = read.csv("JPM.csv", header = TRUE)

# print out the first row to check headers
head(jpm2018, 1)
typeof(jpm2018)

# For this table, name it as JPM2018 and do following things:
# - Create a sub-table which only contains Open, High, Low and Close
sub.jpm2018 <- jpm2018[, 2:5, ]

# - Using sapply() we mentioned in class to calculate mean value for each 
# column and save it as a vector
m1.jpm2018 <- sapply(jpm2018[, 2:7], mean)
m1.jpm2018
typeof(m1.jpm2018)

# - Using apply() we mentioned in class to calculate mean value for each row 
# and save it as a 3 by 5 matrix, the data should be assigned by row.

# creating a temporary vector to store mean of rows from JPM2018
temp.m2 <- apply(jpm2018[1:15, 2:7], 1, mean)

# arranging data from temp.M2 into a 3 x 5 matrix by row
m2 = matrix(temp.m2, nrow = 3, ncol = 5, byrow = TRUE)
m2

# Question 4: Self-study
# 1. What’s the difference between ”mapply” and ”lapply”?

# Answer: 
# mapply can be used to call a function for multiple vectors/lists at one index
# at a time. This functionality allows mapply to be described as multivariate.

# 2. How to use ”mapply”? Write an example.

t1 <- 1:4
t2 <- 5:8

#This would perform: 1+5, 2+6, 3+7, 4+8
mapply(sum, t1, t2)

# 3. Can you use ”mapply” to the function you created in Question 2?
# If yes, assign two vectors as inputs for the self-defined function.

t3 <- 5:9
t4 <- 10:14
# Performs LCM of # between 5 and 10, then LCM of # between 6 and 11, etc.
mapply(MultipleLCM, t3, t4)

# Question 5: Loops and paste()
# Download Dow30.csv and SP100.csv and do the following:

dow30 = read.csv("Dow30.csv", header = TRUE)
tick.dow30 = dow30[, 2]
# head(DOW30, 1)
sp100 = read.csv("SP100.csv", header = TRUE)
tick.sp100 = sp100[, 1]
# head(SP100, 1)

# These two files are the list of constituents of S&P100 and the list of Dow 
# Jones. You need to find out the index of each Dow company in S&P100 company 
# list. Don’t make any change in the tables. You must choose a loop to finish 
# this question.
# For example, ”MMM” is the first symbol in Dow30.csv, and it is 67th in the
# table of S&P100. Output example:
# "MMM--66"
# "AXP--11"
# "AAPL--1"
# ...

for(i in tick.dow30){
  tick.temp <- i
  counter <- 1
  for(j in tick.sp100){
    if(tick.temp == j){
      output <- paste(j, counter, sep = "--")
      print(output)
    }
    counter <- counter + 1
  }
}

# In order to accomplish this task, you need to study how to use paste() 
# by yourself. (Additional information: For your convenient, I already 
#               delete some Dow companies which are not listed in S&P 100. 
#               Therefore, it is not a fully list.)






