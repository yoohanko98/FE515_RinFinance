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

PrimeFactorization <- function() {
  
  if (n > 2) {
    numvec <- numeric()
    while(n %% 2 == 0){
      numvec = c(numvec, 2)
      n = n/2
    }
    i = 3
    while(n != 1) {
      while(n %% i == 0) {
        numvec = c(numvec, i)
        n = n / i
      }
      i = i + 2
    }
  }
  
  
}

get_prime_factors <- function() {
  num <- as.numeric(readline(prompt="Enter number: " ))
  n <- num
  if (n > 2) {
    numvec <- numeric()
    while(n %% 2 == 0){
      numvec = c(numvec, 2)
      n = n/2
    }
    i = 3
    while(n != 1) {
      while(n %% i == 0) {
        numvec = c(numvec, i)
        n = n/i
      }
      i = i + 2
    }
    sprintf("All Prime Factors of %d are:%s", num, paste0(sort(numvec), collapse = ","))
  }
  else {
    stop("Try a bigger number")
  }
}





# Question 3: “apply” function
# Download JPM.csv from canvas and read this table in R using command. 

# Importing data set provided
setwd("/Users/yoohanko98/OneDrive - stevens.edu/Stevens/Semester 7/FE 515 (R in Finance)/HW/FE515_RinFinance/HW1")
JPM2018 = read.csv("JPM.csv", header = TRUE)

# print out the first row to check headers
head(JPM2018, 1)
typeof(JPM2018)

# For this table, name it as JPM2018 and do following things:
# - Create a sub-table which only contains Open, High, Low and Close
Sub.JPM2018 <- JPM2018[, 2:5, ]

# - Using sapply() we mentioned in class to calculate mean value for each 
# column and save it as a vector
M1.JPM2018 <- sapply(JPM2018[, 2:7], mean)
M1.JPM2018
typeof(M1.JPM2018)

# - Using apply() we mentioned in class to calculate mean value for each row 
# and save it as a 3 by 5 matrix, the data should be assigned by row.
temp.M2 <- apply(JPM2018[1:15, 2:7], 1, mean)
M2 = matrix(temp.M2, nrow = 3, ncol = 5, byrow = TRUE)
M2

# Question 4: Self-study
# 1. What’s the difference between ”mapply” and ”lapply”?

# mapply can be used to call a function for multiple vectors/lists at one index
# at a time. This functionality allows mapply to be described as multivariate.

# 2. How to use ”mapply”? Write an example.


# 3. Can you use ”mapply” to the function you created in Question 2?
# If yes, assign two vectors as inputs for the self-defined function.
# If not, explain why.




# Question 5: Loops and paste()







