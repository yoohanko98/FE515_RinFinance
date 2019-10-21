# Assignment 3
# "I pledge my honor I have abided by the Stevens Honor System." -YHK
# FE 515 R in Finance
# - Yoohan Ko
# Fall 2019, Stevens Institute of Technology

# =============================================================================

# Question 1: Quantmod package (30 points)
# Quantmod is a powerful package when downloading daily equity data. During the
# class time, I only present part of the functions from this package. To solve 
# this question, you are required to use functions from this package.


# 1. Download 1 year length data for an equity (You can select any equity you 
# like), set the start time as Jan. 1st, 2018 and end time as Dec 31st, 2018

library(quantmod)

getSymbols(Symbols = "AMD", 
           from = "2018-01-01", 
           to = "2018-12-31")

# 2. Calculate daily return using functions from this package. In this 
# question, you shall calculate both simple return and log return. 
# (The two return sequences will be used in the next question.)

# calculating simple return by using periodReturn()
amd.simple.rtn <- data.frame(dailyReturn(AMD$AMD.Adjusted, 
                                        type = 'arithmetic'))

# calculating log return by specifying type = log
amd.log.rtn <- data.frame(dailyReturn(AMD$AMD.Adjusted, 
                                      type = 'log'))

# manual way to calculate simple and log returns (used for verifying results)
# amd = data.frame(AMD)
# head(amd)
# amd.price <- amd$AMD.Adjusted
# 
# amd.pt <- amd.price[2:length(amd.price)]
# amd.pt1 <- amd.price[1: (length(amd.price)-1) ]
# 
# amd.simple.rtn <- (amd.pt - amd.pt1) / amd.pt1
# amd.log.rtn <- log(amd.pt) - log(amd.pt1)


# 3. Use chartseries() to add a Relative Strength Index line one the equity you
# selected. When visualize this index, please use the default setting. Later, 
# explain how to use this index in equity trading.

chartSeries(AMD, theme = chartTheme('white'))
addRSI()

# =============================================================================

# Question 2: Basic statistics value and self-defined function (20 points)
# In this question, you need to design a self-defined function to calculate 
# the first moment up to the fourth moment for the return sequences you 
# obtained from Question 1. To help you build this function, please following 
# instructions:

# 1. Your input should be a vector. Other than this format, your function 
# should stop working and send the user a warning message.

# 2. Calculate the first moment up to fourth moment for your input.

# 3. The object you want to return should be a “report”, this report should 
# contain values from step 2. Meanwhile, you should tell the user the data 
# is left skew or right skew, heavy tail or short tail.

amd.simple.rtn.vec <- amd.simple.rtn$daily.returns
amd.log.rtn.vec <- amd.log.rtn$daily.returns

getMoment <- function(vec.in) {
  if (is.vector(vec.in)) {
    print("Function input is a vector. Continuing function...")
  } else {
    stop("Function input is not a vector. Stopping...")
  }
  
  # first moment: Mean
  amd.simple.rtn.1 <- mean(amd.simple.rtn.vec)
  amd.log.rtn.1 <- mean(amd.log.rtn.vec)

  
  # second moment: Variance
  amd.simple.rtn.2 <- var(amd.simple.rtn.vec)
  amd.log.rtn.2 <- var(amd.simple.rtn.vec)

  
  # third moment: Skewness
  library(timeDate)
  amd.simple.rtn.3 <- skewness(amd.simple.rtn.vec)
  amd.log.rtn.3 <- skewness(amd.simple.rtn.vec)

  
  # fourth moment: Kurtosis
  amd.simple.rtn.4 <- kurtosis(amd.simple.rtn.vec)
  amd.log.rtn.4 <- kurtosis(amd.log.rtn.vec)
  
  # generating report for simple return
  print("=============== Generating report for simple return ===============")
  print(paste("The mean for the simple return is:", amd.simple.rtn.1))
  print(paste("The variance for the simple return is:", amd.simple.rtn.2))
  if(amd.simple.rtn.3 < 0) {
    print(paste("The data for the simple return is left skewed"))
  } else {
    print(paste("The data for the simple return is right skewed"))
  }
  if(amd.simple.rtn.4 > 3) {
    print(paste("The data for the simple return has a heavy tail"))
  } else {
    print(paste("The data for the simple return has a short tail"))
  }
  
  # generating report for log return
  print("================ Generating report for log return ================")
  print(paste("The mean for the log return is:", amd.simple.rtn.1))
  print(paste("The variance for the log return is:", amd.log.rtn.2))
  if(amd.log.rtn.3 < 0) {
    print(paste("The data for the log return is left skewed"))
  } else {
    print(paste("The data for the log return is right skewed"))
  }
  if(amd.log.rtn.4 > 3) {
    print(paste("The data for the log return has a heavy tail"))
  } else {
    print(paste("The data for the log return has a short tail"))
  }
  
  
}

getMoment(amd.log.rtn.vec)


# =============================================================================

# Question 3: Data Visualization (50 points)
# In Question 1, you are required to generate the Relative Strength Index using
# the function from Quantmod. In this question, you are required to generate 
# this index by yourself without using any package. Please use the same equity 
# data you downloaded in Question 1.

# 1. The final goal is generating a figure which contains two plots. One for 
# the equity price movement. Another one for the index. Make sure you have 
# proper title, x-label, y-label, legend and etc. in your plot.



# 2. Investigate how to calculate the RSI index in detail. Make sure you will 
# have the index value for all trading days between Jan. 1st, 2018 and 
# Dec 31st, 2018. (Hint: you may need extra equity data to replicate the same 
# result as you have in Question 1)



# 3. Based on the index value you get, how many times you observe a strong 
# selling signal (higher than 70)? How many times you observe a strong buying 
# signal? (lower than 30)










