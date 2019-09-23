# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 

# RANDOM WALK MODEL!!!!!!! ####
random.walk.model <-
  function(samples, drift = 0.0125, sdrw = 0.3, criterion = 4.921) {
    theNumber <- 0
    howMany <- 0
    accuracy.array <- numeric()
    rt.array <- numeric()
    
    # repeat for number of samples
    for (i in 1:samples) {
      theNumber <- 0 # start walk at 0
      howMany <- 0 # start counter at 0
      
      # one sample
      while ((theNumber <= criterion) && (theNumber >= (0 - criterion))) {
        theNumber <- (theNumber + rnorm(1, mean = drift, sd = sdrw)) # add to "theNumber," to keep track of walk
        howMany <- (howMany + 1) # increment counter by 1
      }
      
      # output correct or not
      if(theNumber >= criterion) {
        accuracy.array[i] <- TRUE
      }
      else {
        accuracy.array[i] <- FALSE
      }
      
      # output how many
      rt.array[i] <- howMany
    }
    
    output <- data.frame(correct = accuracy.array,
                         rt = rt.array)
    
    return(output)
  }

# test the model ####
library(dplyr)
randomwalk.test <- random.walk.model(1000)

sum(randomwalk.test$correct) / length(randomwalk.test$correct) # should be close to 0.5
mean(randomwalk.test$rt) # should be about 112

walkcorrect.data <- randomwalk.test %>% filter(correct == TRUE)
walkincorrect.data <- randomwalk.test %>% filter(correct == FALSE)
mean(walkcorrect.data$rt) # correct mean
mean(walkincorrect.data$rt) # incorrect mean

#ACCUMULATOR MODEL!!!!!!! ####
accumulator.model <- function(samples, rate.1=82.5, rate.2=89, criterion=3){
  theNumber.1 <- 0 # accumulator 1
  theNumber.2 <- 0 # accumulator 2
  howMany <- 0 # number of samples
  
  # initialize arrays
  accuracy.array <- numeric()
  rt.array <- numeric()
  
  # repeat for number of samples
  for (i in 1:samples) {
    theNumber.1 <- 0 # start walk.1 at 0
    theNumber.2 <- 0 # start walk.1 at 0
    howMany <- 0 # start counter at 0
    
    # one sample
    while ((theNumber.1 <= criterion) && (theNumber.2 <= criterion)) {
      theNumber.1 <- (theNumber.1 + rexp(1, rate.1)) # add to "theNumber.1," to keep track of walk
      theNumber.2 <- (theNumber.2 + rexp(1, rate.2)) # add to "theNumber.1," to keep track of walk
      howMany <- (howMany + 1) # increment counter by 1
    }
    
    # output correct or not
    if(theNumber.1 >= criterion || theNumber.1 >= theNumber.2) {
      accuracy.array[i] <- TRUE
    }
    else {
      accuracy.array[i] <- FALSE
    }
    
    # output how many
    rt.array[i] <- howMany
  }
  
  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}

# test the model ####
acc.test <- accumulator.model(1000)
acccorrect.data <- acc.test %>% filter(correct == TRUE)
accincorrect.data <- acc.test %>% filter(correct == FALSE)
# MEAN: should be close to 0.8
sum(acc.test$correct) / length(acc.test$correct)
# correct mean
mean(acccorrect.data$rt)
# incorrect mean
mean(accincorrect.data$rt)


# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:


# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.
