#Question 1
#Initialising a function that generates a random walk
#@params numSteps - number of steps, stepRange - maximum size of step
generateWalkFunc <- function(numSteps, stepRange){
  #define the initial starting point
  initial.value <- 0
  #set the current point before the first iteration to the starting point
  #the randomWalk always starts at 0 (initial value)
  currentPoint <- initial.value
  randomWalk <- initial.value
  vec <- vector()
  #for loop which starts at 2 because our stating point is already initialised
  #iterate through until we complete the desired number of steps
  for(i in 2:numSteps){
    #The current position of the walk is the position of the previous point + a 
    #random value taken from a uniform distribution where the range varies from -stepRange to + stepRange.
    randomWalk[i] <- randomWalk[i - 1] + runif(1, -stepRange, stepRange)
    if(i == 2){
      #print(initial.value) #The first value always printed is the initial position (when i is equal to 2 since
      vec <- c(vec, initial.value)
      # thats where the for loop starts)
    }else{
      #print(currentPoint + c(0, cumsum(randomWalk[i])))
      #print(randomWalk[i])
      vec <- c(vec, randomWalk[i])
    }
    currentPoint <- randomWalk[i]
  }
  print(vec)
  plot(randomWalk, type = "l", main = paste("Random Walks Generated From ", numSteps, " Steps"))
}
#Calling the above function with a 100 steps each with a maximum size of 1 from each current position
generateWalkFunc(100, 1)
