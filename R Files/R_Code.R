#Question 1
#Initialising a function that generates a random walk
#@params numSteps - number of steps, stepRange - maximum size of step
generateWalkFunc <- function(numSteps, stepRange){
  #define the initial starting point
  initial.value <- 0
  currentPoint <- initial.value
  randomWalk <- initial.value
  for(i in 2:numSteps){
    randomWalk[i] <- randomWalk[i - 1] + runif(1, -stepRange, stepRange)
    if(i == 0){
      print(initial.value)
    }else{
      #print(currentPoint + c(0, cumsum(randomWalk[i])))
      print(randomWalk[i])
    }
    currentPoint <- randomWalk[i]
  }
  plot(randomWalk, type = "l", main = paste("Random Walks Generated From ", numSteps, " Steps"))
}

generateWalkFunc(100, 1)


#EXAMPLE OF STACKOVERFLOW
GenerateRandomWalk <- function(k = 250,initial.value = 0) {
  # Add a bionomial at each step
  samples = rbinom(k,1,0.5)
  samples[samples==0] = -1
  initial.value + c(0, cumsum(samples))
}
GenerateRandomWalk(10,0)