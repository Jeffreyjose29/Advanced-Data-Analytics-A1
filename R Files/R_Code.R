####################################### STATS321 ASSIGNMENT ONE ######################################
              ############################ JEFFREY JOSE #################################

######### QUESTION ONE ##########

### PART A ###

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
    currentPoint <- randomWalk[i] #set the current point to the current point of the random walk
  }
  print(vec) #print the final vector showing all the points taken in the 100 steps
  #plot the randomly generated steps as a line plot
  plot(randomWalk, type = "l", main = paste("Random Walks Generated From ", numSteps, " Steps"))
}

### PART B ###

#Calling the above function with a 100 steps each with a maximum size of 1 from each current position
generateWalkFunc(100, 1)


### PART C ###

#Similiar function to the one defined above
generateWalkFuncPartC <- function(numSteps, stepRange){
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
    currentPoint <- randomWalk[i] #set the current point to the current point of the random walk
  }
  #print(vec) #print the final vector showing all the points taken in the 100 steps
  #plot the randomly generated steps as a line plot
  plot(randomWalk, type = "l", main = paste("Random Walks Generated From ", numSteps, " Steps"))
  
  return(tail(vec, n = 1)) #return the last position of each vector
}

testC <- c() #empty vector to hold all the 1000 final positions
for(iterations in 1:1000){
  #iterate through 1000 function calling and append each final position onto testC
  testC <- append(testC, generateWalkFuncPartC(50, 5), after = length(testC))
}
print(testC) #print the final vector containing the 1000 final positions

### PART D ###

#Plot the density graph of the 1000 final positions with a defined normal curve superimposed
plot(density(testC), main = "Density Curve Of the Final Points")
curve(dnorm(x, mean=0, sd=5 * sqrt(50/3)),  col="red", lwd=2, add=TRUE, yaxt="n")



######### QUESTION TWO ##########

### Initial Setup ###

getwd() #get the current working directory
setwd('C:/Users/Jeffrey Jose/Desktop/Stats321 A1/R Files') #set the working directory to save and open files to and from
#read the csv file into a dataframe object called 'blood.df'
blood.df <- read.csv(file = 'C:/Users/Jeffrey Jose/Desktop/Stats321 A1/R Files/blood.csv', header = TRUE, sep = ',')
head(blood.df) #show the first 6 rows of the dataset
colnames(blood.df) #show all the column headers in the dataset

#Check the data types to ensure everything is right
typeof(blood.df$Sex) 
typeof(blood.df$Age)
typeof(blood.df$RedBCC)
typeof(blood.df$Haemoglobin)

### PART A ###

#Ask Bob whether sex is required within this plot as it is categorical
pairs(~Sex + Age + RedBCC + Haemoglobin, data = blood.df, main = "Simple Scatterplot Between Pairs Of Variables Given In The Dataset")

### PART B ###

boxplot(Haemoglobin ~ Sex, data = blood.df, main = "Haemoglobin Difference In Males And Females", xlab = "Sex", 
        ylab = "Haemoglobin Levels (g/dl)", col = "orange", border = "brown")

### PART C ###

mod1 <- lm(Haemoglobin ~ Sex, data = blood.df)
summary(mod1)
#Releveling to make "M" the baseline category
table(blood.df$Sex)
blood.df$Sex <- relevel(blood.df$Sex, ref = "F")

### Part D ###

fit.lm <- lm(Haemoglobin ~ Sex + RedBCC + Sex:RedBCC, data = blood.df)
summary(fit.lm)

### Part E ###

par(mfrow = c(2,2))
plot(fit.lm)

### Part F ###

par(mfrow = c(1,1))
#Visually look at the distribution of the residuals of the model
hist(residuals(fit.lm))
boxplot(residuals(fit.lm))
shapiro.test(residuals(fit.lm)) #shapiro wilks test to gain the p-value to test for the assumption of normaility of the residuals.

### Part G ###

newRedBCC = seq(3.5, 6, length = 100)
newHaemoglobinF = predict(fit.lm, data.frame(RedBCC = newRedBCC, Sex = "F"))
newHaemoglobinM = predict(fit.lm, data.frame(RedBCC = newRedBCC, Sex = "M"))

plot(newRedBCC, newHaemoglobinF, main="Predicted Haemoglobin Levels", ylab="Haemoglobin", xlab="RedBCC")
points(newRedBCC, newHaemoglobinM, col="red", pch=2)
legend(3.5,18,c("Female","Male"), col=c("black", "red"), pch=c(1,2))

### Part H ###
## Q1 ##

X <- model.matrix(fit.lm) #the model matrix of the linear model
y <- c()
y <- c(blood.df$Haemoglobin) #vector of haemoglobin values

#Estimates of the least squares estimates of the regression coefficient 
coef(fit.lm)

## Q2 ##

#Creating the hat matrix
#Unsure so need to ask Bob if done correctly?
H = X%*%(solve(t(X)%*%X)%*%t(X)) 
nrow(H)
ncol(H)
diag(H)[1]
print(H[1:3, 1:3]) #print first 3 rows and columns of the hat matrix

## Q3 ##

#transpose of the hat matrix
t(H) * y
