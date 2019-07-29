trees #open the trees dataset
class(trees) #checks to see if its a dataframe
help(trees) #more information on the data
attach(trees) 

#Information on the data
Girth
Height
Volume

#checks the class type of the given column
class(Girth)
#counts how many values in the dataset or column
nrow(trees)
length(Girth)
#plot the distribution
plot(trees)
#plots the correlation
cor(trees)

#summary statistics
summary(trees)

#linear model function and summary of the linear model function
volume.lm = lm(Volume ~ Girth + Height, data = trees)
summary(volume.lm)

#plot the volume  column and values
plot(Volume)
#plot the value with a linear model on the y axis
plot(Volume, ylim = c(0, 80))
#fit the initial points of the volume plot
points(fitted(volume.lm), pch=16)

#plot the residuals of the volumne linear model
plot(residuals(volume.lm), type = "h")
abline(0, 0)

#histogram plot of the linear model
hist(residuals(volume.lm))

#density plot of the linear model
plot(density(residuals(volume.lm)))

#add a normal curve onto the density plot to see the variance
lines(seq(-15, 15, .01), dnorm(seq(-15, 15, .01),
mean(residuals(volume.lm)), sd(residuals(volume.lm))), col="red")
