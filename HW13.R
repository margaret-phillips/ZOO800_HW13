# Maggie Phillips
# Zoo 800
# HW 13

library(tidyverse)

##-------------- Objective 1 -------------------------##
"Estimate the parameters of the linear regression of area burned on dragon size using the
analytical solution to the linear regression (equation in lecture)."

# first, load in the csv with dragon data
dragons<- read.csv("dragon_data.csv")

#first plot the data so that I can visualize slope, intercept, etc

ggplot(data= dragons, mapping= aes(size, acres_on_fire))+
  geom_point()+
  geom_smooth(method = "lm")

#need to make a matrix with the data and then do the next part:
x<- cbind(1, dragons$size) #creating matrix with column of 1 for intercept
y<- cbind(dragons$acres_on_fire) #creating matrix for y (dependent variable)

#the analytical solution does reverse matrix multiplication to get B
b <- solve(t(x) %*% x) %*% t(x) %*% y
print(b)

##------------- Objective 2 ---------------------------##
"Estimate the parameters of the linear regression of area burned on dragon size using ordinary
least squares based on:
a. A grid search (estimate the slope and intercept to the nearest 0.1)
b. Optimization using the optim() function
c. Verify that the optimization routine converged and is not sensitive to starting values"

"Brute force – grid search
• loop over a range of plausible values of the intercept and (in a nested loop) the slope
• save the sum of squared errors for each pair of values in a matrix
• find the cell in the matrix with the minimum value and extract the corresponding values of
the intercept and slope
• if greater precision is desired, repeat the steps above using a finer grid centered near the
values found in the first iteration"

################ part a #######################################################


#combine x and y into a matrix
x_y<- cbind(x, y)

#make a sequence for slope and for intercept and use those to loop
slope<- seq(.4, 2.0, by= 0.01) #ended up making the steps smaller bc intercept was missed the first time
intercept<- seq(-1.6, 0, by= 0.01)

#loop over intercept and slope (nested) to calculate sum of squares for each x, y in matrix
#create matrix to store the results of grid search:
# Extract x and y from matrix
x_vals <- as.numeric(x_y[, 2]) #pulling out values for x and y
y_vals <- as.numeric(x_y[, 3]) #setting as numeric just in case values were getting distorted

#initialize results for storage in loop below!
results <- matrix(nrow = length(slope) * length(intercept), ncol = 3)
colnames(results) <- c("slope", "intercept", "sum_sq")
counter <- 1

# Loop over slope and intercept
for (s in slope) {
  for (i in intercept) {
    y_fit <- (s * x_vals) + i
    sum_sq <- sum((y_vals - y_fit)^2)
    
    results[counter, ] <- c(s, i, sum_sq)
    counter <- counter + 1
  }
}

# Convert to data frame
results_df <- as.data.frame(results)

# View best fits
head(results_df[order(results_df$sum_sq), ])
ss<- min(results_df$sum_sq) #this is slope of 1.35 and intercept of -1.49

#write a print statement here to pull out values based on index

######### the grid search yields a slope of 1.35 and an intercept of -1.49

####################### part b ######################################

# Objective function: sum of squared residuals
ssq <- function(par, x_vals, y_vals) {
  slope <- par[1]
  intercept <- par[2]
  y_fit <- slope * x_vals + intercept
  sum((y - y_fit)^2)
}
# Initial guesses for slope and intercept
init <- c(1, 0)   # slope = 1, intercept = 0

# Run optimization
fit <- optim(par = init, fn = ssq, x = x_vals, y = y_vals)

fit$par      # optimized slope and intercept
fit$value    # minimized sum of squares

###### this method yields a slope of 1.35 and an intercept of -1.37

################### part c ################################################




##-------------- Objective 3 ---------------------------##
"Estimate the parameters of the linear regression of area burned on dragon size using maximum
likelihood based on:
a. A grid search (estimate the slope and intercept to the nearest 0.1)
b. Optimization using the optim() function
c. Verify that the optimization routine converged and is not sensitive to starting values"

"Try all combinations of plausible values for the parameters
• Calculate the negative log likelihood for each one
• Find the parameter combination that minimizes this value"

############################ part a #########################################

#need to search for slope, intercept, and variance (plug this in from objective 2)

slope<- seq(.4, 2.0, by= 0.01) #just going to use the same seqs as before
intercept<- seq(-1.6, 0, by= 0.01)

results_obj3 <- data.frame(slope=numeric(), intercept=numeric(), nll=numeric())

for (s in slope) {
  for (i in intercept) {
    y_fit <- s * x_vals + i
    resid <- y_vals - y_fit
    n <- length(y_vals)
    
    #can get variance from least sum of squares in obj 2
    sigma2 <- ss/ ((length(x)-1)) #variance is sum of squares/ n-1
    
    # negative log-likelihood
    nll <- (n/2) * log(2*pi*sigma2) + sum(resid^2) / (2*sigma2)
    
    results_obj3 <- rbind(results_obj3, data.frame(slope=s, intercept=i, nll=nll))
  }
}

#top 5 best fit combinations
results[order(results_obj3$nll), ][1:5, ]

#####this grid search yields a slope of 1.35 and an intercept of -1.49

################# part b ###################################################



################# part c ##################################################

##----------------------- Objective 4 ----------------------------##

"A. Compare the slope and intercept estimates from the three approaches. Are they the same?
(They should be very close)
"