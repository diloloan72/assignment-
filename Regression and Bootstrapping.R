##############
##### QUESTION 1: 
# Import the data set: 
sesame <- read.csv("https://tinyurl.com/wlgl63b") 
# Checking if there is any missing values in the "treatment" column
unique(sesame$treatment) # Only 1 and 0, no NA

## Question 1a: 
# separating treatment and control group
sesame_control = sesame[which(sesame$treatment==0),]
sesame_treated = sesame[which(sesame$treatment==1),]

# fit regression models for both groups
lm_control = lm(post.test ~ pre.test, data=sesame_control)
lm_treated = lm(post.test ~ pre.test, data=sesame_treated)

# plot the data and the regression lines
# create an empty plot with labelled axes 
plot(x= c(0:125), y=c(0:125), type='n', xlab='pre-test, x', ylab='post-test, y') 
# add data points for control group and treated group
points(sesame_control$pre.test, sesame_control$post.test, pch= 1) #hollow circle
points(sesame_treated$pre.test, sesame_treated$post.test, pch= 19) #filled-in circle 
# draw regression lines
abline(lm_control, lty=2)
abline(lm_treated)
#add legend and title of the plot
legend("bottomright",legend=c('Control group','Treated group'), 
       lty=2:1, pch = c(1,19), cex = 0.8)
title('Figure 1. Pre-test/ post-test scores of Grade 4')

## Question 1b
# Check the 11th observation in the treated group 
sesame_treated[11,]
# Change the post-test value of observation 11th in the treated group into "75"
sesame_treated[11,'post.test'] = 75
# update the regression line
lm_treated_new = lm(post.test ~ pre.test, data=sesame_treated)

# plot the data and the regression lines
# create an empty plot with all axes labelled 
plot(x=c(0:125),y=c(0:125),type='n', xlab='pre-test, x', ylab='post-test, y') 
points(sesame_control$pre.test, sesame_control$post.test, pch=1) # hollow circles
points(sesame_treated$pre.test, sesame_treated$post.test, pch=19) # filled-in circles
points(104.1, 75, col='red', pch = 19) # the modified point has pre-test = 104.1, post-test = 75
                                       # change the color of the modified point to red
# regression lines: 
abline(lm_control, lty = 2)
abline(lm_treated_new)
# add legend and tittle
legend("bottomright", legend = c('Control group','Treated group', 'Modified point'),
       col = c("black", "black", "red"), lty= 2:1:0, pch = c(1,19,19), cex = 0.8)
title('Figure 2. New Pre-test/post-test scores of Grade 4')

## Question 1c
# Load necessary library: 
library(arm)
# Fit the regression model: 
lm_1c = lm(post.test ~ treatment + pre.test + treatment:pre.test, data = sesame)
# Run simulation 20 times:
sim_1c = sim(lm_1c, n.sims=20)
sim_1c
# A properly-labeled data visualization that shows treatment effect for Grade 4 
plot(0, 0, xlim = range(sesame$pre.test), ylim=c(-5,10),
      xlab="pre-test", ylab="treatment effect",
      main="Figure 3. Treatment effect for Grade 4")
abline (0, 0, lwd=.5, lty=2)
for (i in 1:20){
  curve (coef(sim_1c)[i,2] + coef(sim_1c)[i,4]*x, lwd=.5, col="gray",
         add=TRUE)
}
curve (coef(lm_1c)[2] + coef(lm_1c)[4]*x, lwd=.5, add=TRUE)
# add legend
legend("topright",legend=c('Main regression line','Simulated Results'), 
       lty=1,col=c('black','grey'), cex = 0.8)

##############
##### Question 2: 
# load arm library for sim function 
library(arm)
# import data
tinting = read.csv(url("https://tinyurl.com/v4bq99k"))

## Question 2a:
# Fit the regression model 
lm_tint <- lm(csoa ~ age + sex + target + I(as.numeric(tint != "no")) + I(as.numeric(tint!= "no")*age), data = tinting) 

# Simulate 
set.seed(123)
iterations <- 1000
sim_tint <- sim(lm_tint, n.sims = iterations)

# Predict csoa for typical treated female units, with target = "hicon", ages = 20, 30, 40, 50, 60, 70, 80
typ_ages = c(20, 30, 40, 50, 60, 70, 80)
# create a matrix to represent treated group 
simulated.treat <- matrix(NA, nrow = iterations, ncol = length(typ_ages))

for (age in typ_ages) {
  Xs <- c(1, age, 0, 1, 1, 1*age) # 1 for intercept, age = 20, 30, 40, 50, 60, 70, 80
                                  # 0 means sex = "f", 1 means target = "hicon" 
                                  # 1 means tint!='no', which are the treated units 
  
  for (i in 1:iterations) {
    simulated.treat[i, age/10 -1] <- sum(Xs*sim_tint@coef[i,]) 
  }
}

# Table with the relevant point estimates (bounds of the prediction intervals and means of y for different ages)
conf.intervals.treat <- apply(simulated.treat, 2, quantile, probs = c(0.025, 0.975)) # 95% confidence intervals
mean.treat <- apply(simulated.treat, 2, mean) #mean
new.conf.intervals.treat <- t(data.frame(conf.intervals.treat)) # transpose rows and columns 
mean1 <- t(data.frame(mean.treat))
new.mean.treat <- t(data.frame(mean1))
table.treat <- data.frame(new.conf.intervals.treat, new.mean.treat)
colnames(table.treat) <- c("Lower Bound of expected values for csoa", "Upper Bound of expected values for csoa",
                           "Mean of expected values for csoa")
rownames(table.treat) <- typ_ages
View(table.treat)

# Plot the predicted csoa (y-axis) against age (x-axis)
plot(x = c(1:100), y = c(1:100), type = "n", 
     main = paste("Figure 4. Prediction intervals and means", "\n", "of csoa by age"), 
     xlab = "age", 
     ylab = "csoa")

for (age in typ_ages) {
  segments(
    x0 = age,
    y0 = conf.intervals.treat[1, age/10 -1],
    x1 = age,
    y1 = conf.intervals.treat[2, age/10 -1],
    lwd = 1)
  points(typ_ages, mean.treat, col = "red", pch = 20)
}

## Question 1b
# create a matrix to represent control group 
simulated.ctrol <- matrix(NA, nrow = iterations, ncol = length(ages))

for (age in typ_ages) {
  Xs <- c(1, age, 0, 1, 0, 0*age) # 1 for intercept, age = 20, 30, 40, 50, 60, 70, 80
                                  # 0 means sex = "f", 1 means target = "hicon", 
                                  # 0 means tint!='no' is FALSE, which means control group
  
  for (i in 1:iterations) {
    simulated.ctrol[i, age/10 -1] <- sum(Xs*sim_tint@coef[i,])
  }
}

# Treatment effect is the difference between treated and control group 
simulated.treat.effect = simulated.treat - simulated.ctrol

# Table with the relevant point estimates (bounds of prediction intervals and means of treatment effect for different ages)
conf.intervals.treat.effect <- apply(simulated.treat.effect, 2, quantile, probs = c(0.025, 0.975)) #95% confidence interval
mean.treat.effect <- apply(simulated.treat.effect, 2, mean) #mean
new.conf.intervals.treat.effect <- t(data.frame(conf.intervals.treat.effect)) # transpose rows and columns 
mean2 <- t(data.frame(mean.treat.effect))
new.mean.treat.effect <- t(data.frame(mean2))
table.treat.effect <- data.frame(new.conf.intervals.treat.effect, new.mean.treat.effect)
colnames(table.treat.effect) <- c("Lower Bound of expected values for treat effect", 
                                  "Upper Bound of expected values for treat effect",
                                  "Mean of expected values for treat effect")
rownames(table.treat.effect) <- typ_ages
View(table.treat.effect)

# Plot the predicted treat effect (y-axis) against age (x-axis)
plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(20,80), 
     ylim = c(-20,20),
     main = paste("Figure 5. Prediction intervals and means", "\n", "of treatment effect by age"), 
     xlab = "age", 
     ylab = "treatment effect")

for (age in typ_ages) {
  segments(
    x0 = age,
    y0 = conf.intervals.treat.effect[1, age/10 -1],
    x1 = age,
    y1 = conf.intervals.treat.effect[2, age/10 -1],
    lwd = 1)
  points(typ_ages, mean.treat.effect, col = "red", pch=20)
}

##############
## Question 3:
# R squared function 
myrsquared <- function(ytrue, ypred) {
  res <- sum((ytrue - ypred)**2) # sum of squares due to regression
  tot <- sum((ytrue - mean(ytrue))**2) # total sum of squares
  return(1-res/tot)
}

# Check the function with lalonde dataset
# load needed library
library(Matching)
# import the lalonde data set and print some rows 
data(lalonde)
head(lalonde)

# Fit the regression line:
lm.check = lm(re78 ~ age + educ, data = lalonde) #predict real earnings in 1978 by age and education
# obtain the R-squared produced by summary() function
summary(lm.check)$r.sq # 0.01646603
# obtain the R-squared produced by my function
myrsquared(lalonde$re78, lm.check$fitted.values) # 0.01646603

##############
## Question 4:
# load foreign library to read .dta file
library(foreign)
# Read the mazedata1.dta data set and print some rows
mazedata = read.dta("C:/Users/dilol/Documents/mazedata1.dta")
head(mazedata)

# Identify treatment and control group. 
# Treated group has "treatment" = "Caste Revealed", and coded as 1 
# Control group has "treatment" != "Caste Revealed", and coded as 0
mazedata$newtreat <- 0 #create a new column in the mazedata data set to quantify treatment and control
mazedata$newtreat[mazedata$treatment=="Caste Revealed"] <- 1 # if "treatment" = "Caste Revealed", 
                                                             # change the respective value in the "newtreat" column into 1
mazedata_control = mazedata[which(mazedata$newtreat == 0),]
mazedata_treated = mazedata[which(mazedata$newtreat == 1),]

# Bootstrapping function
set.seed(72)
iterations <- 10000
coef_store <- rep(NA, iterations)

# Fit the linear model predicting round1 by newly-defined treatment indicator
for (i in 1:iterations) {
  lm.maze = lm(round1 ~ newtreat, data = mazedata[sample(1:nrow(mazedata), nrow(mazedata), replace = T),])
  coef_store[i] <- lm.maze$coefficients[2] 
  #the slope represents treatment effect because it shows the difference in round1 if caste is revealed vs not
}

# Confidence interval of the treatment effect from bootstraps 
bstr.conf.interval <- quantile(coef_store, c(0.025, 0.975)) #95% confidence interval
bstr.conf.interval

# Obtain the a conventional confidence interval for the treatment effect
lm.maze = lm(round1 ~ newtreat, data = mazedata)
conventional.conf.interval <- confint(lm.maze)[2,]
conventional.conf.interval

## Question 4a:
# create a table with the relevant results (bounds on the 2 confidence intervals: one via bootstrapping, one via confint)
table.4a <- data.frame("Bootstrap Simulation CI" = bstr.conf.interval, 
           "Conventional CI" = conventional.conf.interval)
View(table.4a)

## Question 4b: 
# create a properly labelled histogram showing bootstrap-sample results
hist(coef_store, main = paste("Figure 6. Bootstrapped Values", "\n", "for Treatment Effect"), 
     xlab = "Treatment Effect", ylab = "Frequency")

##############
# Question 5:
# Obtain the dataset: 
foo <- read.csv(url("https://tinyurl.com/yx8tqf3k"))
# Randomly remove 2000 observations, and set it aside as a test set 
set.seed(12345)
test_set_rows <- sample(1:length(foo$age), 2000, replace = FALSE) 
test_foo = foo[test_set_rows,]
train_foo = foo[-test_set_rows,]

# 2 models of varying complexity for student whose surname starts with N
glm1 = glm(treat ~ education, data = train_foo)
glm2 = glm(treat ~ . -re74 + age*education, data = train_foo)

#10-fold cross validation
library(boot)
cv.10.simple = cv.glm(train_foo, glm1, K=10)$delta[1]
cv.10.complex = cv.glm(train_foo, glm2, K=10)$delta[1]

#LOOCV 
loocv.simple = cv.glm(train_foo, glm1)$delta[1]
loocv.complex = cv.glm(train_foo, glm2)$delta[1]

# Calculate the test set error:
test.simple = mean((test_foo$treat - predict.lm(glm1, test_foo))^2)
test.complex = mean((test_foo$treat - predict.lm(glm2, test_foo))^2)

# create a table that summarizes 2 10-fold CV estimates, 2 LOOCV estimates, and 2 test set error rates
table.5 <- data.frame("10-fold CV error" = c(cv.10.simple, cv.10.complex), 
                      "LOOCV error" = c(loocv.simple, loocv.complex), 
                      "Test set error" = c(test.simple, test.complex))
colnames(table.5) = c("10-fold CV error","LOOCV error","Test set error")
rownames(table.5) = c("Simple model", "Complex model")
View(table.5)

##############
# Question 6: 
trt = matrix(NA,nrow=2,ncol=7)
ctrl = matrix(NA,nrow=2,ncol=7) 

trt[,1]=c(0, 2) #18
ctrl[,1]=c(3, 10)
trt[,2]=c(0, 3) #20
ctrl[,2]=c(2, 8)
trt[,3]=c(0, 4) #22
ctrl[,3]=c(2, 7)
trt[,4]=c(1, 3) #24
ctrl[,4]=c(2, 6)
trt[,5]=c(1, 3) #26
ctrl[,5]=c(2, 5)
trt[,6]=c(1, 3) #28
ctrl[,6]=c(2, 4)
trt[,7]=c(1, 2) #30
ctrl[,7]=c(1, 3)


c1 = rgb(red = 1, green = 0, blue = 0, alpha = 0.5) #trt
c2 = rgb(red = 0, green = 0, blue = 1, alpha = 0.5) #ctrl

plot(x = c(1:100), y = c(1:100), type = "n", xlim = c(17,31), ylim = c(0,11), cex.lab=1.2,
     main = "Alcohol Consumption - 95% Prediction Intervals", xlab = "Age",ylab = "Drinks per Week")

for (age in seq(from=18,to=30,by=2)) { 
  segments(x0 = age-0.05, y0 = trt[1, (age-18)/2+1],
           x1 = age-0.05, y1 = trt[2, (age-18)/2+1],lwd = 3,col=c1)
  
  segments(x0 = age+0.05, y0 = ctrl[1, (age-18)/2+1],
           x1 = age+0.05, y1 = ctrl[2, (age-18)/2+1],lwd = 3,col=c2)
}
legend('topright',legend=c('Treatment','Control'),fill=c(c1,c2))
mtext("https://tinyurl.com/vwxuwop", side = 1, cex = 0.5, adj = 0, padj = 10)
