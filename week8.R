#Simple example using the women dataset
str(women)

#Aiming to predict from height
#Dependant variable = weight
# Indepedant variable = height

simple_linear_model <- lm(weight ~ height, data = women)
simple_linear_model

summary(simple_linear_model)

plot(women$weight, women$height,
     xlab = "Height in inches",
     ylab = "Weight in lbs",
    main = "Scatter plot showing the regression line for weight
     predicted from height")
abline(simple_linear_model)

#Correaltion coefficient between the variables

# -1 = perfect negative correlation
#+1 = perfect negative correlation
# -0.2 <x<+0.2 suggests that much of the variation in the outcome varibale is not explained by the predictor

confint(simple_linear_model)

cor(women$height, women$weight)
#Model accuracy-goodness of fit
#3 quantities
#Residual standard errors(RSE)
#R-squared
#F statistic

#Residual standrd error= 1.525 = prediction error rate
#When comparing 2 models, the smallest RSE is the best fits the model
#Here, the observed values deviate from the true regression line 1.5 units on an average

#R2
#High r squared means good indicator that the model variablity in the outcome can be
# explained by the model.
# A number close to 0 = model does not explain much of the variability
#F statistic 
#Overall significance of the model
#A large F statistic corresponds to a significant p value
#Larger F statistic means significant p- value

test_data <- 120
test_data_frame <- data.frame(test_data)
predicted_weight <- predict(simple_linear_model, test_data_frame)

#Build a model to predict distance from speed
#Using the cars dataset
#1st step- Check model assumptions
#These are the core assumptions

#Linearity among the variables
#Normality
#No collinearity
#Independance

#Check linearity of the data using scatter plot
# x axis- indepedant variable
#y- axis = dependant variable
#If relationship exists then the linearity assumption is validated.

scatter.smooth(x = cars$speed, y= cars$dist, 
               main = "Distance ~ speed",
               xlab = "Car Speed",
               ylab = "Stopping distance")

cor(cars$speed, cars$dist)
pear_corr <- cor.test(cars$speed, cars$dist)
pear_corr

#Outlier = 1.5 * interquartile range
#IQR = distance between 25th and 75th percentile
#We need to check speed and distance for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
attach(cars)
boxplot(speed, 
        main= "Speed", sub = paste("Outlier rows:", boxplot.stats(speed)$out))

boxplot(dist, 
        main= "Distance", sub = paste("Outlier rows:", boxplot.stats(dist)$out))
detach(cars)
par <- opar

#Remove line 120 because it is an outlier
cars <- subset(cars, cars$dist != 120)


