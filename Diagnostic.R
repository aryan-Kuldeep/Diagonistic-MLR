library(ISLR2)

#ImPort dataset
Dataset <- Boston
Dataset

# Checking NULL values
is.na(Boston)
sum(is.na(Boston))

# Response - medv, Regressor - crim, dis , rm , rad.
# now we have 16 model and we choode best fit model according to adj r^2 prediction.

lm.fit1 <- lm(medv ~ crim + dis + rm + rad, data = Dataset)
lm.fit2 <- lm(medv ~ crim + dis + rm, data = Dataset)
lm.fit3 <- lm(medv ~ dis + rm + rad, data = Dataset)
lm.fit4 <- lm(medv ~ rm + rad + crim, data = Dataset)
lm.fit5 <- lm(medv ~ rad + crim + dis, data = Dataset)
lm.fit6 <- lm(medv ~ crim + dis , data = Dataset)
lm.fit7 <- lm(medv ~ crim + rm, data = Dataset)
lm.fit8 <- lm(medv ~ crim + rad, data = Dataset)
lm.fit9 <- lm(medv ~ dis + rm , data = Dataset)
lm.fit10 <- lm(medv ~ dis + rad, data = Dataset)
lm.fit11 <- lm(medv ~ rm + rad , data = Dataset)
lm.fit12 <- lm(medv ~ crim, data = Dataset)
lm.fit13 <- lm(medv ~ dis, data = Dataset)
lm.fit14 <- lm(medv ~ rm , data = Dataset)
lm.fit15 <- lm(medv ~rad, data = Dataset)


summary(lm.fit1)$adj.r.squared
summary(lm.fit2)$adj.r.squared
summary(lm.fit3)$adj.r.squared
summary(lm.fit4)$adj.r.squared
summary(lm.fit5)$adj.r.squared
summary(lm.fit6)$adj.r.squared
summary(lm.fit7)$adj.r.squared
summary(lm.fit8)$adj.r.squared
summary(lm.fit9)$adj.r.squared
summary(lm.fit10)$adj.r.squared
summary(lm.fit11)$adj.r.squared
summary(lm.fit12)$adj.r.squared
summary(lm.fit13)$adj.r.squared
summary(lm.fit14)$adj.r.squared
summary(lm.fit15)$adj.r.squared

# So our best fit model is : lm.fit4
lm.fit4
summary(lm.fit4)

# So we perform diagnosis on model 4

# 1. Influential point (outliers) : we will check with the help of cooks distance.

cooks_distace <- cooks.distance(lm.fit4)
for(i in 1 : 506){
  if( cooks_distace[i] > 4/(502)){
    print(cooks_distace[i])
  }
}

# Now we will check this with the help of dffits.

df.fits <- dffits(lm.fit4)
for(i in 1 : 506){
  if(df.fits[i] > 4/502){
    print(df.fits[i])
  }
}

# 2. Now we will check that errors are uncorelated or not, with the help of Darwin Watson Test.
library(DescTools)
DurbinWatsonTest(lm.fit4,alternative = "less")
DurbinWatsonTest(lm.fit4,alternative = "greater")
DurbinWatsonTest(lm.fit4,alternative = "two.sided") # Errors are positively correlated.

# 3. Now we check if the data is hetroscadasticity or not .

plot(lm.fit4 $ fitted.values, lm.fit4 $ residuals, xlab = "FittesValues", ylab = "Residuals")

# We can see our data is homoscadasticity.

# 4. Now we will check normality assumption i.e. whether residuals are normaly distributed or not

qqnorm(rstudent(lm.fit4))
qqline(rstudent(lm.fit4),col="blue")
shapiro.test((rstudent(lm.fit4)))
 
# Residuals are not normally distributed.

# 5. Let's us check linearity assumption's. 

library(car)
avPlots(lm.fit4) # linearity assumption is valid as we're getting slope in each graph 

# 6. Now we will check Mullticolinearity.
library(mctest)
imcdiag(lm.fit4,method = "VIF") # Mulliticolinearity is not present in this model.

















