#Regress Score on Price
lmscore_price<- lm(Score~Price, data = Cameras)
lmscore_price
plot(lmscore_price, pch=19)
#Estimate Score for a Camera Costing $70

70*0.05525+46.66880

#Find R**2 for the Above Regression
summary(lmscore_price)

#Find s for the Data Set
summary(lmscore_price)

# Standardize Residuals and See Actual Residual
rstandard(lmscore_price)
residuals(lmscore_price)

# Plot Leverge Values for the Above Regression
plot(hatvalues(lmscore_price), type = "h")

# Plot the above regression with PCH=19
plot(lmscore_price, pch=19)

# Generate a 99% Confidence Interval for a Camera Costing $250

newcamera<- data.frame(Price = 250)
predict(lmscore_price, newcamera, interval = "prediction", level=0.99)


#Generate a boxplot of student motivations and level of instructor
boxplot(S.M ~ Level, data = Facebook)

# Verify Assumption 1 with a Normality Distribution Plot
qplot(sample=S.M, data = Facebook, color=Level)

#verify Assumption 2 with a Bartlett Test
bartlett.test(S.M ~ Level, data = Facebook)

#Run aov for ANOVA
ANOVAFacebook<-(aov(S.M ~ Level, data = Facebook))
summary(ANOVAFacebook)

#Determine which means are different
TukeyHSD(ANOVAFacebook)




