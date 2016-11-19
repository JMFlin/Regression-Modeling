library(arm)
library(foreign)
library(car)

iq.data <- read.spss("child.iq.sav", to.data.frame=TRUE)

#2. Checking the data and use of R objects briefly

head(iq.data) 

iq.data$momage

mean(iq.data$momage) # the mean of mothers age

#3. Types of variables

str(iq.data) # Types of variables

#An integer variable momedu is transformed into a factor variable fmomeduc and the classes are named:
iq.data$fmomeduc=factor(iq.data$momeduc, labels=c("no hs education","hs grad","some college","college grad"))

# checking
head(iq.data)

# 3. continues:
str(iq.data)

#Everything is right!
    
# 4. Statistics and plots (the description of the data)

#4a) Statistics of each variable

summary(iq.data)

#Compare momeduc and fmomeduc, the latter one is explained using the counts. 

#4b) Plots for one variable

hist(iq.data$childscore, main="Child's score")
hist(iq.data$momage, main="Mother's s age")


#We can see some asymmetry in the empirical distribution of child's score. 

#4b) continues:
    
barplot(table(iq.data$fmomeduc), main="Mother's education")


#4c) Statistics and plots for two variables

plot(childscore~momage, data=iq.data)

#We cannot see strong linear dependence between momage and childscore.

#Correlation coefficient
cor(iq.data$childscore, iq.data$momage)

#The value of correlation coefficient is low, which can be seen from the scatterplot too.

#Boxplots of a continuous variable in the classes of a factor:
boxplot(childscore~fmomeduc, data=iq.data, ylab="Childscore")

#Some increasing trend can be seen in childscore when the level of education is increasing.  
#Circles are extreme values with respect the rest of data in a certain class, called also outliers. 
#The outliers are outside the interval [Q1-1.5*IQR, Q2+1.5*IQR]. 

#4c) continues: 
    
#Statistics of a continuous variable in the classes of a factor:
tapply(iq.data$childscore,iq.data$fmomeduc, summary)

#According to boxplots and statistics it seems that the child's scores are higher, when the mothers are more educated.

#5. How would you consider if the mother age will predict the child's score? (Statistical analysis part)

#5a) Fit the regression model and print the estimated values of parameters and other results: 

m1 <- lm(childscore~momage, data=iq.data)

summary(m1) # coefficients, t-tests, F-test, R-squared


#5a) continues:
display(m1) # short summary


#The mean childs scores increases 0.84 points when mothers age increases one unit (interpretation of momage). The intercept is the y-value, when momage=0. 

#5b) Diagnostics with plots:

plot(m1, which=1) # res vs. fit


#In the plot of the residuals vs. fitted values the points are expected to be located nicely around the zero line with similar vertical spread for all x-values (fitted values). 
#There should not be any curvature structure (the red line is straight, the red line is a regression curve instead of regression line), also no heteroscedasticity, meaning that the residuals spread more with certain x-values. 
#In that sense, this figure looks nice. Only some observations seem to be outliers (numbered). 


#5b) continues:

plot(m1, which=2) # qqplot

#In Normal q-q plot the theoretical quantiles are compared to the quantiles of standardized residuals. 
#The points forming a straight line in the east-north direction is expected. 
#In this case the  points are quite nicely around a straight line, however, some deviations from normality can be seen.  
#Let us further consider the histogram of residuals, where we can see some deviations from symmetry.
hist(m1$res)


# Some asymmetry can be seen. However, the sample size is large, thus we do not need to worry so much (Central limit theorem) when making statistical inference.  
# 5c) Scatterplot with the fitted regression line

plot(childscore~momage, data=iq.data)
abline(m1)

# 5d) Our final conclusion if the mother age predicts the childs score:
summary(m1) # coefficients,t-tests, F-test, R-squared

#The variable momage is statistically significant variable to explain the variation of childscore (p=0.027). 
#The regression coefficient of momage is 0.84, which describes the average change in the childs score when the mothers age increases one year, the effect 
#of momage  is said to be positive, the older mother, the better childs score. 
#The constant  of the regression model is 67.78 which is y-value of the regression line at momage=0. 
#The constant of this model is not interpretable as such. If the variable was centered in that case the interpretation of constant would be more interesting. 
#Note that R-squared is really small 1.2 %, which means that variable momage explains 1.2 % of the variation of variable childscore.


#5d) continues: 
    
# Extra question 1: According to the fitted regression model, the child's score is higher on average when the mother is older. 
# Therefore, the recommendation could be that the mother should give the birth older. 

# Extra question 2:  When making a recommendation, it should be considered:
# i) if  the effect of the mother age is practically significant (not only statistically significant). 
# In this case the difference between youngest and oldest mothers is 10 scores on average.  
# ii) if the variation of child's score is explained well by the mother's age (only 1.2 %  in this case, which is not much).
# The regression model is valid at the mother's ages which are measured. 

#6. How would you consider if the mother education will predict the childs score? (Statistical analysis part)

#6a)  Fit the model and print the estimated values of parameters and other results:

m2 <- lm(childscore~fmomeduc, data=iq.data)

#Print the results:
summary(m2)	


#All classes are statistically significant with respect to the baseline "no hs" education.

#b) Diagnostics

plot(m2, which=1) # res vs. fit
plot(m2, which=2) # qqplot 
hist(m2$res)

#6b) continues:


#When considering a factor variable fmomeduc, there are only three different predicted values, which can be seen in the plot of res vs fitted with  three groups. 
#The residuals are quite normally distributed according to Normal q-q plot (above right) and histogram of residuals (below), some deviation from symmetry can be seen: Similar interpretation as earlier. 

#6c) Interpret the coefficients:

display(m2)

#The classes of variable fmomeduc,"hs grad" and "some college", the level of childs score on average 10 points higher when compared to "no hs" education. 
#In the last class "college grad" has the scores are on average 18.89 point higher as in "no hs" education class. R-squared is small also with this model.  

#6d) Comparison the model m2 to the model m3 where momeduc is a continuous variable:  

m3 <- lm(childscore~momeduc, data=iq.data)
summary(m3)


#This model m3 suggests that the average change between education classes is always the same  5.107 points. 
#From the model m2 we could, however, see that the amount of change differs between classes. 
#R-squared of m2 is a bit higher, which tells us that m2 is a bit better model. 
#To this end, we made and used the factor variable fmomeduc. 


#7.  How would you consider if the mother education and mother age together will predict the childs score? (Statistical analysis part)

# 7a)  Fit the model. Print the summary of the model

m4 <- lm(childscore~momage+fmomeduc, data=iq.data)
summary(m4)



#7b) Diagnostics
plot(m4, which=1) # res vs. fit

#Also here, we can see three groups of residuals due to the factor fmomeduc, but here the momage makes some dispersion. 
#Note that two the effect of classes "hs grad" and "some college" are quite similar when the coefficients are compared. 
#The same observations are marked as outliers as earlier. 
#The same ourliers can be seen from Normal q-q plots and histogram of residuals. Histogram is a bit asymmetric similarly as earlier. 

#7b) continues:

plot(m4, which=2) # qqplot 
hist(m4$res)


#7c)  Interpret the coefficients

#In the summary, after fitting  momage the three coefficients related to fmomedu classes tells the mean change to be added into the intercept when moved from the class "no hs" to the class "hs grad", the class "some college" and to the class "college grad". 
#According to the summary, the variable momage does not explain the variation of childscore after the factor fmomeduc has been fitted into the model. 

#Extra questions: R-squared of the model m4 is now 6 %, not very high. 
#According to these results, the education of mother at least "hs grad" increases the childscore on average. 
#The mother age does not have statistically significant effect after the education  has fitted into the  model m4. 
#Therefore, according to the model m4 it would perhaps be better not to give any recommendation based on mothers's age. 



#Forming a new binary variable of mom's education.


iq.data$momhs <- recode(iq.data$momeduc, "c('no hs education')=0;else=1")
iq.data$fmomhs <- factor(iq.data$momhs, labels=c("no hs", "hs"))

#Checking:

head(iq.data)

str(iq.data)



#Boxplots of a child's score in two classes of fmomhs:
    
boxplot(childscore~fmomhs, data=iq.data, ylab="Childscore")

#1b) Fit the regression model using momage and fmomhs with interaction

m5 <- lm(childscore~momage+fmomhs+momage:fmomhs, data=iq.data)

#and print the estimated values of parameters and other results:
    
summary(m5) # kertoimet, yksittäiset t-testit, F-testi


#1c)  Assumptions: Residuals are distributed in both sides of vertical line without any pattern.    

par(mfrow=c(2,2))
plot(m5)
par(mfrow=c(1,1))
#Some outliers exists.  

#1d)  Interpret the coefficients:   

display(m5)

#Interpretation: Interaction term  is statistically significant. 
#In that case lower degree terms (momage ja fmomhs)  should be kept in the model.  

#Interpretation of coefficients: 
    
#Mothers with no hs education:  
#Child's score mean =  105.22-1.24*momage

#Mothers with at least hs grad:  
#Child's score mean =  105.22-1.24*momage-38.41+2.21*momage
#=  (105.22-38.41) +(2.21-1.24)* momage
#=   66.81+ 0.97*momage

#Exercise 2.

#2a)  Plot child's score and mom's age as in exercise 1, but add educ_binary to separate two groups.

plot(childscore~momage, data=iq.data, col=fmomhs, xlab="momage", ylab="childscore")


#2b)  Regression coefficients from object m5: 
    
kert <- m5$coef
kert

#Mothers with no hs education:  
    
anohs <- kert[1]
bnohs <- kert[2]
anohs
bnohs 


#Mothers with at least hs grad:  
    
ahs <- sum(kert[c(1,3)])
bhs <- sum(kert[c(2,4)])
ahs

bhs



#2c) Plot regression lines for both groups: mothers with no hs and mothers with hs:
    
abline(anohs, bnohs, col=1) # no hs
abline(ahs, bhs, col=2) # hs 


