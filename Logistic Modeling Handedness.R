library(foreign)
library(gmodels) #for cross tabulation
dat <- read.spss("handedness.sav", to.data.frame=TRUE)

head(dat)
table(is.na(dat))
dat <- na.omit(dat)

factorize <- function(data, features){
    for(feature in features){
        if(class(data[, feature]) == 'character')
            data[, feature] = factor(data[, feature])
    }
    return(data)
}

dat <- factorize(dat, names(dat))

#-----------------------------PART 1

#1 
#What are the proportions of 'mostly right handed' and 'mostly left handed' in the sample?

table(dat$Right_handed)
table(dat$Left_handed)

sum(dat$Right_handed == "mostly right-handed")/nrow(dat)
sum(dat$Left_handed == "mostly left-handed")/nrow(dat)

#2 
#Which everyday situations seem to define right or left handedness most clearly? 

pairs(dat)
names(dat)[1:6]

#3 
#What is the age and sex distribution in the sample?
#Is the sample representative of a general population? 
#Cross tabulate the variable Right handed with Age and Sex.

plot(dat$Age)
table(dat$Age)
plot(dat$Sex,)
table(dat$Sex)
sum(dat$Sex == "male", na.rm = T)/sum(table(dat$Sex), na.rm = T)#Should be 51%, too many males
CrossTable(dat$Right, dat$Age)
CrossTable(dat$Right, dat$Sex)
xtabs(~dat$Right+dat$Sex+dat$Age, data=dat)

#4 
#Draw a histogram of Handedness. 
#Calculate the mean and the median.
#Which one is a more appropriate measure of average score?
#What is the interpretation in each case?

hist(dat$handedness)#Left skewed
mean(dat$handedness)
median(dat$handedness)#Use median because of skewness

#5
#Draw a histogram of Handedness in the groups Mother's handedness and Father's handedness separately.

plot(dat$Mothershand, main = "Mother's handedness")
table(dat$Mothershand)

plot(dat$Fathershand, main = "Father's handedness")
table(dat$Fathershand)

#6
#Are Handedness and Height correlated in the sample?

cor(my_dat$handedness, my_dat$Height)#no correlation

#-----------------------------PART 2

#1
#Estimate a linear regression model with Handedness score as outcome variable and Mother's 
#and Father's handedness as predictor variables.
#Does the result change if you add the variable Sex to the model?
#What is the interpretation of the intercept and regression coefficients?

linear_model1 <- lm(dat$handedness ~ dat$Mothershand + dat$Fathershand)
summary(linear_model1)

linear_model2 <- lm(dat$handedness ~ dat$Mothershand + dat$Fathershand + dat$Sex)
summary(linear_model2)

#2
#Estimate a linear regression model with Handedness score as outcome variable
#and Theory as a predictor. 
#What are the interpretations of the intercept and regression coefficient for Theory?

linear_model3 <- lm(dat$handedness ~ dat$Theory)
summary(linear_model3)

#3
#Add now Skills into your model. What are the interpretations of the intercept and regression 
#coefficients for Theory and Skills now?
#Are they related to handedness?

linear_model4 <- lm(dat$handedness ~ dat$Theory + dat$Skills)
summary(linear_model4)
#Confidence intervals of difference parameters not containing 0 imply that
#there is a statistically significant difference between the populations.

#4
#Use now the variable Right-handed as an outcome variable in a 
#logistic regression model. Use the predictors in 2-4 in your models. 
#How do you interpret the regression coefficients now?

logistic_model1 <- glm(Right_handed ~ Mothershand + Fathershand,
                      data = dat, family = "binomial")
summary(logistic_model1)
round(summary(logistic_model1)$coef, 5)
round(exp(coef(logistic_model1)[2]),3)
    
logistic_model2 <- glm(dat$Right_handed ~ dat$Mothershand + dat$Fathershand + dat$Sex,
                      family = "binomial")
summary(logistic_model2)
exp(coef(logistic_model2))

logistic_model3 <- glm(dat$Right_handed ~ dat$Theory,
                       family = "binomial")
summary(logistic_model3)
exp(cbind(OR = coef(logistic_model3), confint(logistic_model3)))

logistic_model4 <- glm(dat$Right_handed ~ dat$Theory + dat$Skills,
                    family = "binomial")
summary(logistic_model4)
exp(cbind(OR = coef(logistic_model4), confint(logistic_model4)))

#5 What are the odds of being left handed if mother and father are left handed?

nrow(subset(dat, Mothershand == "left" & Fathershand == "left"))
#only 2 people in this sample have a left-handed mother and father. 

newdata <- subset(dat, Mothershand == "left" & Fathershand == "left")
#1 of which is mostly left-handed

#Odds:
2/nrow(dat)
exp(2/nrow(dat))

#6
#The data seems good for this purpose