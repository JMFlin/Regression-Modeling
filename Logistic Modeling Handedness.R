library(foreign)
library(gmodels) #for cross tabulation
library(psych)
library(ggplot2)
library(rms)

dat <- read.spss("handedness.sav", to.data.frame=TRUE)

head(dat)
table(is.na(dat))
#dat <- na.omit(dat)

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

describe(dat)

table(dat$Sex)
table(dat$Age)

sum(dat$Age == "<30 years")/nrow(dat)
sum(dat$Age == "31-40 years")/nrow(dat)
sum(dat$Age == ">=41 years")/nrow(dat)
#The sample is almost representative in terms of sex but not in age

#2 
#Which everyday situations seem to define right or left handedness most clearly? 

pairs(dat, gap=0, pch=19, cex=0.4, col="darkblue", panel = "panel.smooth")
names(dat)[1:6]

#3 
#What is the age and sex distribution in the sample?
#Is the sample representative of a general population? 
#Cross tabulate the variable Right handed with Age and Sex.

qplot(dat$Age)
table(dat$Age)
qplot(dat$Sex,)
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

n        <- nrow(dat)
mean     <- mean(dat$handedness)
sd       <- sd(dat$handedness)

breaks <- pretty(dat$handedness, n = nclass.FD(dat$handedness), min.n = 1)
bwidth <- breaks[2]-breaks[1]
ggplot(dat,aes(handedness))+geom_histogram(binwidth=bwidth,fill="white",colour="black")+
    labs(title = paste("Freedman-Diaconis' Method"))+
    theme(plot.title = element_text(size = rel(1.5)))+
    stat_function(fun = function(x, mean, sd, n, bw = bwidth){ 
            dnorm(x = x, mean = mean, sd = sd) * n * bw
            }, args = c(mean = mean, sd = sd, n = n, bw = bwidth))#Left skewed


mean(dat$handedness)
median(dat$handedness)
#Median is 1 -> more than half of sample are right-handed. 
#Normally median is better for skewed distribution but in this case mean (0.77) may be more informative.

#5
#Draw a histogram of Handedness in the groups Mother's handedness and Father's handedness separately.

qplot(dat$Mothershand, main = "Mother's handedness")
table(dat$Mothershand)

qplot(dat$Fathershand, main = "Father's handedness")
table(dat$Fathershand)

#6
#Are Handedness and Height correlated in the sample?
ggplot(dat, aes(Height, handedness)) + geom_point()
my_dat <- na.omit(dat)
cor.test(my_dat$handedness, my_dat$Height, method = c("pearson"))#no correlation
cor.test(my_dat$handedness, my_dat$Height, method = c("kendall"))#no correlation
cor.test(my_dat$handedness, my_dat$Height, method = c("spearman"))#non-parametric
#Correlation between height and handedness is negative and very weak.

#-----------------------------PART 2

#1
#Estimate a linear regression model with Handedness score as outcome variable and Mother's 
#and Father's handedness as predictor variables.
#Does the result change if you add the variable Sex to the model?
#What is the interpretation of the intercept and regression coefficients?

linear_model1 <- lm(dat$handedness ~ dat$Mothershand + dat$Fathershand)
summary(linear_model1)
#In this model there is no reasonable interpretation of Intercept

linear_model2 <- lm(dat$handedness ~ dat$Mothershand + dat$Fathershand + dat$Sex)
summary(linear_model2)

#2
#Estimate a linear regression model with Handedness score as outcome variable
#and Theory as a predictor. 
#What are the interpretations of the intercept and regression coefficient for Theory?

linear_model3 <- lm(dat$handedness ~ dat$Theory)
summary(linear_model3)

#Intercept 0.75 means that if favorite subject is not math, mother tongue or sciences,
#prediction of handedness score is 0.75. If favorite subject is math, mother tongue
#or sciences prediction is 0.75+0.035. Theory is not a significant predictor (p=0.623).

#3
#Add now Skills into your model. What are the interpretations of the intercept and regression 
#coefficients for Theory and Skills now?
#Are they related to handedness?

linear_model4 <- lm(dat$handedness ~ dat$Theory + dat$Skills)
summary(linear_model4)
#there is a statistically significant difference between the populations.

#Intercept 0.75 means that if favorite subject is not math, mother tongue or sciences,
#prediction of handedness score is 0.75. If favorite subject is math, mother tongue
#or sciences prediction is 0.715+0.070. If favorite subject is art, music or sports
#prediction is 0.715+0.067. Theory is not a significant predictor (p=.434). Skills is not a significant predictor (p=0.515).

#4
#Use now the variable Right-handed as an outcome variable in a 
#logistic regression model. Use the predictors in 2-4 in your models. 
#How do you interpret the regression coefficients now?

logistic_model1 <- glm(Right_handed ~ Mothershand + Fathershand,
                      data = dat, family = "binomial")
summary(logistic_model1)#The logistic regression coefficients give the change in the log odds of the outcome for a one unit change in the predictor variable. 
#For a change from Motherhandright to Motherhandleft, the log odds of being Right-handed(versus other) decreases by -1.38. 
#For a change from Motherhandright to Motherhandboth, the log odds of being Right-handed(versus other) decreases by 1.419. To interpret the coefficients as odds ratios, you have to exponentiate them.
round(exp(coef(logistic_model1)[2]),3)#Now we can say that for a change from Motherhandright to Mothershandleft, the odds of being right-handed (versus other) is 25% less.
lrm(logistic_model1)#the model explains 7% of the variance
    
logistic_model2 <- glm(dat$Right_handed ~ dat$Mothershand + dat$Fathershand + dat$Sex,
                      family = "binomial")
summary(logistic_model2)
exp(coef(logistic_model2))
#For a change from Sexfemale to Sexmale, the log odds of being Right-handed(versus other) decreases by -0.4.


logistic_model3 <- glm(dat$Right_handed ~ dat$Theory,
                       family = "binomial")
summary(logistic_model3)
exp(cbind(OR = coef(logistic_model3), confint(logistic_model3)))
#For a change from Theoryno to Theoryyes, the log odds of being Right-handed(versus other) increases by 0.1178.

logistic_model4 <- glm(dat$Right_handed ~ dat$Theory + dat$Skills,
                    family = "binomial")
summary(logistic_model4)
#For a change from Skillno to Skillyes, while holding Theory at a constant level, the log odds of being Right-handed(versus other) increases by 0.0417.
exp(cbind(OR = coef(logistic_model4), confint(logistic_model4)))
#Theory increases probabality of being mostly right-handed (OR=1.253).
#Skills increases probability of being mostly right-handed (OR=1.064).
#But both are not significant predictors at 0.05 signifance level.

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