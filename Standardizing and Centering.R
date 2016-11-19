library(foreign)

#Centering

my_data <- read.spss("child.iq.sav", to.data.frame=TRUE)
my_data <- na.omit(my_data)

mom.hs <- my_data$momeduc == "hs grad"
mom.iq <- my_data$childscore * 0.7 #just a proxy cause we dont have the data

#Centering an indicator mom.hs (high school completed) and a continuous variable mom.iq

c.mom.hs <- mom.hs - mean(mom.hs)
c.mom.iq <- mom.iq - mean(mom.iq)

model <- lm(my_data$childscore ~ c.mom.hs+c.mom.iq+c.mom.hs:c.mom.iq)
summary(model)
#Main effect + interaction. say mom.iq is 0.59 and interaction is -0.48 then 0.59-0.48=0.11
#so 1 change in iq changes score by 0.11

#Standardizing

z.mom.hs <- (mom.hs - mean(mom.hs))/(2*sd(mom.hs))
z.mom.iq <- (mom.iq-mean(mom.iq))/(2*sd(mom.iq))

model <- lm(my_data$childscore ~ z.mom.hs+z.mom.iq+z.mom.hs:z.mom.iq)
summary(model)
#Here we compare the estimated coefficients on the same scale. So this is a better way.
#The highest estimated coef is the most important variable.