#WELL-SWITCHING: example of logistic modeling Many of the wells used for drinking purposes in Bangladesh
#and South Asia are contaminated with natural arsenic. Arsenic is a cumulative poison, and exposure increases the
#risk of cancer and other diseases. 
#The research team (from US and Bangladesh) measured all the wells and labeled as "safe" if the arsenic level was below
#0.5 units in 100mg/l. People using unsafe wells were encouraged to switch to nearby safe wells. 
#A few years later the research team returned to check who
#had switched wells to understand the predictive factors for switching. 
#References: Geen, Steen, Vergeng et al (2003) and Gelman, Trevisani,Lu, van Geen, (2004).

#THE DATA

#Outcome: y=1, if household switch to a safe well, 0 if not 
#Predictors: dist distance (in meters) to the closest safe well arsenic arsenic level of the respondent's well
#educ education level of the head of the household. 

wells <- read.table("wells.txt",header=T)

attach(wells)

# preliminary check of data
summary(wells)

str(wells)

# distributions of the predictors; need for transformations?
hist(dist); hist(arsenic)
# Both predictors are highly skewed to the left

# Load a program to analyse a logistic regression model
library(arm) # glm module in R (generalized linear models

## Model 1. Fit a logistic model for switching given the distance of safe well
fit.1 <- glm (switch ~ dist,family=binomial(link="logit"))

# rescale distance in 100 meters
dist100 <- dist/100

# model prediction: backtransform model results to probabilities
invlogit <- function(x){1/(1+exp(-x))}

## Model 2. rescale distance in 100-meter scale
fit.2 <- glm(switch~dist100,family=binomial(link="logit"))
display (fit.2)
# jittered plots (function)
jitter.binary = function(a, jitt=.05){
    ifelse (a==0, runif (length(a),0,jitt),runif (length(a),1-jitt,1))
}

# jittered plots and logistic model curve
switch.jitter <- jitter.binary(switch)
plot(dist,switch.jitter)

# curve of probability of switching given distance
curve(invlogit(coef(fit.1)[1] + coef(fit.1)[2]*x), add=TRUE)#Decreasing effect of distance in the logistic model


## Model 3. add arsenic level
fit.3 <- glm(switch~dist100+arsenic,family=binomial(link="logit"))
display (fit.3)

# center the predictors
c.dist100 <- dist100 - mean(dist100)
c.arsenic <- arsenic - mean(arsenic)

# curves of probability of switching given dist100 and arsenic with data plots
par(mfrow = c(2,1))
plot(dist,switch.jitter, xlim=c(0,max(dist)))
curve(invlogit(cbind(1,x/100,0.5)%*% coef(fit.3)), add=TRUE)
curve(invlogit(cbind(1,x/100,1.0)%*% coef(fit.3)), add=TRUE)

plot(arsenic,switch.jitter, xlim=c(0,max(arsenic)))
curve(invlogit(cbind(1,0,x)%*% coef(fit.3)), add=TRUE)
curve(invlogit(cbind(1,0.5,x)%*% coef(fit.3)), add=TRUE)
par(mfrow = c(1,1))

# The longer the distance, the less switching. The higher arsenic level, the more switching (Note: no linear effect)


## Model 4. center the predictors
fit.4 <- glm (switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic,family=binomial(link="logit"))
display(fit.4)

# compare nested models with deviance
deviance <- -2*(round(fit.4$deviance,1)- round(fit.3$deviance,1))
deviance
# 6.2, corresponding critical value X^2=3.841, 5% level


# Testing nested models: compare Model 4 vs Model 3 by the difference in deviances vs. the difference in degrees
#of freedom (k) with the values of the X^2-distribution.
#Here df is k4-k3=4-3=1, difference in deviance = -2(3927.6-3930.7)= 6.2 and the corresponding X^2=3.841, p=0.05, so
#Model 4 is not significantly better than Model 3.
#X^2 distribution: probability level (alpha)
#Df 0.5 0.10 0.05 0.02 0.01 0.001
#1 0.455 2.706 3.841 5.412 6.635 10.827
#2 1.386 4.605 5.991 7.824 9.210 13.815
#3 2.366 6.251 7.815 9.837 11.345 16.268
#4 3.357 7.779 9.488 11.668 13.277 18.465
#5 4.351 9.236 11.070 13.388 15.086 20.517


# rescale education
educ4 <- educ/4


## Model 5. add interaction of distance between arsenic level
fit.7 <- glm (switch ~ c.dist100 + c.arsenic +
                    c.dist100:c.arsenic +educ4, family=binomial(link="logit"))
display(fit.7)

# Adding education level of the head of household improves the fit: difference of deviances 3927.6-3907.9= 26.7 with
#only 1 df loss.

# education centered
c.educ4 <- educ4 - mean(educ4)
# log arsenic level centered
c.log.arsenic <- log(arsenic) - mean(log(arsenic))
## Model 6. add interactions of education between distance and arsenic level
fit.9 <- glm (switch ~ c.dist100 + c.log.arsenic +
                    c.educ4 +c.dist100:c.log.arsenic + c.dist100:c.educ4 +
                    c.log.arsenic:c.educ4,family=binomial(link="logit"))
display (fit.9)
# No significant interaction effects between arsenic level and others but the higher education the head of household
# has, the more probable to switch even when the distance increases.


# Average predictive differences: use the model with main effects only (Model 7.)

fit.10 <- glm (switch ~ dist100 + arsenic + educ4, family =
                   binomial(link="logit"))
display (fit.10)


# compare households with distance next to (lo) and 100 meters from (hi) a safe well
b <- coef(fit.10)
hi <- 1
lo <- 0
delta <- invlogit (b[1] + b[2]*hi + b[3]*arsenic +b[4]*educ4) -
     invlogit (b[1] + b[2]*lo + b[3]*arsenic + b[4]*educ4)
mean(delta)

# compare households with arsenic level 0.5 (lo) to level 1 (hi)
hi <- 1.0
lo <- 0.5
delta <- invlogit (b[1] + b[2]*dist100 + b[3]*hi + b[4]*educ4) - invlogit (b[1] + b[2]*dist100 + b[3]*lo + b[4]*educ4)
mean(delta)

# compare households with education level 0 (lo) to level 3 (hi)
hi <- 3
lo <- 0
delta <- invlogit (b[1]+b[2]*dist100+b[3]*arsenic+b[4]*hi) - invlogit (b[1]+b[2]*dist100+b[3]*arsenic+b[4]*lo)
mean(delta)

# Model 8. add interaction between distance and arsenic level
fit.11 <- glm (switch ~ dist100 + arsenic + educ4 +
                     dist100:arsenic, family=binomial(link="logit"))
display (fit.11)


# compare households with distance next to (lo) and 100 meters from (hi) a safe well, now with interaction
b <- coef (fit.11)
hi <- 1
lo <- 0
delta <- invlogit (b[1] + b[2]*hi + b[3]*arsenic + b[4]*educ4 + b[5]*hi*arsenic) -
     invlogit (b[1] + b[2]*lo + b[3]*arsenic + b[4]*educ4 + b[5]*lo*arsenic)
mean(delta)

# Summary: Long distance (100m vs 0m) has a much larger effect on switching than high arsenic level (1 vs 0.5) but
# if the head of household was educated and aware of the poisonous effect of arsenic, the probability of switching
# increased even when the nearest safe well was further away.


# binned residuals: average residuals in bins

## Defining binned residuals (function)

# predicted values for residual analysis
pred.1 <- fit.1$fitted.values

binned.resids <- function (x, y, nclass=sqrt(length(x))){
    breaks.index <- floor(length(x)*(1:(nclass-1))/nclass)
    breaks <- c (-Inf, sort(x)[breaks.index], Inf)
    output <- NULL
    xbreaks <- NULL
    x.binned <- as.numeric (cut (x, breaks))
    for (i in 1:nclass){
        items <- (1:length(x))[x.binned==i]
        x.range <- range(x[items])
        xbar <- mean(x[items])
        ybar <- mean(y[items])
        n <- length(items)
        sdev <- sd(y[items])
        output <- rbind (output, c(xbar, ybar, n, x.range, 2*sdev/sqrt(n)))
    }
    colnames (output) <- c ("xbar", "ybar", "n", "x.lo", "x.hi", "2se")
    return (list (binned=output, xbreaks=xbreaks))
}

## Binned residuals vs. estimated probability of switching 

br.1 <- binned.resids(pred.1, switch-pred.1, nclass=40)$binned
plot(range(br.1[,1]), range(br.1[,2],br.1[,6],-br.1[,6]), xlab="Estimated  Pr (switching)", ylab="Average residual", type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline(0,0, col="gray", lwd=.5)
lines(br.1[,1], br.1[,6], col="gray", lwd=.5)
lines(br.1[,1], -br.1[,6], col="gray", lwd=.5)
points(br.1[,1], br.1[,2], pch=19, cex=.5)

## Plot of binned residuals vs. covariates of interest

# distance 

br.dist <- binned.resids(dist, switch-pred.1, nclass=40)$binned
plot(range(br.dist[,1]), range(br.dist[,2],br.dist[,6],-br.dist[,6]), xlab="Distance to nearest safe well", ylab="Average residual", type="n", main="Binned residual plot", mgp=c(2,.5,0))
abline (0,0, col="gray", lwd=.5)
lines(br.dist[,1], br.dist[,6], col="gray", lwd=.5)
lines(br.dist[,1], -br.dist[,6], col="gray", lwd=.5)
points(br.dist[,1], br.dist[,2], pch=19, cex=.5)

error.rate <- mean((pred.1>0.5 & switch==0) | (pred.1>0.5 & switch==1))
error.rate
