#Load packages
library(dplyr)
library(tidyverse)
library(car)
library(stats)
# Import data
dat <- rio::import("Table7_5.sav")
# Change class of variables
dat$A <- as.factor(dat$A)
dat$B <- as.factor(dat$B)
dat$gp <- as.factor(dat$gp)
#check normality
shapiro.test(x=dat[complete.cases(dat$y), ]$y)
#Check for homoscedasticity
leveneTest(y~A*B, data = dat)
# Two- way ANOVA
Anova(lm(y~A*B, data = dat, contrasts=list(B=contr.sum,A=contr.sum)), type="III")
## ? How to get it to also show "corrected total" like in SPSS and do pairwise comparisons?
## ? How to do bonferroni
# One-way ANOVA
Anova(lm(y~gp, data = dat, contrasts = list(gp=contr.sum)), type="III")
# Recode effect representation
dat <- dat %>%
     mutate(
             a1 = ifelse(A == 1, 1, -1),  # Main effect of A
             b1 = ifelse(B == 1, 1, ifelse(B == 2, 0, -1)),  # Main effect of B
             b2 = ifelse(B == 1, 0, ifelse(B == 2, 1, -1)),  # Main effect of B
             ab11 = a1 * b1,  # Interaction effect
             ab12 = a1 * b2   # Interaction effect
         )
# UNIANOVA w parameter estimates
full <- lm(y~a1+b1+b2+ab11+ab12, data = dat)
summary(full)
Anova(full, type = 3)
# Reduced model 1
r1 <- lm(y~b1+b2+ab11+ab12, data = dat)
summary(r1)
Anova(r1, type=3)
# Reduced model 2
r2 <- lm(y~a1+ab11+ab12, data = dat)
summary(r2)
Anova(r2, type=3)
# Reduced model 3
r3 <- lm(y~a1+b1+b2, data = dat)
summary(r3)
Anova(r3, type=3)
# Design matrix for reduced model
##    1 0 -1 1 0
##    1 0  0 0 0
##    0 1  0 0 0
##    0 0  1 0 0
##    0 0  0 1 0
##    0 0  0 0 1.
dat <- dat %>%
        mutate(
              cm1 = ifelse(gp %in% c(1,2), 1, 0),
              cm2 = ifelse(gp == 3,  1, 0),
              cm3 = ifelse(gp == 1,  -1, ifelse(gp==4,1,0)),
              cm4 = ifelse(gp %in% c(1,5), 1, 0),
              cm5 = ifelse(gp == 6,1,0)
                   )
# Reduced model with contrasts
r4<-aov(y~cm1+cm2+cm3+cm4+cm5, data=dat)
summary(r4) # Residual finally matched SPSS! But SS does not?

         