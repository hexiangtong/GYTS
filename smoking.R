library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
#library(car) package ‘car’ was built under R version 3.4.1 
library(MASS)
library(gmodels)
library(aod) # for wald.test
library(Hmisc)
library(sjPlot)

setwd("~/Documents/MY WORK/Smoking Behavior/Smoking-Intention")

dat <- read.csv("Smoking Intention-All-Scale.csv", header = TRUE)
int.1 <- factor(dat$Future.smoker.1, levels = c('1', '2', '3', '4'), 
                labels = c('Definitely Not', 'Probably Not', 'Probably Yes', 'Definitely Yes')) 
dat <- cbind(dat, int.1)
head(dat)
attach(dat)

Par.Smo.Label <- factor(Par.Smo, levels = c('1', '2', '3', '4', '5'), 
                        labels = c('None', 'Both', 'Father only', 'Mother only', "I don't know")) 

lapply(dat[, c("int.1", "Age", "Sex", "PDI", "Exp")], table)
ftable(xtabs(~ PDI + int.1 + Par.Smo.Label, data = dat)) #three-way table

Sex <- factor(Sex)
PDI <- factor(PDI)
Par.Smo.B <- factor(Par.Smo.B)
Fam.Dis <- factor(Fam.Dis)
Fri.Smo <- factor(Fri.Smo)
#Exp <- factor(Exp)
School <- factor(School)
#Pro <- factor(Pro)
#Anti <- factor(Anti)

ggplot(dat, aes(x = int.1, y = Pro)) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  #facet_grid(Age ~ Sex, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

ordinal.reg <- polr(int.1 ~ Age+Sex+PDI+Par.Smo.B+Fam.Dis+Fri.Smo+Exp+School+Pro+Anti
                    +PDI*Par.Smo.B+PDI*Fam.Dis+PDI*Fri.Smo+PDI*Exp+PDI*School+PDI*Pro+PDI*Anti
                    +Par.Smo.B*Fam.Dis+Par.Smo.B*Fri.Smo+Par.Smo.B*Exp+Par.Smo.B*School+Par.Smo.B*Pro+Par.Smo.B*Anti
                    +Fam.Dis*Fri.Smo+Fam.Dis*Exp+Fam.Dis*School+Fam.Dis*Pro+Fam.Dis*Anti
                    +Fri.Smo*Exp+Fri.Smo*School+Fri.Smo*Pro+Fri.Smo*Anti
                    +Exp*School+Exp*Pro+Exp*Anti+School*Pro+School*Anti+Pro*Anti,
                    data = dat, Hess=TRUE)
summary(ordinal.reg)
ctable <- coef(summary(ordinal.reg))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
# obtain CIs at 95% using profiled log-likelihood
ci <- confint(ordinal.reg, level = 0.90)
# obtain CIs at 95% CIs using standard errors
#confint.default(ordinal.reg)
exp(coef(ordinal.reg))
exp(cbind(OR = coef(ordinal.reg), ci))

reg <- glm(Future.smoker.1 ~ Age+Sex+PDI+Par.Smo.B+Fri.Smo+Exp+School+Pro+Anti
           +PDI*Exp+PDI*Pro+PDI*Anti+PDI*Par.Smo,
           data = dat, family = gaussian())
summary(reg)
ctable <- coef(summary(reg))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
# obtain CIs at 95% using profiled log-likelihood
ci <- confint(reg, level = 0.90)
# obtain CIs at 95% CIs using standard errors
#confint.default(ordinal.reg)
exp(coef(ordinal.reg))
exp(cbind(OR = coef(ordinal.reg), ci))

reg.1 <- glm(Future.smoker.1 ~ Age+Sex+PDI+Par.Smo.B+Fam.Dis+Fri.Smo+Exp+School+Pro+Anti
             +PDI*Par.Smo.B+PDI*Fam.Dis+PDI*Fri.Smo+PDI*Exp+PDI*School+PDI*Pro+PDI*Anti
             +Fam.Dis*Par.Smo.B+Fam.Dis*Fri.Smo+Fam.Dis*Exp+Fam.Dis*School+Fam.Dis*Pro+Fam.Dis*Anti
             +Exp*Pro+Exp*Anti+Exp*School+Exp*Par.Smo.B
             +Pro*Anti+Pro*School+Pro*Par.Smo.B
             +Anti*School+Anti*Par.Smo.B
             +School*Par.Smo.B,
             data = dat, family = gaussian())
summary(reg.1)
ctable <- coef(summary(reg.1))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ci <- confint(reg.1, level = 0.90)

reg.2 <- glm(Future.smoker.1 ~ Age+Sex+PDI+Par.Smo.B+Fam.Dis+Fri.Smo+Exp+School+Pro+Anti
             +PDI*Fam.Dis+PDI*Exp+PDI*Pro+PDI*Anti+PDI*School
             +Exp*Anti+Exp*Pro+Exp*School,
             data = dat, family = gaussian())
summary(reg.2)

reg.3 <- lm(Future.smoker.1 ~ Age+Sex+PDI+Par.Smo.B+Fam.Dis+Fri.Smo+Exp+School+Pro+Anti
            +PDI*Par.Smo.B+PDI*Fam.Dis+PDI*Fri.Smo+PDI*Exp+PDI*School+PDI*Pro+PDI*Anti,
            data = dat)
summary(reg.3)


sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

s <- with(dat, summary(as.numeric(int.1) ~ Age+Sex+PDI+Par.Smo.B+Fam.Dis+Fri.Smo
                       +Exp.Home+Exp.Nhome+School
                       +Pro.Logo+Pro.Free+Pro.Media+Pro.Events+Anti.Media+Anti.Events, fun=sf))

glm(I(as.numeric(int.1) >= 2) ~ Age, family="binomial", data = dat)
glm(I(as.numeric(int.1) >= 3) ~ Age, family="binomial", data = dat)
glm(I(as.numeric(int.1) >= 4) ~ Age, family="binomial", data = dat)

s[, 5] <- s[, 5] - s[, 4]
s[, 4] <- s[, 4] - s[, 3]
s[, 3] <- s[, 3] - s[, 3]
s

plot(s, which=1:4, pch=1:4, xlab='logit', main=' ', xlim=range(s[,3:4]))

detach(dat)
rm(list = ls())