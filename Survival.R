library(ggplot2)
library(survival)
library(ggpubr)
library(survminer)
library(lubridate)
library(ggfortify)

long=read.csv(file.choose())

#Base Kaplan-Meier survival curve
fit=survfit(Surv(Time) ~ Treatment, data = long)

autoplot(fit,xlab = "Days", ylab = "Overall survival probability", 
         main="Survival at 28 Days")
dan=autoplot(fit,xlab = "Days", ylab = "Overall survival probability", 
             main="Survival at 28 Days")
dan +theme_bw()


survdiff(Surv(Time) ~ Treatment, data = long)