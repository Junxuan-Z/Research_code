#Paper 1 data analysis

# Load necessary libraries
library(dplyr)  
library(readr) 
library(readxl) 
library(tidyr)
library(ggpubr)
library(tidySEM)
library(lavaan)
library(ggplot2)
library(dplyr)
library(readxl)
library(psych)
library(knitr)
library (MVN)  
library(corrplot)
library(semPlot)
library(semTools)
library(indprod)
library(interactions)


df <- read_xlsx("path/to/dataset")
df1 <- subset(df,select = c("EC","ED","PR_ED","PR_EC","overall EC","overall ED","AR_ER","PR_ER","Self_ER"))

#-------------------Data Imputation-------------------------------------------
## Load Packages
library(VIM)
library(mice)
library(lattice)

OPES <- df1[, c("overall EC","overall ED")]
OPES <- data.frame(OPES)
summary(OPES)
## Look for missing > 18% variables
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(OPES,2,pMiss)
apply(OPES,1,pMiss)
## Missing data pattern
md.pattern(OPES)
## Plot of missing data pattern
aggr_plot <- aggr(OPES, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

## Impute missing data using mice
## about 6% average missing data, so maxit= 6
tempData <- mice(OPES,m=5,maxit=6,meth='pmm',seed=500)
summary(tempData)
tempData$meth
completedData <- complete(tempData,1)

## Create "overall EC" column in df1
df1$"overall EC" <- completedData$overall.EC
df1$"overall ED" <- completedData$overall.ED

df1$"overall EC" <- completedData$overall.EC
df1$"overall ED" <- completedData$overall.ED

#------------------Aim 1 (relationship)-----------------------------------------
dff <- df1 
colnames(dff)
names(dff)[names(dff) == "EC"] <- "AR.EC"
names(dff)[names(dff) == "ED"] <- "AR.ED"
names(dff)[names(dff) == "PR_ED"] <- "PR.ED"
names(dff)[names(dff) == "PR_EC"] <- "PR.EC"
names(dff)[names(dff) == "overall EC"] <- "OR.EC"
names(dff)[names(dff) == "overall ED"] <- "OR.ED"
names(dff)[names(dff) == "PR_ER"] <- "PR.ER"
names(dff)[names(dff) == "AR_ER"] <- "AR.ER"
names(dff)[names(dff) == "Self_ER"] <- "Parent.ER"

##---------------------Covariance-----------------------------------------------
covmatrix <- round(cov(dff),digits=3)
kable(covmatrix,booktabs=TRUE,format="markdown")
kable(describe(dff,type=2),booktabs=TRUE,format="markdown")


mat <- cor.mtest(dff,conf.level= 0.95)
m <- cor(dff)
print(mat$p)

corrplot(m,type="upper",tl.pos="tp", p.mat = mat$p, insig = "blank",col=COL2('RdBu', 10),tl.col = 'black',)
corrplot(m,,add=T,type="lower", method="number", col="black", diag=FALSE,tl.pos="n", cl.pos="n", p.mat = mat$p, insig = "blank")

##-----------------------t-test-------------------------------------------------

attach(df1)
t_test_P <- t.test(AR_ER, PR_ER, paired = TRUE)
print(t_test_P)
detach(df1)

#-------------------Aim 2 (Polynomial regression model_Discrepancy)-------------
df2 <- data.frame(sapply(df1, function(x) scale(x, scale=FALSE)))

attach(df2)

##-------------Models for adolescent-reported emotion regulation-----------------
model1 <- lm(AR_ER ~ poly(EC, PR_EC,degree=2, raw=TRUE))
summary (model1)

model2 <- lm(AR_ER ~ poly(ED, PR_ED,degree=2, raw=TRUE))
summary (model2)

model3 <- lm(AR_ER ~ poly(EC, overall.EC,degree=2,raw=TRUE))
summary (model3)

model4 <- lm(AR_ER ~ poly(ED, overall.ED,degree=2,raw=TRUE))
summary (model4)


##-------------Models for parent-reported emotion regulation--------------------
modela <- lm(PR_ER ~ poly(PR_EC,EC, degree=2, raw=TRUE))
summary (modela)

modelb <- lm(PR_ER ~ poly(PR_ED,ED,degree=2, raw=TRUE))
summary (modelb)

modelc <- lm(PR_ER ~ poly(PR_EC, overall.EC,degree=2,raw=TRUE))
summary (modelc)

modeld <- lm(PR_ER ~ poly(PR_ED, overall.ED,degree=2,raw=TRUE))
summary (modeld)

#--------------Aim 3 (Difficulties in emotion regulation)-----------------------

summary(lm(EC~AR_ER + PR_EC + overall.EC + AR_ER*PR_EC + overall.EC*AR_ER))


summary(lm(ED~AR_ER + PR_ED + overall.ED + PR_ED*AR_ER + overall.ED*AR_ER))


summary(lm(PR_EC ~ Self_ER + EC + overall.EC + EC*Self_ER + overall.EC*Self_ER))


summary(lm(PR_ED ~ Self_ER + ED + overall.ED + ED*Self_ER + overall.ED*Self_ER))

##--------------Interaction Plot------------------------------------------------
SIG <- lm(PR_EC~Self_ER+EC+overall.EC+EC*Self_ER+overall.EC*Self_ER)
summary(SIG)

library(jtools)
interact_plot(SIG, pred = EC, modx = Self_ER, cond.int = TRUE, 
              x.label = "Adolescent-reported emotion coaching",
              legend.main = "Mother difficulties in emotion regulation",
              y.label = "Parent-reported emotion coaching")

detach(df2)

#------------------------Supplementary analysis---------------------------------
##-----Association between demographic variables and study variables------------

df5 <- subset(df,select = c("EC","ED","PR_ED","PR_EC","AR_ER","PR_ER","Self_ER","family_income","family_makeup","edu_parent","race_parent"))
df5$"overall EC" <- df1$`overall EC`
df5$"overall ED" <- df1$`overall ED`
covmatrix <- round(cov(df5),digits=3)
kable(covmatrix,booktabs=TRUE,format="markdown")
kable(describe(df5,type=2),booktabs=TRUE,format="markdown")


mat <- cor.mtest(df5,conf.level= 0.95)
m <- cor(df5)
print(mat$p)

corrplot(m,type="upper",tl.pos="tp", p.mat = mat$p, insig = "blank",col=COL2('RdBu', 10),tl.col = 'black',)
corrplot(m,,add=T,type="lower", method="number", col="black", diag=FALSE,tl.pos="n", cl.pos="n", p.mat = mat$p, insig = "blank")


##-------------------------Adolescent reported ER-------------------------------
library(ppcor)
attach(df)
pcor.test(EC,AR_ER,family_makeup)

pcor.test(ED,AR_ER,family_makeup)

pcor.test(PR_EC,AR_ER,family_makeup)

pcor.test(PR_ED,AR_ER,family_makeup)

pcor.test(df1$`overall EC`,AR_ER,family_makeup)

pcor.test(df1$`overall ED`,AR_ER,family_makeup)


##-------------------------Parent reported ER-----------------------------------
pcor.test(EC,PR_ER,family_makeup)

pcor.test(ED,PR_ER,family_makeup)

pcor.test(PR_EC,PR_ER,family_makeup)

pcor.test(PR_ED,PR_ER,family_makeup)

pcor.test(df1$`overall EC`,PR_ER,family_makeup)

pcor.test(df1$`overall ED`,PR_ER,family_makeup)


##-------------Models for adolescent-reported emotion regulation (with covariance family_makeup)--------------

attach(df2)

model1 <- lm(AR_ER ~ poly(EC, PR_EC,degree=2, raw=TRUE)+ df$family_makeup)
summary (model1)

model2 <- lm(AR_ER ~ poly(ED, PR_ED,degree=2, raw=TRUE)+ df$family_makeup)
summary (model2)

model3 <- lm(AR_ER ~ poly(EC, overall.EC,degree=2,raw=TRUE)+ df$family_makeup)
summary (model3)

model4 <- lm(AR_ER ~ poly(ED, overall.ED,degree=2,raw=TRUE)+ df$family_makeup)
summary (model4)


##-----------------Models for parent-reported emotion regulation (with covariance family_makeup)-----------------
modela <- lm(PR_ER ~ poly(PR_EC,EC, degree=2, raw=TRUE)+ df$family_makeup)
summary (modela)

modelb <- lm(PR_ER ~ poly(PR_ED,ED,degree=2, raw=TRUE)+ df$family_makeup)
summary (modelb)

modelc <- lm(PR_ER ~ poly(PR_EC, overall.EC,degree=2,raw=TRUE)+ df$family_makeup)
summary (modelc)

modeld <- lm(PR_ER ~ poly(PR_ED, overall.ED,degree=2,raw=TRUE)+ df$family_makeup)
summary (modeld)

##-------------Models for adolescent-reported emotion regulation (with covariance race_parent)--------------
model1 <- lm(AR_ER ~ poly(EC, PR_EC,degree=2, raw=TRUE)+ df$race_parent)
summary (model1)

model2 <- lm(AR_ER ~ poly(ED, PR_ED,degree=2, raw=TRUE)+ df$race_parent) 
summary (model2)

model3 <- lm(AR_ER ~ poly(EC, overall.EC,degree=2,raw=TRUE)+ df$race_parent)
summary (model3)

model4 <- lm(AR_ER ~ poly(ED, overall.ED,degree=2,raw=TRUE)+ df$race_parent)
summary (model4)


##-------------Models for parent-reported emotion regulation (with covariance race_parent)--------------------
modela <- lm(PR_ER ~ poly(PR_EC,EC, degree=2, raw=TRUE)+ df$race_parent)
summary (modela)

modelb <- lm(PR_ER ~ poly(PR_ED,ED,degree=2, raw=TRUE)+ df$race_parent)
summary (modelb)

modelc <- lm(PR_ER ~ poly(PR_EC, overall.EC,degree=2,raw=TRUE)+ df$race_parent)
summary (modelc)

modeld <- lm(PR_ER ~ poly(PR_ED, overall.ED,degree=2,raw=TRUE)+ df$race_parent)
summary (modeld)

##-------------Aim 3 (with covariance family_makeup)-----------------------

summary(lm(EC~AR_ER + PR_EC + overall.EC + AR_ER*PR_EC + overall.EC*AR_ER + family_makeup))


summary(lm(ED~AR_ER + PR_ED + overall.ED + PR_ED*AR_ER + overall.ED*AR_ER + family_makeup))


summary(lm(PR_EC ~ Self_ER + EC + overall.EC + EC*Self_ER + overall.EC*Self_ER + family_makeup))


summary(lm(PR_ED ~ Self_ER + ED + overall.ED + ED*Self_ER + overall.ED*Self_ER + family_makeup))

##-------------Aim 3 (with covariance race_parent)-----------------------

summary(lm(EC~AR_ER + PR_EC + overall.EC + AR_ER*PR_EC + overall.EC*AR_ER + race_parent))


summary(lm(ED~AR_ER + PR_ED + overall.ED + PR_ED*AR_ER + overall.ED*AR_ER + race_parent))


summary(lm(PR_EC ~ Self_ER + EC + overall.EC + EC*Self_ER + overall.EC*Self_ER + race_parent))


summary(lm(PR_ED ~ Self_ER + ED + overall.ED + ED*Self_ER + overall.ED*Self_ER + race_parent))

