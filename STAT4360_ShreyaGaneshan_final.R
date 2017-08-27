# STAT 4360 Final Project 
# Shreya Ganeshan 
# 29 November 2016 

# ----------------- Importing Libraries ------------------
library(MASS)
library(car) # for Durbin Watson Test and Variance Inflation Factor (VIF)
library(leaps) # for regsubsets
# ----------------- Reading in Dataset ------------------
# clear all previous variables
rm(list=ls())
data = read.table("gasd96.txt", header = T)
data
attach(data) # to call variables by name
detach(data)

# ----------------- Editing Variables ------------------
# Variables: COUNTY, T8, T3, T5, ENRL, DENS, IMTR, IM, IL, PLUN

# changing variable responses to binary categorical variables 
NameIMTR = IMTR
NameIMTR[NameIMTR == 1] = "Metro Area"
NameIMTR[NameIMTR == 0] = "Non-Metro Area"
IMTR=NameIMTR
IMTR 

NameIM = IM
NameIM[NameIM == 1] = "Medium Enrollment"
NameIM[NameIM == 0] = "Non-Medium Enrollment"
IM=NameIM
IM 

NameIL = IL
NameIL[NameIL == 1] = "Large Enrollment"
NameIL[NameIL == 0] = "Non-Large Enrollment"
IL=NameIL
IL

# ----------------- Parsing Dataset ------------------
# dataset with only numeric variables 
# removing OBS (column 1), COUNTY (column 2), IMTR (column 8), IM (column 9), IL (column 10)
datanumeric = data[,(c(3,4,5,6,7,11))]
datanumeric

# dataset with only binary categorical variables 
datacat = data[,(c(8,9,10))]
datacat

# -------------------------------------------------------
# SECTION 3: DATA SUMMARY
# ----------------- Summary Statistics: Numerical ------------------
#
summary(data)
stem(T8)
sd(T8)
sd(T3)
sd(T5)
sd(ENRL)
sd(DENS)
sd(PLUN)

# creating a function to calculate summary stats all at once for all numerical variables
summarystats = function(x) { 
  c(Mean=mean(x), Median=median(x), STDV=sd(x), Min=min(x), Max=max(x), 
  Q1=quantile(x,(1/4),names=F), Q3=quantile(x,(3/4),names=F), IQR=IQR(x))
}
# creating a dataframe (vector) to store al the summary stats for all numerical variables
stats = data.frame(summarystats(T8), summarystats(T3), summarystats(T5), summarystats(ENRL),
                   summarystats(DENS), summarystats(PLUN))
stats
#transpose and round the stats dataframe/matrix
stats = t(stats)
stats = round(stats,3)
stats

# creating a new table (writing a csv file) to store summary stats of all numeric vars
write.csv(stats, file = "finalsummarynumeric.csv", quote=F)

# ----------------- Summary Statistics: Categorical ------------------
# does not make sense to tabulate summary statistics, so use frequency tables
# though, if categorical variables were still coded as 0 or 1, then finding the mean/median
  # could elicit how if there are more or less 0 or 1 values depending on how close 
  # mean/median values are to 0.5
# freqencies of categorical variables: IMTR, IM, IL
IMTRfreq = IMTR
IMTRfreq = table(IMTRfreq)
IMTRfreq # 0 = 124 and 1 = 50

IMfreq = IM
IMfreq = table(IMfreq)
IMfreq # 0 = 107 and 1 = 67

ILfreq = IL
ILfreq = table(ILfreq)
ILfreq # 0 = 155 and 1 = 19

# lisitng all binary categorical var in frequency table
freqtable = data.frame(IMTRfreq, IMfreq, ILfreq)
freqtable
# renaming columns
colnames(freqtable)[1] = "IMTR"
colnames(freqtable)[2] = "Frequency"
colnames(freqtable)[3] = "IM"
colnames(freqtable)[4] = "Frequency"
colnames(freqtable)[5] = "IL"
colnames(freqtable)[6] = "Frequency"

# creating a new table (writing a csv file) to store frequencies of all categorical vars
write.csv(freqtable, file = "finalfreqcat.csv", quote=F)

# ----------------- Box Plots of Each Variable ------------------
# Numerical Variables 
datanumeric
# standardizing variables (distance from respective means / respective SD)
datanumericstd = scale(datanumeric, center = T, scale = T)
datanumericstd
boxplot(datanumericstd, col = "grey", ylab = "Standardized Observations",
        xlab = "Numerical Variables", main = " Distribution of Standardized Numerical Variables")

boxplot(T8, col="grey", ylab = "Score Values", main = "Summary of 8th Grade Scores in all Districts")

# ----------------- Plots of T8 vs. Each Variable ------------------

plot(modelfinal)
par.plot = par(mfrow=c(2, 4))
plot(T8 ~ T3, xlab = 'T3', ylab = 'T8',main = 'T8 vs T3')
plot(T8 ~ T5, xlab = 'T5', ylab = 'T8', main = 'T8 vs T5')
plot(T8 ~ ENRL, xlab = 'ENRL', ylab = 'T8', main = 'T8 vs ENRL')
# plot(T8 ~ log(ENRL), xlab = 'log(ENRL)', ylab = 'T8', main = 'T8 vs log(ENRL)')
plot(T8 ~ DENS, xlab = 'DENS', ylab = 'T8', main = 'T8 vs DENS')
# plot(T8 ~ log(DENS), xlab = 'log(DENS)', ylab = 'T8', main = 'T8 vs log(DENS)')
plot(T8 ~ PLUN, xlab = 'PLUN', ylab = 'T8', main = 'T8 vs PLUN')
boxplot(T8 ~IMTR, xlab = 'IMTR', ylab = 'T8', main = 'T8 vs IMTR')
boxplot(T8 ~IM, xlab = 'IM', ylab = 'T8', main = 'T8 vs IM')
boxplot(T8 ~IL, xlab = 'IL', ylab = 'T8', main = 'T8 vs IL')

pairs(c(datanumeric,datacat)) # good for appendix

# par(mfrow=c(1,1))
barplot(IMTRfreq, col=c("grey", "brown"), ylim = c(0,125),
        main = "Number of School Districts in Metro Areas", names.arg =c('Non-Metro', 'Metro'))
# no = 124, yes = 50

barplot(IMfreq, col=c("grey", "brown"), ylim = c(0,115),
        main = "Number of Medium-Enrollment Districts in GA", names.arg =c('Medium', 'Non-Medium'))
# no = 107, yes = 67
# -------------------------------------------------------
# SECTION 4: ANALYSIS
# ----------------- Correlation of All Variables ------------------
# numerical 
corrnum = round(cor(datanumeric),2)
corrnum
#writing this into a CSV table 
write.csv(corrnum, file = "correlationmatrix.csv", quote=F)

# cateogrical 
corrcat = round(cor(datacat),2)
corrcat
#writing this into a CSV table 
write.csv(corrcat, file = "correlationmatrixcat.csv", quote=F)

# ----------------- Checking Assumptions ------------------
# normality fo T8
# no clear pattern in residuals 
# independence/random sampling
# can also plot residual diagnostics (but can't do this without the model)

# ----------------- Plotting Normality of Response Variable ------------------
par(mfrow=c(1,1))
# plot density function 
density = density(T8) # uniform density
plot(density, main = 'Density Distribution of T8')

# normal curve over distribution of T8
h = hist(T8, main = 'Distribution of T8', col = 'grey', breaks = 10, xlab = 'T8')
xfit<-seq(min(T8),max(T8),length=40) 
yfit<-dnorm(xfit,mean=mean(T8),sd=sd(T8)) 
yfit <- yfit*diff(h$mids[1:2])*length(T8) 
lines(xfit, yfit, col="blue", lwd=2)

shapiro.test(T8)  # fail to reject the null hypothesis that sample comes from a population 
  # with normal distribution

# ----------------- Regression Model ------------------
# model 1 includes all numerical and binary categorical variables in the model 
model1 = lm(T8~ T3 + T5 + ENRL + DENS + (IMTR==1) + (IM==1) + (IL==1) + PLUN, data = data)
summary(model1)
# make sure the categorical indicators are coded as 1 == TRUE in reg
vif(model1) # no coefficient has very high multicolinearity

# model 2 includes ln(DENS) and ln(ENRL)
model2 = lm(T8~ T3 + T5 + log(ENRL) + log(DENS) + (IMTR==1) + (IM==1) + (IL==1) + PLUN, data = data)
summary(model2)

# MODEL SELECTION: 
# backward selection - instructions for project
step1 = stepAIC(model1, direction = 'backward', data = data)
step1$anova
step2 = stepAIC(model1, direction = 'both', data = data)
step2$anova

# model3 is the one identified as the best by BACKWARD

# This is still not as good: T8 ~ T3 + T5 + log(ENRL) + log(DENS) + IMTR + IM + PLUN - IM
model3 = lm(T8~ T3 + T5 + ENRL + DENS + (IMTR==1) + (IM==1) + PLUN, data = data)
summary(model3)

vif(model3) # no coefficient has very high multicolinearity
summary(influence.measures(model3)) # attempting to find influential points

par(mfrow=c(1,1))

# another way to check best model: use BIC criterion
all = regsubsets(T8~(T3 + T5 + DENS + ENRL + PLUN + IMTR + IM + IL),data =data, nvmax = 10, really.big = T)
summary(all)
summary(all)$bic
plot(1:8, summary(all)$bic,type="b",pch=19,col="sienna")
subsets(all, statistic = "bic", main = 'Best Model to Predict T8') 
# 3 VARIABLES = BEST MODEL but this is based on stricter p-value threshold (0.01)

# ----------------- Final Regression Model ------------------
modelfinal = lm(T8~ T3 + T5 + ENRL + DENS + (IMTR) + (IM) + PLUN, data = data)
summary(modelfinal)
anova = anova(modelfinal)
aov(modelfinal)
write.csv(anova, file = "anova.csv", quote=F)

# tried interaction term between T3 and T5 in the model: 
# (T3:T5 + T3 + T5) and (T3:T5) but term was never significant

# ----------------- Regression Diagnostics ------------------
par(mfrow=c(2,2))
plot(modelfinal)

# ----------------- Cook's Distance/Influential Points ------------------

summary(influence.measures(modelfinal))
par(mfrow=c(1,1))
plot(cooks.distance(modelfinal))

# ----------------- Plotting Observed vs. Predicted Values ------------------
# regression residual values 
residuals = (summary(modelfinal))$residuals
residuals

# predicted values from regression
predicted = predict(modelfinal) 

# plot of observed T8 versus predicted T8
plot(T8~predict(modelfinal), main = 'Observed T8 vs Predicted T8 Values', xlab = 'Predicted', 
     ylab = 'Observed')

# abline(modelfinal) hard to do for all 8 predictors at once

# ----------------- Durbin-Watson Test ------------------
# tests for time dependence among residuals
dwt(residuals)
# ----------------- Chi-Squared Test ------------------
chisq.test(abs(residuals))

# ----------------- Breusch-Pagan Test ------------------
# tests for homoskedasticity or non-constant variance (again)
ncvTest(modelfinal)

# ----------------- Variance Inflation Factor ------------------
vif(modelfinal)

# ----------------- Model Comparison ------------------
# null model - intercept is expected value of T8 (mean) 
# all rest beta parameters are 0
# meanT8 = as.numeric(mean(T8))
# meanT8rep = rep((meanT8rep), 174)
# meanT8rep = c(meanT8rep)
# meanT8rep
# is.numeric(meanT8rep)
#cbind(meanT8rep)

modelnull = lm(T8 ~ 1)
summary(modelnull)

# full model - all numerical and binary cateogrical
modelfull = model1
modelfull
summary(modelfull)
write.csv(fullsum, file = "fullmodelreg.csv", quote=F)


# final model - T3, T5, ENRL, DENS, IMTR IM, PLUN
modelfinal
summary(modelfinal)

#compare the two
anovacompare = anova(modelnull, modelfull, modelfinal)
write.csv(anovacompare, file = "modelcomparison.csv", quote=F)

# ----------------- Predicting Value ------------------
data
# predict T8 for APPLING County
64.13 + (0.181*106) + (0.3686*116) - (0.000195*3494) +(.008228*32.10) -(3.221*0) - (3.319*1) - (44.85*0.57184) #105 actual and 96.69
# predict T8 for VALDOSTA County
64.13 + (0.181*97) + (0.3686*98) - (0.000195*7455) +(.008228*166.53) -(3.221*0) - (3.319*1) - (44.85*0.58471) #84 actual and 88.18304

dwt

#NOTES FROM PRESENTATIONS: 
# mean of categorical variables can indicate if there are 
  # more 0's than 1's (mean<0.5 indicates that there are more 0s)
# issue with distribution skewness with ENRL and DENS
# show response normality
# library leaps - regsubsets
# correlation matrix (must account for account for interaction between T5 and T8 and DENS and ENRL)
# USE "pairs"