library(olsrr)   
library(ggplot2)
#library(bstats)
library(lmtest)
library(car)

##-------------------------------------------------------##
##      Simple Linear Regression Model (Example 1:       ##
##          Dados do propelente de foguete)              ##
##-------------------------------------------------------##
## y: Força de cisalhamento (em psi)
## x: Idade dos propalelentes (semanas)

example1_SLRM<-read.table("C:/Users/Machado/OneDrive - unb.br/Área de Trabalho/GLM data sets/SimpleLM_ex1.txt",header=T)
example1_SLRM

## Scatterplot Graph
x11()
plot(example1_SLRM[,"x"],example1_SLRM[,"y"], xlab = "Idade (em semanas)", ylab = "Força (em psi)")

## Adjusted Model
fit.ex1<- lm(example1_SLRM[,"y"]~example1_SLRM[,"x"], data=example1_SLRM)
summary(fit.ex1)

## Regression line
x11()
plot(example1_SLRM[,"x"],example1_SLRM[,"y"], xlab = "Age (in Weeks)", ylab = "Força (em psi)")
abline(fit.ex1,col="red")
##
##
##------------------##
## Model Diagnostic ##
##------------------##
#x11()
#par(mfrow=c(2,2))
#plot(example1_SLRM[,"x"], resid(fit.ex1), ylab = "Ordinary Residuals", xlab = "x")
#abline(h=0, col="red")
#plot(example1_SLRM[,"y"], resid(fit.ex1), ylab = "Ordinary Residuals", xlab = "y")
#abline(h=0, col="red")

##---------------------------------##
## Plot for residuals independence ##
##---------------------------------##
x11()
par(mfrow=c(2,2))
plot(resid(fit.ex1), ylab = "Ordinary Residuals", pch=1) 
##
hist(resid(fit.ex1), xlab = "Ordinary Residuals", main="")
boxplot(resid(fit.ex1))

##---------------##
## Envelope Plot ##
##---------------##
x11()
qqPlot(resid(fit.ex1))
##
##

x11()
par(mfrow=c(2,2))
plot(fit.ex1)




##-------------------------------##
## Perform the Shapiro-Wilk test ##
##-------------------------------##
shapiro.test(resid(fit.ex1))

##-------------------------------##
## Perform breush-pagan test for ##
## heterocedascity               ##
##-------------------------------##
bptest(fit.ex1)

##-------------------------------##
## Perform the independence test ##
##-------------------------------##
dwtest(fit.ex1)
##
##
##
##
##
##
##
##
##-------------------------------------------------------##
##           Description of the olsrr package            ##
##-------------------------------------------------------##
## olsrr offers the following tools                      ##
## to detect influential observations:                   ##
##-------------------------------------------------------##
## - Cook’s D Bar Plot                                   ##
## - Cook’s D Chart                                      ##
## - DFBETAs Panel                                       ##
## - DFFITs Plot                                         ##
## - Studentized Residual Plot                           ##
## - Standardized Residual Chart                         ##
## - Studentized Residuals vs Leverage Plot              ##
## - Deleted Studentized Residual vs Fitted Values Plot  ##
## - Potential Residual Plot                             ##
##-------------------------------------------------------##
##
##-----------##
## EXAMPLE 2 ##
##-----------##
set.seed(2023)
data<- data.frame(rnorm(100,2,4),runif(100))
colnames(data)<-c("y","x")

##
fit<-lm(y~x,data)
##
summary(fit)

x11()
par(mfrow=c(2,2))
plot(fit)

X11()
par(mfrow=c(1,2))
plot(data[,"x"], rstandard(fit), ylab = "Standarized Residuals", xlab="X", las=1, pch=19)
abline(h=c(-2,2), col="red", lwd=2)
##
hist(resid(fit), main="Residual Histogram", xlab = "Ordinary Residuals")

##--------------------##
## Influence measures ##
##--------------------##

dfbetas(fit)
dffits(fit)
covratio(fit)
#### Studentized residuals
x11()
ols_plot_resid_stud(fit)

## Plots
plot(cooks.distance(fit), type="h", main="Cook's distance",
     ylab="D", xlab="Observation number", las=1 )


plot(dffits(fit), type="h", main="DFFITS",
     ylab="DFFITS", xlab="Observation number", las=1 )

dfbi <- 1
plot(dfbetas(fit)[, dfbi + 1], type="h", main="DFBETAS for 
      beta2", ylab="DFBETAS", xlab="Observation number", las=1 )
##
influence.measures(fit)

##----------------------------------------##
## Cook Distance bars (Threshold=4/(n-2)) ##
##----------------------------------------##

x11()
ols_plot_cooksd_bar(fit)

##------------##
## DFBetaS(i) ##
##------------##
ols_plot_dfbetas(fit)

##-----------##
## DFFitS(i) ##
##-----------##
x11()
ols_plot_dffits(fit)

##---------------------------------##
## Plot for residuals independence ##
##---------------------------------##
x11()
par(mfrow=c(2,2))
plot(resid(fit), ylab = "Ordinary Residuals", pch=1) 
##
hist(resid(fit), xlab = "Ordinary Residuals", main="")
boxplot(resid(fit))

##---------------##
## Envelope Plot ##
##---------------##
qqPlot(resid(fit), ylab="Ordinary Residuals", xlab = "Normal Quantiles")

##-------------------------------------##
## Darbin-Watson Test for independence ##
##-------------------------------------##
dwtest(fit) 

shapiro.test(resid(fit))

##--------------------------------------##
## Perform the normal Shapiro-Wilk test ##
## for the residuals                    ##
##--------------------------------------##
shapiro.test(residuals(fit))