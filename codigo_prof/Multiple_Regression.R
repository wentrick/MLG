library(corrplot)
require(plotrix)
library(lmtest)
library(psych) 
library(car)
library(phia)

##----------------------------------##
## Multiple Linear Regression Model ##
##----------------------------------##
## EXAMPLE 1:
##------------------------------------------------------------##
## Os dados da do ficheiro ChimicalPerformance.txt apresentam ## 
## o desempenho de um processo químico em função de diversas  ## 
## variáveis de processo controláveis.                        ##
##------------------------------------------------------------##
## VARIABLE DESCRIPTION:
##
##  y: CO2
## X1: Space time, min.
## X2: Temperature, °C
## X3: Percent solvation
## X4: Oil yield (g/100 g MAF)
## X5: Coal total
## X6: Solvent total
## X7: Hydrogen consumption


Chperfom.data<-read.table("C:/Users/Machado/OneDrive - unb.br/Área de Trabalho/GLM data sets/ChemicalPerform.txt", header=T)
##
head(Chperfom.data)
summary(Chperfom.data)

## psych package To plot multiple histograms multi.hist(dataset)
##multi.hist(Chperfom.data, nrow = 1)

## Multiple Scatterplots
x11()
pairs(Chperfom.data, gap=0.5, pch = 19)

## Correlation Matrix
cor(Chperfom.data)

## Multiple Regression Model
MultipleReg1<- lm(y~., data=Chperfom.data)
##
summary(MultipleReg1)

## Residual Analysis-I
x11()
par(mfrow=c(2,2))
plot(MultipleReg1)

## Residual Analysis-II
x11()
plot(resid(MultipleReg1), ylab="Residuals", xlab="Index")


## Plot the residual x predicted value to validate the hypothesis
x11()
plot(x=predict(MultipleReg1),y=residuals(MultipleReg1))


## Perform the normal Shapiro-Wilk test for the residuals
shapiro.test(residuals(MultipleReg1))

## Perform breush-pagan test for hetereocedascity
bptest(MultipleReg1)

## Perform Durbin-Watson test for Independence
durbinWatsonTest(MultipleReg1)

##----------------------##
## CONFIDENCE INTERVALS ##
##----------------------##
confint(MultipleReg1, level=0.95)

## Analysis of Variance
anova(MultipleReg1)

##----------------------------------##
## Computing the ANOVA sum square's ##
##----------------------------------##
X <- model.matrix(MultipleReg1)
n <- nrow(X)
p <- ncol(X)
##
H <- X%*%solve(t(X)%*%X)%*%t(X)
I <- diag(n)
vec1<-array(1, dim=n)
J <- vec1%*%t(vec1)
##
##
SSTotal <- round(t(Chperfom.data[,"y"])%*%(I-(1/n)*J)%*%Chperfom.data[,"y"],3)
SSError <- round(t(Chperfom.data[,"y"])%*%(I-H)%*%Chperfom.data[,"y"],3)
SSRegres<- round(t(Chperfom.data[,"y"])%*%(H-(1/n)*J)%*%Chperfom.data[,"y"],3)

##--------------------##
## MEAN SQUARE ERRORS ##
##--------------------##
MSError <- round(SSError/(n-p),3)
MSRegres<- round(SSRegres/(p-1),3)
##
Fcal<- round(MSRegres/MSError,3)
Fcrt<- round(qf(0.05,df1=p-1, df2=n-p, lower.tail=F),3)
pval<- round(pf(Fcal,df1=p-1, df2=n-p, lower.tail=F),8)


##--------------------------##
## IMPLEMENTING ANOVA TABLE ##
##--------------------------##
gl.reg  <- p-1
gl.erro <- n-p
gl.total<- n-1
##
Anova.table <- as.table(matrix(c(SSRegres, gl.reg  , MSRegres, Fcal, pval,
                                 SSError , gl.erro , MSError ,  " ",  " ",
                                 round(SSTotal,3) , gl.total,      " ", " ", ""), 
                               ncol=5, byrow=TRUE))
colnames(Anova.table) <- c('SumSquare','df', 'MeanSquare', 'Fcal', 'p-value')
rownames(Anova.table) <- c('Regression','Error','Total')
as.table(Anova.table)



##-------------------##
## Covariance Matrix ##
##-------------------##
##
vcov(MultipleReg1) # covariance matrix for model parameters
influence(MultipleReg1) # regression diagnostics
influence.measures(MultipleReg1) # Table with the main influence measures
hatvalues(MultipleReg1) # for the hat matrix diagonals
covratio(MultipleReg1)
dffits(MultipleReg1) #DFMultipleReg1S
dfbetas(MultipleReg1) #DFBETAS
cooks.distance(MultipleReg1) #Cook's Distance
##
##
##
##
##
##
##
##
##
##
##
##
##-----------##
## EXAMPLE 2 ##
##-----------##
## Obs: Observações
##   Y: Tempo de entrega (em minutos)
##  X1: Número de casos
##  X2: Distância (em 1000 pés)

MultipleReg2<-read.table("C:/Users/Machado/OneDrive - unb.br/Área de Trabalho/GLM data sets/MultipleReg_ex1.txt", header=T)
##
head(MultipleReg2)

## Multiple Scatterplots
x11()
pairs(MultipleReg2, gap=0.5, pch = 19)

## Correlation Matrix
cor(MultipleReg2)


## Multiple Regression Model
MultipleReg2<- lm(Y~X1+X2, data=MultipleReg2)
##
X <- model.matrix(MultipleReg2)

## Print the summary of the output of MultipleReg2
summary(MultipleReg2)

# CIs for model parameters
confint(MultipleReg2, level=0.95)

## Plot Q-Q normal plot of the residuals
qqnorm(residuals(MultipleReg2))

## Plot the residual x predicted value to validate the hypothesis
plot(x=predict(MultipleReg2),y=residuals(MultipleReg2))

## Perform the normal Shapiro-Wilk test for the residuals
shapiro.test(residuals(MultipleReg2))
##
##
## Other useful functions
coefficients(MultipleReg2) # model coefficients
fitted(MultipleReg2) # predicted values
residuals(MultipleReg2) # residuals
residuals(MultipleReg2)/sqrt(sum(residuals(MultipleReg2)^2)/(length(residuals(MultipleReg2))-ncol(X)))# standadized residuals
rstandard(MultipleReg2) # internaly studentized residuals
rstudent(MultipleReg2) # externaly studentized residuals 
anova(MultipleReg2) # anova table
vcov(MultipleReg2) # covariance matrix for model parameters
influence(MultipleReg2) # regression diagnostics
influence.measures(MultipleReg2) # Table with the main influence measures
hatvalues(MultipleReg2) # for the hat matrix diagonals
covratio(MultipleReg2)
dffits(MultipleReg2) #DFMultipleReg2S
dfbetas(MultipleReg2) #DFBETAS
cooks.distance(MultipleReg2) #Cook's Distance

##For SAS anova table
#source("SASAnova.R")
#SASanova(MultipleReg2)

##For VIF 
library("car")
vif(MultipleReg2)

## Perform breush-pagan test for hetereocedascity
library(lmtest)
bptest(MultipleReg2)

## Perform white test for hetereocedascity
library(bstats)
white.test(MultipleReg2)

##------------------------------##
## Computing ANOVA sum square's ##
##------------------------------##
X <- model.matrix(MultipleReg2)
n <- nrow(X)
p <- ncol(X)
##
H <- X%*%solve(t(X)%*%X)%*%t(X)
I <- diag(n)
vec1<-array(1, dim=n)
J <- vec1%*%t(vec1)
##
SSTotal <- round(t(Chperfom.data[,"y"])%*%(I-(1/n)*J)%*%Chperfom.data[,"y"],3)
SSError <- round(t(Chperfom.data[,"y"])%*%(I-H)%*%Chperfom.data[,"y"],3)
SSRegres<- round(t(Chperfom.data[,"y"])%*%(H-(1/n)*J)%*%Chperfom.data[,"y"],3)

##--------------------##
## MEAN SQUARE ERRORS ##
##--------------------##
MSError <- round(SSError/(n-p),3)
MSRegres<- round(SSRegres/(p-1),3)
##
Fcal<- round(MSRegres/MSError,3)
Fcrt<- round(qf(0.05,df1=p-1, df2=n-p, lower.tail=F),3)
pval<- pf(Fcal,df1=p-1, df2=n-p, lower.tail=F)

##--------------------------##
## IMPLEMENTING ANOVA TABLE ##
##--------------------------##
gl.reg  <- p-1
gl.erro <- n-p
gl.total<- n-1
##
Anova.table <- as.table(matrix(c(SSRegres, gl.reg  , MSRegres, Fcal,
                                 SSError , gl.erro , MSError ,  " ",
                                 round(SSTotal,3) , gl.total,      " ", " "), 
                               ncol=4, byrow=TRUE))
colnames(Anova.table) <- c('SumSquare','df', 'MeanSquare', 'Fcal')
rownames(Anova.table) <- c('Regression','Error','Total')
as.table(Anova.table)