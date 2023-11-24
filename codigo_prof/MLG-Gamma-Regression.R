rm(list = ls())
##
Chperfom<-read.table("dados/ChemicalPerform.txt", header=T)

##-----------------------------##
##    DESCRIPTIVE ANALYSIS     ##
##-----------------------------##
hist(Chperfom$y, ylab = "Frequências", xlab = "Variável resposta")
##
str(Chperfom)
summary(Chperfom)

##-----------------------------##
## Fitting the Gama Regression ##
##-----------------------------##
##
fit.gamma<-glm(y~., data = Chperfom, family = Gamma(link="log"))
summary(fit.gamma)
##
##
## Confidence intervals
confint(fit.gamma)

##
##-------------------------------------##
## SOME STATISTICAS BASED ON RESIDUALS ##
##-------------------------------------##

## 1.Pearson Residual sum square
G=sum(residuals(fit.gamma,type="pearson")^2)

pchisq(G,df.residual(fit.gamma),lower.tail = F)


pchisq(deviance(fit.gamma), df.residual(fit.gamma), lower.tail=FALSE)

##------------------------------##
## 2.Naglekerke (1991) R-square ##
##------------------------------##
Num<- 1-exp((fit.gamma$dev-fit.gamma$null)/nrow(Chperfom))
Den<- 1-exp(-fit.gamma$null/nrow(Chperfom))
R2_Nag<- Num/Den; R2_Nag



##--------------------##
## An alternative R^2 ##
##--------------------##
Null.dev<- fit.gamma$null.deviance
Resi.dev<- deviance(fit.gamma)
##
R2.dev<- (Null.dev-Resi.dev)/Null.dev; R2.dev

##----------------------##
## Analysis of Deviance ##
##----------------------##
anova(fit.gamma, test="Chisq")

## Estimate of disperssion parameter phi
df<- df.residual(fit.gamma) ## residual degree of freedom
dv<- deviance(fit.gamma)    ## residual deviance
phi.est<- dv/df; phi.est

## Goodness-of-fit analysis
pchisq(deviance(fit.gamma), df.residual(fit.gamma), lower=FALSE)

eta.gamma<- as.matrix(predict(fit.gamma, type="link"))

##------------------------------------##
## RESIDUAL ANALYSIS: Usual Residuals ##
##------------------------------------##
res.gama_ordinar<- resid(fit.gamma, type="response")
res.gama_working<- resid(fit.gamma, type="working")
res.gama_pearson<- resid(fit.gamma, type="pearson")
res.gama_devianc<- resid(fit.gamma, type="deviance")


par(mfrow=c(2,2))
plot(eta.gamma,res.gama_ordinar, xlab = "Eta.hat", ylab ="Resíduos", main="Resíduos Ordinários")
plot(eta.gamma,res.gama_working, xlab = "Eta.hat", ylab ="Resíduos", main="Resíduos Workings")
plot(eta.gamma,res.gama_pearson, xlab = "Eta.hat", ylab ="Resíduos", main="Resíduos de Pearson")
plot(eta.gamma,res.gama_devianc, xlab = "Eta.hat", ylab ="Resíduos", main="Resíduos Desvio")

##------------------------------------------##
## RESIDUAL ANALYSIS: Studentized Residuals ##
##------------------------------------------##
## For Pearson Residuals

par(mfrow=c(2,2))
plot(fit.gamma)


## For deviance residuals
st.res.dev<- rstudent(fit.gamma, type="deviance")

st.res.per<- rstudent(fit.gamma, type="pearson")


##-----------------------------##
## INFLUENCE MEASURES IN GLM'S ##
##-----------------------------##
##
betas<-as.vector(coef(fit.gamma))
X.mat<- model.matrix(fit.gamma)
eta.hat<- as.matrix(predict(fit.gamma, type="link"))
##
##eta.gamma<- X.mat%*%betas

## Hat values
hatvalues(fit.gamma)

## Cook Distance
cooks.distance(fit.gamma)

## Aggregated Measures
influence.measures(fit.gamma)

## Some residual graphs
par(mfrow=c(2,2))
plot(cooks.distance(fit.gamma), lwd = 2, col = "blue", type="h", ylab = "Distância de Cook")
plot(hatvalues(fit.gamma), type = "h", lwd = 2, col = "blue", ylab= "hii")
plot(hatvalues(fit.gamma), eta.hat, lwd = 2, col = "blue", xlab = "Eta.hat", ylab= "hii")
