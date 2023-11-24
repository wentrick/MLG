##-----------------------------------------------------------##
##     BINARY DATASET (Source: Montegomery, pag. 487)        ##                  
##-----------------------------------------------------------##
## Em um ensaio clínico descrito por Piegorsch (1992) sobre  ##
## o efeito analgésico do tratamento iontoforético com vincr-## 
##-istina, um medicamento anticancerígeno genérico, pacientes## 
## idosos sofrendo de neuralgia foram randomizados para o    ##
## tratamento ativo ou para um grupo de controle.Os pacientes##
## foram entrevistados seis semanas após o tratamento para   ##
## determinar se alguma melhora na neuralgia era evidente,   ##
## de modo que o...                                          ##
##-----------------------------------------------------------##
##
##
##------------------------------------------------------------##
##            MEDPAR DATA SET (AVAILABLE IN R)                ##
##------------------------------------------------------------##
## O banco de dados nacional do hospital de internação        ##
## Medicare dos EUA é conhecido como dados Medpar, que é      ##
## preparado anualmente a partir de registros de arquivamento ##
## do hospital. Arquivos Medpar para cada estado também são   ##
## preparados. Os dados completos do Medpar consistem em 115  ##
## variáveis. O Medpar nacional possui cerca de 14 milhões de ##
## registros, sendo um registro para cada internação. Os dados##
## no arquivo medpar vêm de arquivos do Medicare de 1991 para ##
## o estado do Arizona. Os dados são limitados a apenas um    ##
## grupo de diagnóstico (DRG 112). Os dados do paciente foram ##
## selecionados aleatoriamente a partir dos dados originais.  ##
##------------------------------------------------------------##
## Factors:                                                   ##
## los: length of hospital stay                               ##
## hmo: Patient belongs to a Health Maintenance Organization, ##
##      binary                                                ##
## white: Patient identifies themselves as Caucasian, binary  ##
## died : Patient died (Response)                             ##
## age80: Patient age 80 and over, binary                     ##
##  type: Type of admission, categorical                      ##
## type1: Elective admission, binary                          ##
## type2: Urgent admission,binary                             ##
## type3: Elective admission, binary                          ##
## provnum: Provider ID                                       ##
##------------------------------------------------------------##
##
data.Logis<-read.table("C:/Users/Machado/OneDrive - unb.br/Área de Trabalho/GLM data sets/Particular-Cases/Automobile-logis.txt", header = T)
##
##load("C:/Users/Machado/OneDrive - unb.br/Área de Trabalho/GLM data sets/Dados-Listas/medpar.rdata")
##data.Logis<- medpar
##
head(data.Logis)
summary(data.Logis)
str(data.Logis)


##---------------##
## GLM R-Package ##
##---------------##
fit.Logis<-glm(y~x1+x2, data = data.Logis, family = binomial(link="logit"))
summary(fit.Logis)

## Confidence intervals for BETAS
require(MASS)
confint(fit.Logis, level = 0.95)

## Model selection
stepAIC(fit.Logis, direction = "both")

## Odds Ratio 
odds<-exp(coef(fit.Logis))
odds

## Confidence intervals for odds
exp(confint(fit.Logis, level = 0.95))

##
##------------------------------------------##
##    ANALYSING THE MODEL GOODNESS OF FIT   ##
##------------------------------------------##
## H0: The specified link function adjust   ##
## correctly the data                       ##
## H1: The specified link function does not ##
## adjust correctly the data                ##
##------------------------------------------##
pchisq(deviance(fit.Logis), df.residual(fit.Logis), lower.tail=FALSE)


##-------------------------------------##
## SOME STATISTICAS BASED ON RESIDUALS ##
##-------------------------------------##

## 1.Pearson Residual sum square
G<- sum(residuals(fit.Logis,type="pearson")^2)
G

##------------------------------##
## 2.Naglekerke (1991) R-square ##
##------------------------------##
Num<- 1-exp((fit.Logis$dev-fit.Logis$null)/nrow(Chperfom))
Den<- 1-exp(-fit.Logis$null/nrow(Chperfom))
R2_Nag<- Num/Den; R2_Nag

##--------------------##
## An alternative R^2 ##
##--------------------##
Null.dev<- fit.Logis$null.deviance
Resi.dev<- deviance(fit.Logis)
##
R2.dev<- (Null.dev-Resi.dev)/Null.dev; R2.dev

##----------------------##
## Analysis of Deviance ##
##----------------------##
anova(fit.Logis, test="Chisq")

## Estimate of disperssion parameter phi
df<- df.residual(fit.Logis) ## residual degree of freedom
dv<- deviance(fit.Logis)    ## residual deviance
phi.est<- dv/df; phi.est

## Goodness-of-fit analysis
pchisq(deviance(fit.Logis), df.residual(fit.Logis), lower=FALSE)


##------------------------------------##
## RESIDUAL ANALYSIS: Usual Residuals ##
##------------------------------------##
res.Logis_ordinar<- resid(fit.Logis, type="response")
res.Logis_working<- resid(fit.Logis, type="working")
res.Logis_pearson<- resid(fit.Logis, type="pearson")
res.Logis_devianc<- resid(fit.Logis, type="deviance")
eta.Logis<- as.matrix(predict(fit.Logis, type="link"))

##
x11()
par(mfrow=c(2,2))
plot(eta.Logis,res.Logis_ordinar, xlab = "Eta.hat", ylab ="Resíduos", main="Resíduos Ordinários")
plot(eta.Logis,res.Logis_working, xlab = "Eta.hat", ylab ="Resíduos", main="Resíduos Workings")
plot(eta.Logis,res.Logis_pearson, xlab = "Eta.hat", ylab ="Resíduos", main="Resíduos de Pearson")
plot(eta.Logis,res.Logis_devianc, xlab = "Eta.hat", ylab ="Resíduos", main="Resíduos Desvio")

##------------------------------------------##
## RESIDUAL ANALYSIS: Studentized Residuals ##
##------------------------------------------##
## For Pearson Residuals
x11()
par(mfrow=c(2,2))
plot(fit.Logis)
##
##
rstudent<- rstudent(fit.Logis)
x11()
plot(rstudent, ylim=c(-2,2))
abline(h=c(-2,2),col="red", lty=1)

##-----------------------------##
## INFLUENCE MEASURES IN GLM'S ##
##-----------------------------##
##
betas<-as.vector(coef(fit.Logis))
X.mat<- model.matrix(fit.Logis)
eta.hat<- as.matrix(predict(fit.Logis, type="link"))
##
##eta.Logis<- X.mat%*%betas

## Hat values
hatvalues(fit.Logis)

## Cook Distance
cooks.distance(fit.Logis)

## Aggregated Measures
influence.measures(fit.Logis)

## Some residual graphs
x11()
par(mfrow=c(2,2))
plot(cooks.distance(fit.Logis), lwd = 2, col = "blue", type="h", ylab = "Distância de Cook")
plot(hatvalues(fit.Logis), type = "h", lwd = 2, col = "blue", ylab= "hii")
plot(hatvalues(fit.Logis), eta.hat, lwd = 2, col = "blue", xlab = "Eta.hat", ylab= "hii")

##
##
##-------------------------------------------##
## Logistic Regression With Separation Issue ##
##-------------------------------------------##
require(logistf)
data(sex2)

str(sexagg)
summary(sexagg)

ni<-1
data.sp<-sexagg[,1:7]
betas0<-rep(0,(ncol(data.sp)-1))
fs.LogisReg(data.sp,betas0,ni) 
##
fit2.usual<-glm(case ~ age+oc+vic+vicl+vis+dia, data=sex2, family = binomial(link="logit"))
summary(fit2.usual)

data(sex2)
lf <- logistf(formula = case ~ age + oc + vic + vicl + vis + dia, data = sex2)
summary(lf)