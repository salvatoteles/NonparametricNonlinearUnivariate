######################################################
#Program for AR2 estimation 
#M?ximo Camacho, Salvador Ramallo, Manuel Ruiz
#November 2021
##########################################################


library(openxlsx)
library(pROC)
library(statcomp)
library(zoo)
library(forecast)
library(MASS)
require(pracma)
library(caret)
rm(list=ls())
set.seed(1243)



##################################################
#ESTIMATION
################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions.R")
#Load data
pib <- read.xlsx('dataUS_21.xlsx')
#mydata <- mydata[36:dim(mydata)[1],]  #para Francia
#mydata <- mydata[16:dim(mydata)[1],]   #para Germany
pib<-pib[1:(dim(pib)[1]-10),] #data up to 2019.4
pib1 <- pib[,3];
pib1 <- detrend(pib1)  #needed in 2019 for USA,UK, Can, Fra y todos en general
ecri <- pib[,2];#or 4, what out
fechas <- pib[,1];
T<-length(pib1)



#empty vectors of predictions
pred <- rep(0,100)
predt2 <- rep(0,100)
predt3 <- rep(0,100)




fore <- rep(0,length(pib1))
probt1 <- rep(0,length(pib1))
probt2 <- rep(0,length(pib1))
probt3 <- rep(0,length(pib1))


for (i in 3:length(pib1)){

  
  yy0 <- as.ts(pib1)
  yy <- as.ts(pib1[1:i])
 

#fit
AR <- arima(yy0, order = c(2,0,0))


cuenta_pos <- 0
cuenta_post2 <- 0
cuenta_post3 <- 0

  #For prediction. Create the multivariate,
#also a vector of means (t+3,t+2 y t+1) y matrix of covariances, with sigma11=AR$sigma2
  mu <- c((1 + as.numeric(AR$coef[1]) + as.numeric(AR$coef[2]) + (as.numeric(AR$coef[1]))^2)*(as.numeric(AR$coef[3])) + ((as.numeric(AR$coef[1]))^3 + 2*as.numeric(AR$coef[1])*as.numeric(AR$coef[2]))*(yy[length(yy)]) + (((as.numeric(AR$coef[1]))^2)*as.numeric(AR$coef[2]) + ((as.numeric(AR$coef[2]))^2))*(yy[length(yy)-1]),
          (1 + as.numeric(AR$coef[1]))*as.numeric(AR$coef[3]) + ((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2]))*(yy[length(yy)]) + as.numeric(AR$coef[1])*as.numeric(AR$coef[2])*(yy[length(yy)-1]),
          as.numeric(AR$coef[3]) + as.numeric(AR$coef[1])*(yy[length(yy)]) + as.numeric(AR$coef[2])*(yy[length(yy)-1]))
  
  mu <- unname(mu)
  
  sigma <- matrix(c((((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2]))^2 + (as.numeric(AR$coef[1]))^2 + 1)*AR$sigma2,(as.numeric(AR$coef[1])*(((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2])) + as.numeric(AR$coef[1])))*AR$sigma2,((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2]))*AR$sigma2,
                    (as.numeric(AR$coef[1])*(((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2])) + as.numeric(AR$coef[1])))*AR$sigma2,(((as.numeric(AR$coef[1]))^2) + 1)*AR$sigma2,as.numeric(AR$coef[1])*AR$sigma2,
                    ((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2]))*AR$sigma2, as.numeric(AR$coef[1])*AR$sigma2, AR$sigma2),
                  nrow=3,ncol=3,byrow=TRUE)
  
#Need Cholesky decomposition so that the matrix is positive-definite
  p <- chol(sigma)

  
  predicciones <- mvrnorm(100, mu, p%*%t(p) )
  
  for (j in 1:100){
  
pred[j] <- mu[3] + predicciones[j,3]
predt2[j] <- mu[2] + predicciones[j,2]
predt3[j] <- mu[1] + predicciones[j,1]


  
  if (yy[i] <0 && pred[j]<0){
    cuenta_pos <- cuenta_pos +1
  }

  if (pred[j] <0 && predt2[j]<0){
    cuenta_post2 <- cuenta_post2 +1
  }  

  if (predt2[j] <0 && predt3[j]<0){
    cuenta_post3 <- cuenta_post3 +1
  }    
  
}




fore[i] <- mean(pred)
probt1[i] <- cuenta_pos/100
probt2[i] <- cuenta_post2/100
probt3[i] <- cuenta_post3/100



}

#Comparator generation

EcriRoc <- rep(0,T)
EcriRoc2 <- rep(0,T)
EcriRoc3 <- rep(0,T)

for (i in 1:(T-3)) {
  if (ecri[i]>0){
    EcriRoc[i]<- 1
  }
  if (ecri[i]>0){
    EcriRoc2[i]<- 1
  }
  if (ecri[i]>0){
    EcriRoc3[i]<- 1
  }
  
  
}






#We plot the case for example of prob at t+2 vs NBER
Pr<-ts(probt2,start=c(1960,2),frequency=4)
plot(Pr, col="black",xlab="year",ylab="",main = "Probabilities",lwd = 1.8,ylim=c(0,1))
#lines(EcriRoc2,type="l,col=2)
rect(xleft=1957.6,xright=1958.4,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1960.2,xright=1961.2,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1969.6,xright=1970.8,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1973.6,xright=1975.2,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1980.0,xright=1980.6,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1981.4,xright=1982.8,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1990.4,xright=1991.2,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=2001.0,xright=2001.8,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=2007.6,xright=2009.4,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=2020.0,xright=2021.2,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)

#write.xlsx(data.frame(probt2[3:length(probt2)]),"Probabilities_US_AR.xlsx")
#plot(forecast(Arima(ts(yy0[200:length(yy0)],start=c(2005,1),frequency=4),order = c(2,0,0))),main="")
#abline(h=0,lty=2)

##################################################################
#Stats

thresholds <- seq(0,1,0.001)  #para la auc_manual function
speci <- rep(0,length(thresholds))
sensi <- rep(0,length(thresholds))
Kappa <- rep(0,length(thresholds))

#los thresholds optimos del in sample 2019
thres1 <- 0.165
thres2 <- 0.021
thres3 <- 0.010


table <- data.frame(probt1[3:(T)],EcriRoc[3:(T)],(probt1[3:(T)]-EcriRoc[3:(T)])^{2})
colnames(table) <- c("probt151","EcriRoc51", "bla")

#table2 <- data.frame(Prob3b[51:T],xROC2[51:T],(Prob3b[51:T]-xROC2[51:T])^{2})
table2 <- data.frame(probt2[3:(T)],EcriRoc2[3:(T)],(probt2[3:(T)]-EcriRoc2[3:(T)])^{2})
colnames(table2) <- c("probt251","EcriRoc251", "bla")

#table3 <- data.frame(Prob4b[51:T],xROC3[51:T],(Prob4b[51:T]-xROC3[51:T])^{2})
table3 <- data.frame(probt3[3:(T)],EcriRoc3[3:(T)],(probt3[3:(T)]-EcriRoc3[3:(T)])^{2})
colnames(table3) <- c("probt351","EcriRoc351", "bla")

#Ahora calculo el AUROC

AUROCPaisesRoc <- auc(table$EcriRoc51,table$probt151)
AUROCPaisesRoc2 <- auc(table2$EcriRoc251,table2$probt251)
AUROCPaisesRoc3 <- auc(table3$EcriRoc351,table3$probt351)

#veamos el brier
tableRefRec <- subset(table[which(table$EcriRoc51 == 1),])
tableRefExp <- subset(table[which(table$EcriRoc51 == 0),])

tableRefRec2 <- subset(table2[which(table2$EcriRoc251 == 1),])
tableRefExp2 <- subset(table2[which(table2$EcriRoc251 == 0),])

tableRefRec3 <- subset(table3[which(table3$EcriRoc351 == 1),])
tableRefExp3 <- subset(table3[which(table3$EcriRoc351 == 0),])


Brier <- sum(table[,3])/length(table[,3])
BrierRec <- sum(tableRefRec[,3])/length(tableRefRec[,3])
BrierExp <- sum(tableRefExp[,3])/length(tableRefExp[,3])

Brier2 <- sum(table2[,3])/length(table2[,3])
BrierRec2 <- sum(tableRefRec2[,3])/length(tableRefRec2[,3])
BrierExp2 <- sum(tableRefExp2[,3])/length(tableRefExp2[,3])

Brier3 <- sum(table3[,3])/length(table3[,3])
BrierRec3 <- sum(tableRefRec3[,3])/length(tableRefRec3[,3])
BrierExp3 <- sum(tableRefExp3[,3])/length(tableRefExp3[,3])


#AUC manual el threshold, kappa, TPR y FPR
resultados_1 <- auc_manual_v2(table$EcriRoc51,table$probt151,thres1)
resultados_2 <- auc_manual_v2(table2$EcriRoc251,table2$probt251,thres2)
resultados_3 <- auc_manual_v2(table3$EcriRoc351,table3$probt351,thres3)

AUC_mano1_v2 <- resultados_1[[1]]
TNR_mano1_v2 <- resultados_1[[2]]
TPR_mano1_v2 <- resultados_1[[4]]
threshold_mano1_v2 <- resultados_1[[5]]
Kappa_mano1_v2 <- resultados_1[[3]]
mediaTPR_mano1_v2 <- resultados_1[[6]]
mediaFPR_mano1_v2 <- resultados_1[[8]]
mediaTNR_mano1_v2 <- resultados_1[[7]]
mediaFNR_mano1_v2 <- resultados_1[[9]]
mediaKappa_mano1_v2 <- resultados_1[[10]]


AUC_mano2_v2 <- resultados_2[[1]]
TNR_mano2_v2 <- resultados_2[[2]]
TPR_mano2_v2 <- resultados_2[[4]]
threshold_mano2_v2 <- resultados_2[[5]]
Kappa_mano2_v2 <- resultados_2[[3]]
mediaTPR_mano2_v2 <- resultados_2[[6]]
mediaFPR_mano2_v2 <- resultados_2[[8]]
mediaTNR_mano2_v2 <- resultados_2[[7]]
mediaFNR_mano2_v2 <- resultados_2[[9]]
mediaKappa_mano2_v2 <- resultados_2[[10]]


AUC_mano3_v2 <- resultados_3[[1]]
TNR_mano3_v2 <- resultados_3[[2]]
TPR_mano3_v2 <- resultados_3[[4]]
threshold_mano3_v2 <- resultados_3[[5]]
Kappa_mano3_v2 <- resultados_3[[3]]
mediaTPR_mano3_v2 <- resultados_3[[6]]
mediaFPR_mano3_v2 <- resultados_3[[8]]
mediaTNR_mano3_v2 <- resultados_3[[7]]
mediaFNR_mano3_v2 <- resultados_3[[9]]
mediaKappa_mano3_v2 <- resultados_3[[10]]






simulaciones <- 1

AUROCmedio <- sum(AUROCPaisesRoc)/simulaciones
AUROCmedio_mano1 <-  sum(AUC_mano1_v2)/simulaciones
#TNR_medio_mano1 <- sum(TNR_mano1_v2)/simulaciones
#TPR_medio_mano1 <- sum(TPR_mano1_v2)/simulaciones
threshold_medio_mano1 <- sum(threshold_mano1_v2)/simulaciones
Kappa_medio_mano1 <- sum(mediaKappa_mano1_v2)/simulaciones
BriermedioRec <- sum(BrierRec)/simulaciones
BriermedioExp <- sum(BrierExp)/simulaciones
Briermedio <- sum(Brier)/simulaciones
mediaTPR <-  sum(mediaTPR_mano1_v2)/simulaciones
mediaFPR <- sum(mediaFPR_mano1_v2)/simulaciones
mediaTNR <- sum(mediaTNR_mano1_v2)/simulaciones
mediaFNR <- sum(mediaFNR_mano1_v2)/simulaciones

AUROCmedio2 <- sum(AUROCPaisesRoc2)/simulaciones
AUROCmedio_mano2 <-  sum(AUC_mano2_v2)/simulaciones
#TNR_medio_mano2 <- sum(TNR_mano2_v2)/simulaciones
#TPR_medio_mano2 <- sum(TPR_mano2_v2)/simulaciones
threshold_medio_mano2 <- sum(threshold_mano2_v2)/simulaciones
Kappa_medio_mano2 <- sum(mediaKappa_mano2_v2)/simulaciones
BriermedioRec2 <- sum(BrierRec2)/simulaciones
BriermedioExp2 <- sum(BrierExp2)/simulaciones
Briermedio2 <- sum(Brier2)/simulaciones
mediaTPR2 <-  sum(mediaTPR_mano2_v2)/simulaciones
mediaFPR2 <- sum(mediaFPR_mano2_v2)/simulaciones
mediaTNR2 <- sum(mediaTNR_mano2_v2)/simulaciones
mediaFNR2 <- sum(mediaFNR_mano2_v2)/simulaciones

AUROCmedio3 <- sum(AUROCPaisesRoc3)/simulaciones
AUROCmedio_mano3 <-  sum(AUC_mano3_v2)/simulaciones
#TNR_medio_mano3 <- sum(TNR_mano3_v2)/simulaciones
#TPR_medio_mano3 <- sum(TPR_mano3_v2)/simulaciones
threshold_medio_mano3 <- sum(threshold_mano3_v2)/simulaciones
Kappa_medio_mano3 <- sum(mediaKappa_mano3_v2)/simulaciones
BriermedioRec3 <- sum(BrierRec3)/simulaciones
BriermedioExp3 <- sum(BrierExp3)/simulaciones
Briermedio3 <- sum(Brier3)/simulaciones
mediaTPR3 <-  sum(mediaTPR_mano3_v2)/simulaciones
mediaFPR3 <- sum(mediaFPR_mano3_v2)/simulaciones
mediaTNR3 <- sum(mediaTNR_mano3_v2)/simulaciones
mediaFNR3 <- sum(mediaFNR_mano3_v2)/simulaciones


####################Print
AUROCmedio
AUROCmedio_mano1
#TNR_medio_mano1
#TPR_medio_mano1
threshold_medio_mano1
Kappa_medio_mano1
BriermedioExp
BriermedioRec
Briermedio
mediaTPR
mediaFPR
mediaTNR
mediaFNR
#TPR1_medio 
#TNR1_medio 
#threshold_medio_1 
#Kappa_medio_1 
AUROCmedio2 
AUROCmedio_mano2 
#TNR_medio_mano2 
#TPR_medio_mano2 
threshold_medio_mano2 
Kappa_medio_mano2 
BriermedioExp2
BriermedioRec2
Briermedio2
mediaTPR2
mediaFPR2
mediaTNR2
mediaFNR2
#TPR2_medio 
#TNR2_medio 
#threshold_medio_2 
#Kappa_medio_2 
AUROCmedio3 
AUROCmedio_mano3 
#TNR_medio_mano3 
#TPR_medio_mano3 
threshold_medio_mano3 
Kappa_medio_mano3
BriermedioExp3
BriermedioRec3
Briermedio3
mediaTPR3
mediaFPR3
mediaTNR3
mediaFNR3



































#We proceed the same way with the whole sample
pib <- read.xlsx('dataUS_21.xlsx')
pib2 <- pib[,3];
pib2 <- detrend(pib2)
fechas <- pib[,1];
T<-length(pib2)


pred <- rep(0,100)
predt2 <- rep(0,100)
predt3 <- rep(0,100)




fore <- rep(0,length(pib2))
probt1 <- rep(0,length(pib2))
probt2 <- rep(0,length(pib2))
probt3 <- rep(0,length(pib2))


for (i in 3:length(pib2)){
  
  
  yy0 <- as.ts(pib2)
  yy <- as.ts(pib2[1:i])
  
  
  #fit
  AR <- arima(yy0, order = c(2,0,0))
  
  
  cuenta_pos <- 0
  cuenta_post2 <- 0
  cuenta_post3 <- 0
  
  #For prediction. Create the multivariate,
  #also a vector of means (t+3,t+2 y t+1) y matrix of covariances, with sigma11=AR$sigma2
  mu <- c((1 + as.numeric(AR$coef[1]) + as.numeric(AR$coef[2]) + (as.numeric(AR$coef[1]))^2)*(as.numeric(AR$coef[3])) + ((as.numeric(AR$coef[1]))^3 + 2*as.numeric(AR$coef[1])*as.numeric(AR$coef[2]))*(yy[length(yy)]) + (((as.numeric(AR$coef[1]))^2)*as.numeric(AR$coef[2]) + ((as.numeric(AR$coef[2]))^2))*(yy[length(yy)-1]),
          (1 + as.numeric(AR$coef[1]))*as.numeric(AR$coef[3]) + ((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2]))*(yy[length(yy)]) + as.numeric(AR$coef[1])*as.numeric(AR$coef[2])*(yy[length(yy)-1]),
          as.numeric(AR$coef[3]) + as.numeric(AR$coef[1])*(yy[length(yy)]) + as.numeric(AR$coef[2])*(yy[length(yy)-1]))
  
  mu <- unname(mu)
  
  sigma <- matrix(c((((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2]))^2 + (as.numeric(AR$coef[1]))^2 + 1)*AR$sigma2,(as.numeric(AR$coef[1])*(((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2])) + as.numeric(AR$coef[1])))*AR$sigma2,((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2]))*AR$sigma2,
                    (as.numeric(AR$coef[1])*(((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2])) + as.numeric(AR$coef[1])))*AR$sigma2,(((as.numeric(AR$coef[1]))^2) + 1)*AR$sigma2,as.numeric(AR$coef[1])*AR$sigma2,
                    ((as.numeric(AR$coef[1]))^2 + as.numeric(AR$coef[2]))*AR$sigma2, as.numeric(AR$coef[1])*AR$sigma2, AR$sigma2),
                  nrow=3,ncol=3,byrow=TRUE)
  
  #Need Cholesky decomposition so that the matrix is positive-definite
  p <- chol(sigma)
  
  
  predicciones <- mvrnorm(100, mu, p%*%t(p) )
  
  for (j in 1:100){
    
    pred[j] <- mu[3] + predicciones[j,3]
    predt2[j] <- mu[2] + predicciones[j,2]
    predt3[j] <- mu[1] + predicciones[j,1]
    
    
    
    if (yy[i] <0 && pred[j]<0){
      cuenta_pos <- cuenta_pos +1
    }
    
    if (pred[j] <0 && predt2[j]<0){
      cuenta_post2 <- cuenta_post2 +1
    }  
    
    if (predt2[j] <0 && predt3[j]<0){
      cuenta_post3 <- cuenta_post3 +1
    }    
    
  }
  
  
  
  
  fore[i] <- mean(pred)
  probt1[i] <- cuenta_pos/100
  probt2[i] <- cuenta_post2/100
  probt3[i] <- cuenta_post3/100
  
  
  
}

#Comparator generation
ecri <- pib[,2];

EcriRoc <- rep(0,T)
EcriRoc2 <- rep(0,T)
EcriRoc3 <- rep(0,T)

for (i in 1:(T-3)) {
  if (ecri[i+1]>0){
    EcriRoc[i]<- 1
  }
  if (ecri[i+2]>0){
    EcriRoc2[i]<- 1
  }
  if (ecri[i+3]>0){
    EcriRoc3[i]<- 1
  }
  
  
}




Pr<-ts(probt2[3:length(probt2)],start=c(1955,4),frequency=4)
plot(Pr, col="black",xlab="year",ylab="",main = "Probabilities",lwd = 1.8,ylim=c(0,1))
#lines(EcriRoc2,type="l,col=2)
rect(xleft=1957.6,xright=1958.4,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1960.2,xright=1961.2,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1969.6,xright=1970.8,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1973.6,xright=1975.2,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1980.0,xright=1980.6,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1981.4,xright=1982.8,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1990.4,xright=1991.2,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=2001.0,xright=2001.8,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=2007.6,xright=2009.4,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=2020.3,xright=2020.4,ybottom=-6,ytop=6, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)

