# Maximo Camacho, Salvador Ramallo & Manuel Ruiz
# www.um.es/econometria/Maximo
# November 2021 @
# This R file reads in data, sets options, and calls numerical 
# optimization routine for numerical estimation of the univariate switching model:
#   Yt = C(st) + Et   Et ~ N(0,V)   st=1,2      @


remove(list=ls())   # Remove global environment
set.seed(1243)
cat("\f")           # Clear the screen
graphics.off()      # Close the current graphical device
set.seed(1)
require("readxl")
require(pracma)
library(openxlsx)
library(pROC)
library(caret)
library(zoo)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#############################################################
#                      LOAD DATA                            #
#############################################################
source("functions.R")
mydata2 <- read.xlsx('dataUS_21.xlsx')
mydata<-mydata2
#mydata <- mydata[36:dim(mydata)[1],]  #para Francia
#mydata <- mydata[16:dim(mydata)[1],]   #para Germany
#mydata<-mydata[1:(dim(mydata)[1]-11),] #data up to 2019.4
fechas<-data.matrix(mydata[,1])
ecri <- mydata[,2]; #2 para usa
y<-data.matrix(mydata[,3]) #3 para USA
T<-length(y)
captst<-T
prec<-rep(0,captst)
prect1<-rep(0,captst)
prect2<-rep(0,captst)
prect3<-rep(0,captst)

#############################################################
#                    SOME PROCEDURES                        #
#                          HERE                             #
#############################################################
ofn<-function(th2,y){
  th=trans(th2)
  captst<-T
  pt0=(1-th[4])/(2-th[4]-th[5])
  pt1=(1-th[5])/(2-th[4]-th[5])
  #pt0=0.5
  #pt1=0.5
  ft=0
  for (it in 1:captst){
    ft0 <- (1/sqrt(2*pi*th[3]))*exp((-0.5/th[3])*(y[it]-th[1])^2)
    ft1 <- (1/sqrt(2*pi*th[3]))*exp((-0.5/th[3])*(y[it]-th[2])^2)
    ftotal <- pt0*ft0+pt1*ft1
  
    ptt0 <- pt0*ft0/ftotal
    ptt1 <- pt1*ft1/ftotal
    prec[it]<<-ptt1

    ft <- ft + log(ftotal)
    
    
    p00 <- th[4]
    p01 <- 1-th[4]
    p10 <- 1-th[5]
    p11 <- th[5]
    pt0 <- p00*ptt0+p10*ptt1
    pt1 <- p01*ptt0+p11*ptt1
    #esta es la pt+1|t
    prect1[it] <<- pt1
    
    
    #y los pt+2|t, volviendo a multiplicar por la de transici?n
    pt20 <- p00*pt0+p10*pt1
    pt21 <- p01*pt0+p11*pt1
    prect2[it] <<- pt21
    
    #y los pt+3|t, volviendo a multiplicar por la de transici?n
    pt30 <- p00*pt20+p10*pt21
    pt31 <- p01*pt20+p11*pt21
    prect3[it] <<- pt31
    
    }
  return(-ft)
}




trans <- function(teta){
  th <-teta;
  th[3] <- teta[3]^2;
  th[4]<- teta[4]^2/(1+teta[4]^2);
  th[5] <- teta[5]^2/(1+teta[5]^2); 
  return(th)
  
}

#############################################################
#             INITIAL PARAMS' VALUES                        #
#############################################################
mu0 <- 1;#1 todos,1 Fra, 0 Ita, (3 Jap podría ser, pero esa da que Japon apenas ha estado en crisis, la estandar da que Japon lleva desde los 90 estancada que es más cierto)
mu1 <- -1;#-3 todos, -1 Fra en 2019 y -3 para 2022, -5 Ita, (-3 Jap podría ser). -1 en US 
#mu1 <- 0.45
 #sig1 <- sd(y);
sig1 <- 1;
p00 <- 1;p11 <--1;
startval<-c(mu0,mu1,sig1,p00,p11)

#############################################################
#             OPTIMIZATION PROCEDURES                       #
#############################################################
opti <- optim(startval,ofn,gr="BFGS",y)
theta <- opti$par
thn <- trans(theta)



if(theta[1]>theta[2]){
   
   probt <- prec
   
   probt1 <- prect1
   
   probt2 <- prect2
   
   probt3 <- prect3
   
}else{
   
   probt <- prec
   
   
   probt1 <- 1-prect1
   
   probt2 <- 1-prect2
   
   probt3 <- 1-prect3
   
}

startval <- theta





#Genero los comparadores

yy <- y

  


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
 

#We plot the case for example of prob at t+2 vs NBER
Pr<-ts(probt2[3:length(probt2)],start=c(1955,4),frequency=4)
plot(Pr, col="black",xlab="year",ylab="",main = "Filter probabilities",lwd = 1.8)
#lines(EcriRoc2,type="l,col=2)
# Note PEAKS: 1960Q1=1960.0; 1960Q2=1960.2; 1960Q3=1960.4; 1960Q4=1960.6
# Note TROUGHS: 1960Q1=1960.2; 1960Q2=1960.4; 1960Q3=1960.6; 1960Q4=1960.8
rect(xleft=1957.6,xright=1958.4,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1960.2,xright=1961.2,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1969.6,xright=1970.8,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1973.6,xright=1975.2,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1980.0,xright=1980.6,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1981.4,xright=1982.8,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=1990.4,xright=1991.2,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=2001.0,xright=2001.8,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=2007.6,xright=2009.4,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)
rect(xleft=2020.0,xright=2020.4,ybottom=-10,ytop=10, col=rgb(red=0,green=0.5,blue=1,alpha=0.2),border = NA)

#write.xlsx(data.frame(probt2[3:length(probt2)]),"Probabilities_US_MS.xlsx")



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

































#we proceed the same for the whole sample
mydata<-mydata2
fechas<-data.matrix(mydata[,1])
y<-data.matrix(mydata[,3])
T<-length(y)
captst<-T
prec<-rep(0,captst)
prect1<-rep(0,captst)
prect2<-rep(0,captst)
prect3<-rep(0,captst)




mu0 <- 1;
mu1 <- -3;
#mu1 <- 0.45
#sig1 <- sd(y);
sig1 <- 1;
p00 <- 1;p11 <--1;
startval<-c(mu0,mu1,sig1,p00,p11)

#############################################################
#             OPTIMIZATION PROCEDURES                       #
#############################################################
opti <- optim(startval,ofn,gr="BFGS",y)
theta <- opti$par
thn <- trans(theta)



if(theta[1]>theta[2]){
  
  probt <- prec
  
  probt1 <- prect1
  
  probt2 <- prect2
  
  probt3 <- prect3
  
}else{
  
  probt <- prec
  
  
  probt1 <- 1-prect1
  
  probt2 <- 1-prect2
  
  probt3 <- 1-prect3
  
}

startval <- theta





#Genero los comparadores
ecri <- mydata[,2];



yy <- y




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


#We plot the case for example of prob at t+2 vs NBER
Pr<-ts(probt2[3:length(probt2)],start=c(1955,4),frequency=4)
plot(Pr, col="black",xlab="year",ylab="",main = "Filter probabilities",lwd = 1.8)
#lines(EcriRoc2,type="l,col=2)
# Note PEAKS: 1960Q1=1960.0; 1960Q2=1960.2; 1960Q3=1960.4; 1960Q4=1960.6
# Note TROUGHS: 1960Q1=1960.2; 1960Q2=1960.4; 1960Q3=1960.6; 1960Q4=1960.8
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



