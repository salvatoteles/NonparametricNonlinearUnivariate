#Program to estimate technical recessions through symbolic histories weighting
#Maximo Camacho, Salvador Ramallo and Manuel Ruiz
#November 2021
library(openxlsx)
library(pROC)
library(statcomp)
library(zoo)
require(pracma)
library(caret)
rm(list=ls())
set.seed(1243)

#############################################################
#                      LOAD DATA                            #
#############################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("functions.R")
pib <- read.xlsx('dataGer_21.xlsx')
#mydata <- mydata[36:dim(mydata)[1],]  #para Francia
mydata <- mydata[16:dim(mydata)[1],]   #para Germany
pib<-pib[1:(dim(pib)[1]-11),] #data up to 2019.4
pib0 <- pib[,3];  #or 3, depends
fechas <- pib[,1];
ecri <- pib[,2];#or 2, what out
T <- length(pib0)


#Estimation for t+1
Prob2b <- matrix(0,length(pib0),1)
Prob12 <- matrix(0.5,length(pib0),1)
Prob21 <- matrix(0.5,length(pib0),1)


for (k in 8:length(pib0)) {

  pib1 <- pib0[1:k]



#count of patterns
patron <- matrix(-20,length(pib0),2)


for (i in 2:length(pib0)) {
  if (pib0[i]>pib0[i-1]){
    patron[i,1]<- pib0[i]-pib0[i-1];
  }
  if (pib0[i]<pib0[i-1]){
    patron[i,2]<- pib0[i]-pib0[i-1];
  }

}


#total for each pattern
valores <- colSums(patron !=-20)





y <-matrix(0,500,2)  
rnames <- matrix(0,500,1)
for (i in 1:500){
  y[i,]=colSums(patron >(25-i*0.1))
  rnames[i] <- round(25-i*0.1,1)
}

row.names(y) <- rnames
colnames(y) <- c("P12","P21")



  
  #x <- pib1[1:k]
  
 #Prob estimation
  
  
  if (pib1[k]>0){
    Prob2b[k] <-  0
  }
  
  if (pib1[k]<0){
    Prob2b[k] <- ((valores[1]-y[as.character(-round(pib1[k],1)),"P12"])*Prob12[k] + (valores[2]-y[as.character(-round(pib1[k],1)),"P21"])*Prob21[k])/(valores[1]*Prob12[k]+valores[2]*Prob21[k])
    }
  
  
  
}


#Estimation for t+2
Prob3b <- matrix(0,length(pib0),1)
Prob123 <- matrix(0,length(pib0),1)
Prob132 <- matrix(0,length(pib0),1)
Prob312 <- matrix(0,length(pib0),1)
Prob231 <- matrix(0,length(pib0),1)
Prob321 <- matrix(0,length(pib0),1)
Prob213 <- matrix(0,length(pib0),1)

for (k in 8:length(pib0)) {

  #pib1 <- pib0[1:k]
  

patron <- matrix(-30,length(pib0),6)
patroncorto <- matrix(-30,length(pib0),6)
patroncorto12 <- matrix(-30,length(pib0),6)

#counting
for (i in 3:length(pib0)) {
  if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i]>pib0[i-2]){
    patron[i,1]<- pib0[i]-pib0[i-2];
    patroncorto[i,1] <- pib0[i]-pib0[i-1];
    patroncorto12[i,1] <- pib0[i-1]-pib0[i-2];
  }
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i]>pib0[i-2]){
    patron[i,2]<- pib0[i]-pib0[i-2];
    patroncorto[i,2] <- pib0[i]-pib0[i-1];
    patroncorto12[i,2] <- pib0[i-1]-pib0[i-2];
  }
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i]<pib0[i-2]){
    patron[i,3]<- pib0[i]-pib0[i-2];
    patroncorto[i,3] <- pib0[i]-pib0[i-1];
    patroncorto12[i,3] <- pib0[i-1]-pib0[i-2];
  }
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i]<pib0[i-2]){
    patron[i,4]<- pib0[i]-pib0[i-2];
    patroncorto[i,4]<- pib0[i]-pib0[i-1];
    patroncorto12[i,4] <- pib0[i-1]-pib0[i-2];
  }
  if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i]<pib0[i-2]){
    patron[i,5]<- pib0[i]-pib0[i-2];
    patroncorto[i,5]<- pib0[i]-pib0[i-1];
    patroncorto12[i,5] <- pib0[i-1]-pib0[i-2];
  }
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i]>pib0[i-2]){
    patron[i,6]<- pib0[i]-pib0[i-2];
    patroncorto[i,6]<- pib0[i]-pib0[i-1];
    patroncorto12[i,6] <- pib0[i-1]-pib0[i-2];
  }
  
}

#look to the classification
valores <- colSums(patron !=-30)

#to avoid 0 in denominator
for (m in 1:length(valores)){if (valores[m]==0){valores[m] <- 1}}

patt <- cbind(patron,patroncorto,patroncorto12)

#count
y <-matrix(0,500,18)  
rnames <- matrix(0,500,1)
for (i in 1:500){
  y[i,]=colSums(patt >(25-i*0.1))
  rnames[i] <- round(25-i*0.1,1)
}

row.names(y) <- rnames
colnames(y) <- c("P123","P132","P312","P231","P321","P213","corto123","corto132","corto312","corto231","corto321","corto213","12corto123","12corto132","12corto312","12corto231","12corto321","12corto213")


  #x <- pib1[1:k]
  x <- pib0
  
  
  #definicion de correlation
  x_ts <- as.zoo(x)
  n <- 1
  n1 <- 2
  x_ts_n <- lag(x_ts, k=-n, na.pad=T)
  x1_ts_n <- lag(x_ts, k=-n1, na.pad=T)
  
  phi1 <- cor(x_ts[!is.na(x_ts_n)], x_ts_n[!is.na(x_ts_n)])
  phi2 <- cor(x_ts[!is.na(x1_ts_n)], x1_ts_n[!is.na(x1_ts_n)])
  
  #to avoid a date where asin is not defined, round the number
  if(phi1>0.9 && phi2 < 0.6){
    phi1 <- 0.9 
    phi2 <- 0.6}
  
  if(phi1>0.85 && phi2 < 0.4){
    phi1 <- 0.9 
    phi2 <- 0.6}
  
  #Definition of probabilities
  Prob123[k] <- (1/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1)))
  Prob132[k] <- (1/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1)))
  Prob312[k] <- (1/4)*(1-(2/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1))))
  Prob231[k] <- (1/4)*(1-(2/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1))))
  Prob321[k] <- (1/4)*(1-(2/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1))))
  Prob213[k] <- (1/4)*(1-(2/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1))))
  
  
  
  #calculo total
  if (pib1[k]>0){
    Prob3b[k] <- ((valores[5]-y[as.character(-round(pib1[k],1)),"12corto321"])*Prob321[k] + (valores[3]-y[as.character(-round(pib1[k],1)),"P312"])*Prob312[k])/(valores[1]*Prob123[k]+valores[2]*Prob132[k]+valores[6]*Prob213[k]+valores[4]*Prob231[k]+valores[3]*Prob312[k] + valores[5]*Prob321[k])
    }
  
  if (pib1[k]<0){
    Prob3b[k] <- ((valores[1]-y[as.character(-round(pib1[k],1)),"P123"])*Prob123[k] + (valores[2]-y[as.character(-round(pib1[k],1)),"P132"])*Prob132[k] + (valores[6]-y[as.character(-round(pib1[k],1)),"12corto213"])*Prob213[k] + (valores[4]-y[as.character(-round(pib1[k],1)),"12corto231"])*Prob231[k] + (valores[3]-y[as.character(-round(pib1[k],1)),"P312"])*Prob312[k] + (valores[5]-y[as.character(-round(pib1[k],1)),"P321"])*Prob321[k])/(valores[1]*Prob123[k]+valores[2]*Prob132[k]+valores[6]*Prob213[k]+valores[4]*Prob231[k]+valores[3]*Prob312[k] + valores[5]*Prob321[k]) 
  }
  
}



#Estimation for t+3
Prob4b <- matrix(0,length(pib0),1)
Prob1234 <- matrix(0,length(pib0),1)
Prob4321 <- matrix(0,length(pib0),1)
Prob1324 <- matrix(0,length(pib0),1)
Prob4312 <- matrix(0,length(pib0),1)
Prob2431 <- matrix(0,length(pib0),1)
Prob1432 <- matrix(0,length(pib0),1)

Prob1243 <- matrix(0,length(pib0),1)
Prob3142 <- matrix(0,length(pib0),1)
Prob3412 <- matrix(0,length(pib0),1)
Prob3124 <- matrix(0,length(pib0),1)
Prob4132 <- matrix(0,length(pib0),1)
Prob2143 <- matrix(0,length(pib0),1)

Prob1423 <- matrix(0,length(pib0),1)
Prob2413 <- matrix(0,length(pib0),1)
Prob2134 <- matrix(0,length(pib0),1)
Prob1342 <- matrix(0,length(pib0),1)
Prob3241 <- matrix(0,length(pib0),1)
Prob2341 <- matrix(0,length(pib0),1)

Prob4123 <- matrix(0,length(pib0),1)
Prob4231 <- matrix(0,length(pib0),1)
Prob3421 <- matrix(0,length(pib0),1)
Prob4213 <- matrix(0,length(pib0),1)
Prob2314 <- matrix(0,length(pib0),1)
Prob3214 <- matrix(0,length(pib0),1)


for (k in 5:length(pib0)) {

  
pib1 <- pib0[1:k]


patron <- matrix(-30,length(pib0),24)
patronmedio <- matrix(-30,length(pib0),24)
patroncorto <- matrix(-30,length(pib0),24)

for (i in 4:length(pib0)) {
  
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-2]<pib0[i-3]){#1
        patron[i,1]<- pib0[i]-pib0[i-3];
    patronmedio[i,1]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,1]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-2]>pib0[i-3]){#2
       patron[i,2]<- pib0[i]-pib0[i-3];
      patronmedio[i,2]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,2]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#3
    patron[i,3]<- pib0[i]-pib0[i-3];
    patronmedio[i,3]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,3]<- pib0[i-2]-pib0[i-3];
    
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#4
    patron[i,4]<- pib0[i]-pib0[i-3];
    patronmedio[i,4]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,4]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#5
    patron[i,5]<- pib0[i]-pib0[i-3];
    patronmedio[i,5]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,5]<- pib0[i-2]-pib0[i-3];
  } 
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#6
    patron[i,6]<- pib0[i]-pib0[i-3];
    patronmedio[i,6]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,6]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#7
    patron[i,7]<- pib0[i]-pib0[i-3];
    patronmedio[i,7]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,7]<- pib0[i-2]-pib0[i-3];
  }
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]>pib0[i-3]){#8
    patron[i,8]<- pib0[i]-pib0[i-3];
    patronmedio[i,8]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,8]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#9
    patron[i,9]<- pib0[i]-pib0[i-3];
    patronmedio[i,9]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,9]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]>pib0[i-3]){#10
    patron[i,10]<- pib0[i]-pib0[i-3];
    patronmedio[i,10]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,10]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]<pib0[i-3]){#11
    patron[i,11]<- pib0[i]-pib0[i-3];
    patronmedio[i,11]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,11]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#12
    patron[i,12]<- pib0[i]-pib0[i-3];
    patronmedio[i,12]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,12]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#13
    patron[i,13]<- pib0[i]-pib0[i-3];
    patronmedio[i,13]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,13]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]<pib0[i-3]){#14
    patron[i,14]<- pib0[i]-pib0[i-3];
    patronmedio[i,14]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,14]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#15
    patron[i,15]<- pib0[i]-pib0[i-3];
    patronmedio[i,15]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,15]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#16
    patron[i,16]<- pib0[i]-pib0[i-3];
    patronmedio[i,16]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,16]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#17
    patron[i,17]<- pib0[i]-pib0[i-3];
    patronmedio[i,17]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,17]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#18
    patron[i,18]<- pib0[i]-pib0[i-3];
    patronmedio[i,18]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,18]<- pib0[i-2]-pib0[i-3];
  } 
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]<pib0[i-3]){#19
    patron[i,19]<- pib0[i]-pib0[i-3];
    patronmedio[i,19]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,19]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#20
    patron[i,20]<- pib0[i]-pib0[i-3];
    patronmedio[i,20]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,20]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#21
    patron[i,21]<- pib0[i]-pib0[i-3];
    patronmedio[i,21]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,21]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]<pib0[i-3]){#22
    patron[i,22]<- pib0[i]-pib0[i-3];
    patronmedio[i,22]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,22]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]>pib0[i-3]){#23
    patron[i,23]<- pib0[i]-pib0[i-3];
    patronmedio[i,23]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,23]<- pib0[i-2]-pib0[i-3];
  }
  
  if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]>pib0[i-3]){#24
    patron[i,24]<- pib0[i]-pib0[i-3];
    patronmedio[i,24]<- pib0[i-1]-pib0[i-3];
    patroncorto[i,24]<- pib0[i-2]-pib0[i-3];
  }
  
  
  
}



valores <- colSums(patroncorto !=-30)

#to avoid 0 in denominator
for (m in 1:length(valores)){if (valores[m]==0){valores[m] <- 1}}


patt <- cbind(patron,patronmedio,patroncorto)


y <-matrix(0,500,72)  
rnames <- matrix(0,500,1)
for (i in 1:500){
  y[i,]=colSums(patt >(25-i*0.1))
  rnames[i] <- round(25-i*0.1,1)
}

row.names(y) <- rnames
colnames(y) <- c("P1234","P4321","P1324","P4312","P2431","P1432","P1243","P3142","P3412","P3124","P4132","P2143","P1423","P2413","P2134","P1342","P3241","P2341","P4123","P4231","P3421","P4213","P2314","P3214","medio1234","medio4321","medio1324","medio4312","medio2431","medio1432","medio1243","medio3142","medio3412","medio3124","medio4132","medio2143","medio1423","medio2413","medio2134","medio1342","medio3241","medio2341","medio4123","medio4231","medio3421","medio4213","medio2314","medio3214","corto1234","corto4321","corto1324","corto4312","corto2431","corto1432","corto1243","corto3142","corto3412","corto3124","corto4132","corto2143","corto1423","corto2413","corto2134","corto1342","corto3241","corto2341","corto4123","corto4231","corto3421","corto4213","corto2314","corto3214")





  
  #x <- pib1[1:k]
  x <- pib0
  #Correlation 
  x_ts <- as.zoo(x)
  n <- 1
  n1 <- 2
  n2 <- 3
  x_ts_n <- lag(x_ts, k=-n, na.pad=T)
  x1_ts_n <- lag(x_ts, k=-n1, na.pad=T)
  x2_ts_n <- lag(x_ts, k=-n2, na.pad=T)
  
  phi1 <- cor(x_ts[!is.na(x_ts_n)], x_ts_n[!is.na(x_ts_n)])
  phi2 <- cor(x_ts[!is.na(x1_ts_n)], x1_ts_n[!is.na(x1_ts_n)])
  phi3 <- cor(x_ts[!is.na(x2_ts_n)], x2_ts_n[!is.na(x2_ts_n)])
  

  
  if(phi1>0.9 && phi2 < 0.6 && phi3 < 0.1){
    phi1 <- 0.9
    phi2 <- 0.601
    phi3 <- 0.11}
  
  if(phi1>0.85 && phi2 < 0.4){
    phi1 <- 0.85 
    phi2 <- 0.4}
  
  
  #Probabilities definition of symbols
  Prob1234[k]= Prob4321[k] <- (1/8)*(1+(2/pi)*(asin((2*phi2-phi1-phi3)/(2*(1-phi1)))+2*asin((2*phi1-phi2-1)/(2*(1-phi1)))))
  Prob3142[k]= Prob2413[k] <- (1/8)*(1+(2/pi)*(2*asin((phi2+phi3-phi1-1)/(2*sqrt((1-phi2)*(1-phi3))))+asin((phi1-phi3)/(2*(1-phi2)))))
  Prob4231[k]= Prob1324[k] <- (1/8)*(1+(2/pi)*(asin((phi1-phi3)/(2*(1-phi2)))-2*asin((1/2)*sqrt((1-phi2)/(1-phi1)))))
  Prob2143[k]= Prob3412[k] <- (1/8)*(1+(2/pi)*(2*asin((phi1+phi3-phi2-1)/(2*sqrt((1-phi1)*(1-phi3))))+asin((2*phi2-phi1-phi3)/(2*(1-phi1)))))
  Prob1243[k]= Prob2134[k]= Prob3421[k]= Prob4312[k]  <- (1/8)*(1+(2/pi)*(asin((phi1+phi2-phi3-1)/(2*sqrt((1-phi1)*(1-phi2))))-asin((2*phi2-phi1-phi3)/(2*(1-phi1)))-asin((1/2)*(sqrt((1-phi2)/(1-phi1))))))
  Prob3124[k]= Prob1342[k]= Prob4213[k]= Prob2431[k] <- (1/8)*(1+(2/pi)*(asin((phi1+phi2-phi3-1)/(2*sqrt((1-phi1)*(1-phi2))))-asin((phi1-phi3)/(2*(1-phi2)))-asin((1/2)*(sqrt((1-phi2)/(1-phi1))))))
  Prob1423[k]= Prob4132[k]= Prob3241[k]= Prob2314[k] <- (1/8)*(1+(2/pi)*(asin((phi2+phi3-phi1-1)/(2*sqrt((1-phi2)*(1-phi3))))+asin((phi1-phi2)/(sqrt((1-phi2)*(1-phi3))))-asin((1/2)*(sqrt((1-phi2)/(1-phi1))))))
  Prob1432[k]= Prob4123[k]= Prob2341[k]= Prob3214[k] <- (1/8)*(1+(2/pi)*(asin((phi1+phi3-phi2-1)/(2*sqrt((1-phi1)*(1-phi3))))-asin((phi1-phi2)/(sqrt((1-phi2)*(1-phi3))))+asin((2*phi1-phi2-1)/(2*(1-phi1)))))
  
  
  #Estimation of probabilities
  if (pib1[k]>0){
    Prob4b[k] <- ((valores[1]-y[as.character(-round(pib1[k],1)),"medio1234"])*Prob1234[k] + (valores[3]-y[as.character(-round(pib1[k],1)),"medio1324"])*Prob1324[k] + (valores[15]-y[as.character(-round(pib1[k],1)),"medio2134"])*Prob2134[k] +  (valores[16]-y[as.character(-round(pib1[k],1)),"medio1342"])*Prob1342[k] + (valores[6]-y[as.character(-round(pib1[k],1)),"P1432"])*Prob1432[k] + (valores[7]-y[as.character(-round(pib1[k],1)),"P1243"])*Prob1243[k] + (valores[12]-y[as.character(-round(pib1[k],1)),"P2143"])*Prob2143[k] + (valores[13]-y[as.character(-round(pib1[k],1)),"P1423"])*Prob1423[k])/(valores[1]*Prob1234[k] + valores[2]*Prob4321[k]+ valores[3]*Prob1324[k] +valores[4]*Prob4312[k] +valores[5]*Prob2431[k]  + valores[6]*Prob1432[k] + valores[7]*Prob1243[k] + valores[8]*Prob3142[k] + valores[9]*Prob3412[k] + valores[10]*Prob3124[k] + valores[11]*Prob4132[k] + valores[12]*Prob2143[k] + valores[13]*Prob1423[k] + valores[14]*Prob2413[k] + valores[15]*Prob2134[k] + valores[16]*Prob1342[k] + valores[17]*Prob3241[k] + valores[18]*Prob2341[k] + valores[19]*Prob4123[k] + valores[20]*Prob4231[k] + valores[21]*Prob3421[k] + valores[22]*Prob4213[k] + valores[23]*Prob2314[k] + valores[24]*Prob3214[k]) 
    
    }
  
  if (pib1[k]<0){
    Prob4b[k] <- ((valores[1]-y[as.character(-round(pib1[k],1)),"P1234"])*Prob1234[k] + (valores[2]-y[as.character(-round(pib1[k],1)),"P4321"])*Prob4321[k] + (valores[3]-y[as.character(-round(pib1[k],1)),"P1324"])*Prob1324[k] + (valores[4]-y[as.character(-round(pib1[k],1)),"P4312"])*Prob4312[k] + (valores[5]-y[as.character(-round(pib1[k],1)),"P2431"])*Prob2431[k] + (valores[6]-y[as.character(-round(pib1[k],1)),"P1432"])*Prob1432[k] + (valores[7]-y[as.character(-round(pib1[k],1)),"P1243"])*Prob1243[k] + (valores[8]-y[as.character(-round(pib1[k],1)),"medio3142"])*Prob3142[k] + (valores[9]-y[as.character(-round(pib1[k],1)),"medio3412"])*Prob3412[k] + (valores[10]-y[as.character(-round(pib1[k],1)),"medio3124"])*Prob3124[k] + (valores[11]-y[as.character(-round(pib1[k],1)),"P4132"])*Prob4132[k] + (valores[12]-y[as.character(-round(pib1[k],1)),"P2143"])*Prob2143[k] + (valores[13]-y[as.character(-round(pib1[k],1)),"P1423"])*Prob1423[k] + (valores[14]-y[as.character(-round(pib1[k],1)),"P2413"])*Prob2413[k] + (valores[15]-y[as.character(-round(pib1[k],1)),"P2134"])*Prob2134[k] + (valores[16]-y[as.character(-round(pib1[k],1)),"P1342"])*Prob1342[k] + (valores[17]-y[as.character(-round(pib1[k],1)),"medio3241"])*Prob3241[k] + (valores[18]-y[as.character(-round(pib1[k],1)),"medio2341"])*Prob2341[k] + (valores[19]-y[as.character(-round(pib1[k],1)),"P4123"])*Prob4123[k] + (valores[20]-y[as.character(-round(pib1[k],1)),"P4231"])*Prob4231[k] + (valores[21]-y[as.character(-round(pib1[k],1)),"medio3421"])*Prob3421[k] + (valores[22]-y[as.character(-round(pib1[k],1)),"P4213"])*Prob4213[k] + (valores[23]-y[as.character(-round(pib1[k],1)),"medio2314"])*Prob2314[k] + (valores[24]-y[as.character(-round(pib1[k],1)),"medio3214"])*Prob3214[k]) / (valores[1]*Prob1234[k] + valores[2]*Prob4321[k]+ valores[3]*Prob1324[k] +valores[4]*Prob4312[k] +valores[5]*Prob2431[k]  + valores[6]*Prob1432[k] + valores[7]*Prob1243[k] + valores[8]*Prob3142[k] + valores[9]*Prob3412[k] + valores[10]*Prob3124[k] + valores[11]*Prob4132[k] + valores[12]*Prob2143[k] + valores[13]*Prob1423[k] + valores[14]*Prob2413[k] + valores[15]*Prob2134[k] + valores[16]*Prob1342[k] + valores[17]*Prob3241[k] + valores[18]*Prob2341[k] + valores[19]*Prob4123[k] + valores[20]*Prob4231[k] + valores[21]*Prob3421[k] + valores[22]*Prob4213[k] + valores[23]*Prob2314[k] + valores[24]*Prob3214[k])
    
    }
}


#Comparators generation


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


probt1 <- Prob2b
probt2 <- Prob3b
probt3 <- Prob4b



#We plot the case for example of prob at t+2 vs NBER
Pr<-ts(probt2[3:length(probt2)],start=c(1961,2),frequency=4)#55,4 usa
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


#write.xlsx(data.frame(probt2[3:length(probt2)]),"Probabilities_US_NP.xlsx")

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


table <- data.frame(Prob2b[3:(T)],EcriRoc[3:(T)],(Prob2b[3:(T)]-EcriRoc[3:(T)])^{2})
colnames(table) <- c("probt151","EcriRoc51", "bla")

#table2 <- data.frame(Prob3b[51:T],xROC2[51:T],(Prob3b[51:T]-xROC2[51:T])^{2})
table2 <- data.frame(Prob3b[3:(T)],EcriRoc2[3:(T)],(Prob3b[3:(T)]-EcriRoc2[3:(T)])^{2})
colnames(table2) <- c("probt251","EcriRoc251", "bla")

#table3 <- data.frame(Prob4b[51:T],xROC3[51:T],(Prob4b[51:T]-xROC3[51:T])^{2})
table3 <- data.frame(Prob4b[3:(T)],EcriRoc3[3:(T)],(Prob4b[3:(T)]-EcriRoc3[3:(T)])^{2})
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






















###################################################
#Same procedure for the whole sample
pib <- read.xlsx('dataUS_21.xlsx')
pib0 <- pib[,3];
fechas <- pib[,1];
T <- length(pib0)


#Estimation for t+1
Prob2b <- matrix(0,length(pib0),1)
Prob12 <- matrix(0.5,length(pib0),1)
Prob21 <- matrix(0.5,length(pib0),1)


for (k in 8:length(pib0)) {
  
  pib1 <- pib0[1:k]
  
  
  
  #count of patterns
  patron <- matrix(-20,length(pib0),2)
  
  
  for (i in 2:length(pib0)) {
    if (pib0[i]>pib0[i-1]){
      patron[i,1]<- pib0[i]-pib0[i-1];
    }
    if (pib0[i]<pib0[i-1]){
      patron[i,2]<- pib0[i]-pib0[i-1];
    }
    
  }
  
  
  #total for each pattern
  valores <- colSums(patron !=-20)
  
  
  
  
  
  y <-matrix(0,500,2)  
  rnames <- matrix(0,500,1)
  for (i in 1:500){
    y[i,]=colSums(patron >(25-i*0.1))
    rnames[i] <- round(25-i*0.1,1)
  }
  
  row.names(y) <- rnames
  colnames(y) <- c("P12","P21")
  
  
  
  
  #x <- pib1[1:k]
  
  #Prob estimation
  
  
  if (pib1[k]>0){
    Prob2b[k] <-  0
  }
  
  if (pib1[k]<0){
    Prob2b[k] <- ((valores[1]-y[as.character(-round(pib1[k],1)),"P12"])*Prob12[k] + (valores[2]-y[as.character(-round(pib1[k],1)),"P21"])*Prob21[k])/(valores[1]*Prob12[k]+valores[2]*Prob21[k])
  }
  
  
  
}


#Estimation for t+2
Prob3b <- matrix(0,length(pib0),1)
Prob123 <- matrix(0,length(pib0),1)
Prob132 <- matrix(0,length(pib0),1)
Prob312 <- matrix(0,length(pib0),1)
Prob231 <- matrix(0,length(pib0),1)
Prob321 <- matrix(0,length(pib0),1)
Prob213 <- matrix(0,length(pib0),1)

for (k in 8:length(pib0)) {
  
  #pib1 <- pib0[1:k]
  
  
  patron <- matrix(-30,length(pib0),6)
  patroncorto <- matrix(-30,length(pib0),6)
  patroncorto12 <- matrix(-30,length(pib0),6)
  
  #counting
  for (i in 3:length(pib0)) {
    if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i]>pib0[i-2]){
      patron[i,1]<- pib0[i]-pib0[i-2];
      patroncorto[i,1] <- pib0[i]-pib0[i-1];
      patroncorto12[i,1] <- pib0[i-1]-pib0[i-2];
    }
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i]>pib0[i-2]){
      patron[i,2]<- pib0[i]-pib0[i-2];
      patroncorto[i,2] <- pib0[i]-pib0[i-1];
      patroncorto12[i,2] <- pib0[i-1]-pib0[i-2];
    }
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i]<pib0[i-2]){
      patron[i,3]<- pib0[i]-pib0[i-2];
      patroncorto[i,3] <- pib0[i]-pib0[i-1];
      patroncorto12[i,3] <- pib0[i-1]-pib0[i-2];
    }
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i]<pib0[i-2]){
      patron[i,4]<- pib0[i]-pib0[i-2];
      patroncorto[i,4]<- pib0[i]-pib0[i-1];
      patroncorto12[i,4] <- pib0[i-1]-pib0[i-2];
    }
    if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i]<pib0[i-2]){
      patron[i,5]<- pib0[i]-pib0[i-2];
      patroncorto[i,5]<- pib0[i]-pib0[i-1];
      patroncorto12[i,5] <- pib0[i-1]-pib0[i-2];
    }
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i]>pib0[i-2]){
      patron[i,6]<- pib0[i]-pib0[i-2];
      patroncorto[i,6]<- pib0[i]-pib0[i-1];
      patroncorto12[i,6] <- pib0[i-1]-pib0[i-2];
    }
    
  }
  
  #look to the classification
  valores <- colSums(patron !=-30)
  
  #to avoid 0 in denominator
  for (m in 1:length(valores)){if (valores[m]==0){valores[m] <- 1}}
  
  patt <- cbind(patron,patroncorto,patroncorto12)
  
  #count
  y <-matrix(0,500,18)  
  rnames <- matrix(0,500,1)
  for (i in 1:500){
    y[i,]=colSums(patt >(25-i*0.1))
    rnames[i] <- round(25-i*0.1,1)
  }
  
  row.names(y) <- rnames
  colnames(y) <- c("P123","P132","P312","P231","P321","P213","corto123","corto132","corto312","corto231","corto321","corto213","12corto123","12corto132","12corto312","12corto231","12corto321","12corto213")
  
  
  x <- pib1[1:k]
  
  
  
  #definicion de correlacion
  x_ts <- as.zoo(x)
  n <- 1
  n1 <- 2
  x_ts_n <- lag(x_ts, k=-n, na.pad=T)
  x1_ts_n <- lag(x_ts, k=-n1, na.pad=T)
  
  phi1 <- cor(x_ts[!is.na(x_ts_n)], x_ts_n[!is.na(x_ts_n)])
  phi2 <- cor(x_ts[!is.na(x1_ts_n)], x1_ts_n[!is.na(x1_ts_n)])
  
  #to avoid certain cases where asin is not defined, round the number
  if(phi1>0.9 && phi2 < 0.6){
    phi1 <- 0.9 
    phi2 <- 0.6}
  
  if(phi1>0.85 && phi2 < 0.4){
    phi1 <- 0.9 
    phi2 <- 0.6}
  
  #Definition of probabilities
  Prob123[k] <- (1/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1)))
  Prob132[k] <- (1/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1)))
  Prob312[k] <- (1/4)*(1-(2/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1))))
  Prob231[k] <- (1/4)*(1-(2/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1))))
  Prob321[k] <- (1/4)*(1-(2/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1))))
  Prob213[k] <- (1/4)*(1-(2/pi)*asin((1/2)*sqrt((1-phi2)/(1-phi1))))
  
  
  
  #calculo total
  if (pib1[k]>0){
    Prob3b[k] <- ((valores[5]-y[as.character(-round(pib1[k],1)),"12corto321"])*Prob321[k] + (valores[3]-y[as.character(-round(pib1[k],1)),"P312"])*Prob312[k])/(valores[1]*Prob123[k]+valores[2]*Prob132[k]+valores[6]*Prob213[k]+valores[4]*Prob231[k]+valores[3]*Prob312[k] + valores[5]*Prob321[k])
  }
  
  if (pib1[k]<0){
    Prob3b[k] <- ((valores[1]-y[as.character(-round(pib1[k],1)),"P123"])*Prob123[k] + (valores[2]-y[as.character(-round(pib1[k],1)),"P132"])*Prob132[k] + (valores[6]-y[as.character(-round(pib1[k],1)),"12corto213"])*Prob213[k] + (valores[4]-y[as.character(-round(pib1[k],1)),"12corto231"])*Prob231[k] + (valores[3]-y[as.character(-round(pib1[k],1)),"P312"])*Prob312[k] + (valores[5]-y[as.character(-round(pib1[k],1)),"P321"])*Prob321[k])/(valores[1]*Prob123[k]+valores[2]*Prob132[k]+valores[6]*Prob213[k]+valores[4]*Prob231[k]+valores[3]*Prob312[k] + valores[5]*Prob321[k]) 
  }
  
}



#Estimation for t+3
Prob4b <- matrix(0,length(pib0),1)
Prob1234 <- matrix(0,length(pib0),1)
Prob4321 <- matrix(0,length(pib0),1)
Prob1324 <- matrix(0,length(pib0),1)
Prob4312 <- matrix(0,length(pib0),1)
Prob2431 <- matrix(0,length(pib0),1)
Prob1432 <- matrix(0,length(pib0),1)

Prob1243 <- matrix(0,length(pib0),1)
Prob3142 <- matrix(0,length(pib0),1)
Prob3412 <- matrix(0,length(pib0),1)
Prob3124 <- matrix(0,length(pib0),1)
Prob4132 <- matrix(0,length(pib0),1)
Prob2143 <- matrix(0,length(pib0),1)

Prob1423 <- matrix(0,length(pib0),1)
Prob2413 <- matrix(0,length(pib0),1)
Prob2134 <- matrix(0,length(pib0),1)
Prob1342 <- matrix(0,length(pib0),1)
Prob3241 <- matrix(0,length(pib0),1)
Prob2341 <- matrix(0,length(pib0),1)

Prob4123 <- matrix(0,length(pib0),1)
Prob4231 <- matrix(0,length(pib0),1)
Prob3421 <- matrix(0,length(pib0),1)
Prob4213 <- matrix(0,length(pib0),1)
Prob2314 <- matrix(0,length(pib0),1)
Prob3214 <- matrix(0,length(pib0),1)



for (k in 4:length(pib0)) {
  
  
  pib1 <- pib0[1:k]
  
  
  patron <- matrix(-30,length(pib0),24)
  patronmedio <- matrix(-30,length(pib0),24)
  patroncorto <- matrix(-30,length(pib0),24)
  
  for (i in 4:length(pib0)) {
    
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-2]<pib0[i-3]){#1
      patron[i,1]<- pib0[i]-pib0[i-3];
      patronmedio[i,1]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,1]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-2]>pib0[i-3]){#2
      patron[i,2]<- pib0[i]-pib0[i-3];
      patronmedio[i,2]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,2]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#3
      patron[i,3]<- pib0[i]-pib0[i-3];
      patronmedio[i,3]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,3]<- pib0[i-2]-pib0[i-3];
      
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#4
      patron[i,4]<- pib0[i]-pib0[i-3];
      patronmedio[i,4]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,4]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#5
      patron[i,5]<- pib0[i]-pib0[i-3];
      patronmedio[i,5]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,5]<- pib0[i-2]-pib0[i-3];
    } 
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#6
      patron[i,6]<- pib0[i]-pib0[i-3];
      patronmedio[i,6]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,6]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#7
      patron[i,7]<- pib0[i]-pib0[i-3];
      patronmedio[i,7]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,7]<- pib0[i-2]-pib0[i-3];
    }
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]>pib0[i-3]){#8
      patron[i,8]<- pib0[i]-pib0[i-3];
      patronmedio[i,8]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,8]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#9
      patron[i,9]<- pib0[i]-pib0[i-3];
      patronmedio[i,9]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,9]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]>pib0[i-3]){#10
      patron[i,10]<- pib0[i]-pib0[i-3];
      patronmedio[i,10]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,10]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]<pib0[i-3]){#11
      patron[i,11]<- pib0[i]-pib0[i-3];
      patronmedio[i,11]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,11]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#12
      patron[i,12]<- pib0[i]-pib0[i-3];
      patronmedio[i,12]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,12]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#13
      patron[i,13]<- pib0[i]-pib0[i-3];
      patronmedio[i,13]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,13]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]<pib0[i-3]){#14
      patron[i,14]<- pib0[i]-pib0[i-3];
      patronmedio[i,14]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,14]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#15
      patron[i,15]<- pib0[i]-pib0[i-3];
      patronmedio[i,15]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,15]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]<pib0[i-3]){#16
      patron[i,16]<- pib0[i]-pib0[i-3];
      patronmedio[i,16]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,16]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#17
      patron[i,17]<- pib0[i]-pib0[i-3];
      patronmedio[i,17]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,17]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#18
      patron[i,18]<- pib0[i]-pib0[i-3];
      patronmedio[i,18]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,18]<- pib0[i-2]-pib0[i-3];
    } 
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]<pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]<pib0[i-3]){#19
      patron[i,19]<- pib0[i]-pib0[i-3];
      patronmedio[i,19]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,19]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#20
      patron[i,20]<- pib0[i]-pib0[i-3];
      patronmedio[i,20]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,20]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]>pib0[i-3]){#21
      patron[i,21]<- pib0[i]-pib0[i-3];
      patronmedio[i,21]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,21]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]>pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]>pib0[i-2] && pib0[i]>pib0[i-3] && pib0[i-1]<pib0[i-3]){#22
      patron[i,22]<- pib0[i]-pib0[i-3];
      patronmedio[i,22]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,22]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]<pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]>pib0[i-3]){#23
      patron[i,23]<- pib0[i]-pib0[i-3];
      patronmedio[i,23]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,23]<- pib0[i-2]-pib0[i-3];
    }
    
    if (pib0[i]<pib0[i-1] && pib0[i-1]>pib0[i-2] && pib0[i-2]>pib0[i-3] && pib0[i]<pib0[i-2] && pib0[i]<pib0[i-3] && pib0[i-1]>pib0[i-3]){#24
      patron[i,24]<- pib0[i]-pib0[i-3];
      patronmedio[i,24]<- pib0[i-1]-pib0[i-3];
      patroncorto[i,24]<- pib0[i-2]-pib0[i-3];
    }
    
    
    
  }
  
  
  
  valores <- colSums(patroncorto !=-30)
  
  #to avoid 0 in denominator
  for (m in 1:length(valores)){if (valores[m]==0){valores[m] <- 1}}
  
  
  patt <- cbind(patron,patronmedio,patroncorto)
  
  
  y <-matrix(0,500,72)  
  rnames <- matrix(0,500,1)
  for (i in 1:500){
    y[i,]=colSums(patt >(25-i*0.1))
    rnames[i] <- round(25-i*0.1,1)
  }
  
  row.names(y) <- rnames
  colnames(y) <- c("P1234","P4321","P1324","P4312","P2431","P1432","P1243","P3142","P3412","P3124","P4132","P2143","P1423","P2413","P2134","P1342","P3241","P2341","P4123","P4231","P3421","P4213","P2314","P3214","medio1234","medio4321","medio1324","medio4312","medio2431","medio1432","medio1243","medio3142","medio3412","medio3124","medio4132","medio2143","medio1423","medio2413","medio2134","medio1342","medio3241","medio2341","medio4123","medio4231","medio3421","medio4213","medio2314","medio3214","corto1234","corto4321","corto1324","corto4312","corto2431","corto1432","corto1243","corto3142","corto3412","corto3124","corto4132","corto2143","corto1423","corto2413","corto2134","corto1342","corto3241","corto2341","corto4123","corto4231","corto3421","corto4213","corto2314","corto3214")
  
  
  
  
  
  
  x <- pib1[1:k]
  
  #Correlation 
  x_ts <- as.zoo(x)
  n <- 1
  n1 <- 2
  n2 <- 3
  x_ts_n <- lag(x_ts, k=-n, na.pad=T)
  x1_ts_n <- lag(x_ts, k=-n1, na.pad=T)
  x2_ts_n <- lag(x_ts, k=-n2, na.pad=T)
  
  phi1 <- cor(x_ts[!is.na(x_ts_n)], x_ts_n[!is.na(x_ts_n)])
  phi2 <- cor(x_ts[!is.na(x1_ts_n)], x1_ts_n[!is.na(x1_ts_n)])
  phi3 <- cor(x_ts[!is.na(x2_ts_n)], x2_ts_n[!is.na(x2_ts_n)])
  
  
  
  if(phi1>0.9 && phi2 < 0.6 && phi3 < 0.1){
    phi1 <- 0.9
    phi2 <- 0.601
    phi3 <- 0.11}
  
  if(phi1>0.85 && phi2 < 0.4){
    phi1 <- 0.85 
    phi2 <- 0.4}
  
  
  #Probabilities definition of symbols
  Prob1234[k]= Prob4321[k] <- (1/8)*(1+(2/pi)*(asin((2*phi2-phi1-phi3)/(2*(1-phi1)))+2*asin((2*phi1-phi2-1)/(2*(1-phi1)))))
  Prob3142[k]= Prob2413[k] <- (1/8)*(1+(2/pi)*(2*asin((phi2+phi3-phi1-1)/(2*sqrt((1-phi2)*(1-phi3))))+asin((phi1-phi3)/(2*(1-phi2)))))
  Prob4231[k]= Prob1324[k] <- (1/8)*(1+(2/pi)*(asin((phi1-phi3)/(2*(1-phi2)))-2*asin((1/2)*sqrt((1-phi2)/(1-phi1)))))
  Prob2143[k]= Prob3412[k] <- (1/8)*(1+(2/pi)*(2*asin((phi1+phi3-phi2-1)/(2*sqrt((1-phi1)*(1-phi3))))+asin((2*phi2-phi1-phi3)/(2*(1-phi1)))))
  Prob1243[k]= Prob2134[k]= Prob3421[k]= Prob4312[k]  <- (1/8)*(1+(2/pi)*(asin((phi1+phi2-phi3-1)/(2*sqrt((1-phi1)*(1-phi2))))-asin((2*phi2-phi1-phi3)/(2*(1-phi1)))-asin((1/2)*(sqrt((1-phi2)/(1-phi1))))))
  Prob3124[k]= Prob1342[k]= Prob4213[k]= Prob2431[k] <- (1/8)*(1+(2/pi)*(asin((phi1+phi2-phi3-1)/(2*sqrt((1-phi1)*(1-phi2))))-asin((phi1-phi3)/(2*(1-phi2)))-asin((1/2)*(sqrt((1-phi2)/(1-phi1))))))
  Prob1423[k]= Prob4132[k]= Prob3241[k]= Prob2314[k] <- (1/8)*(1+(2/pi)*(asin((phi2+phi3-phi1-1)/(2*sqrt((1-phi2)*(1-phi3))))+asin((phi1-phi2)/(sqrt((1-phi2)*(1-phi3))))-asin((1/2)*(sqrt((1-phi2)/(1-phi1))))))
  Prob1432[k]= Prob4123[k]= Prob2341[k]= Prob3214[k] <- (1/8)*(1+(2/pi)*(asin((phi1+phi3-phi2-1)/(2*sqrt((1-phi1)*(1-phi3))))-asin((phi1-phi2)/(sqrt((1-phi2)*(1-phi3))))+asin((2*phi1-phi2-1)/(2*(1-phi1)))))
  
  
  #Estimation of probabilities
  if (pib1[k]>0){
    Prob4b[k] <- ((valores[1]-y[as.character(-round(pib1[k],1)),"medio1234"])*Prob1234[k] + (valores[3]-y[as.character(-round(pib1[k],1)),"medio1324"])*Prob1324[k] + (valores[15]-y[as.character(-round(pib1[k],1)),"medio2134"])*Prob2134[k] +  (valores[16]-y[as.character(-round(pib1[k],1)),"medio1342"])*Prob1342[k] + (valores[6]-y[as.character(-round(pib1[k],1)),"P1432"])*Prob1432[k] + (valores[7]-y[as.character(-round(pib1[k],1)),"P1243"])*Prob1243[k] + (valores[12]-y[as.character(-round(pib1[k],1)),"P2143"])*Prob2143[k] + (valores[13]-y[as.character(-round(pib1[k],1)),"P1423"])*Prob1423[k])/(valores[1]*Prob1234[k] + valores[2]*Prob4321[k]+ valores[3]*Prob1324[k] +valores[4]*Prob4312[k] +valores[5]*Prob2431[k]  + valores[6]*Prob1432[k] + valores[7]*Prob1243[k] + valores[8]*Prob3142[k] + valores[9]*Prob3412[k] + valores[10]*Prob3124[k] + valores[11]*Prob4132[k] + valores[12]*Prob2143[k] + valores[13]*Prob1423[k] + valores[14]*Prob2413[k] + valores[15]*Prob2134[k] + valores[16]*Prob1342[k] + valores[17]*Prob3241[k] + valores[18]*Prob2341[k] + valores[19]*Prob4123[k] + valores[20]*Prob4231[k] + valores[21]*Prob3421[k] + valores[22]*Prob4213[k] + valores[23]*Prob2314[k] + valores[24]*Prob3214[k]) 
    
  }
  
  if (pib1[k]<0){
    Prob4b[k] <- ((valores[1]-y[as.character(-round(pib1[k],1)),"P1234"])*Prob1234[k] + (valores[2]-y[as.character(-round(pib1[k],1)),"P4321"])*Prob4321[k] + (valores[3]-y[as.character(-round(pib1[k],1)),"P1324"])*Prob1324[k] + (valores[4]-y[as.character(-round(pib1[k],1)),"P4312"])*Prob4312[k] + (valores[5]-y[as.character(-round(pib1[k],1)),"P2431"])*Prob2431[k] + (valores[6]-y[as.character(-round(pib1[k],1)),"P1432"])*Prob1432[k] + (valores[7]-y[as.character(-round(pib1[k],1)),"P1243"])*Prob1243[k] + (valores[8]-y[as.character(-round(pib1[k],1)),"medio3142"])*Prob3142[k] + (valores[9]-y[as.character(-round(pib1[k],1)),"medio3412"])*Prob3412[k] + (valores[10]-y[as.character(-round(pib1[k],1)),"medio3124"])*Prob3124[k] + (valores[11]-y[as.character(-round(pib1[k],1)),"P4132"])*Prob4132[k] + (valores[12]-y[as.character(-round(pib1[k],1)),"P2143"])*Prob2143[k] + (valores[13]-y[as.character(-round(pib1[k],1)),"P1423"])*Prob1423[k] + (valores[14]-y[as.character(-round(pib1[k],1)),"P2413"])*Prob2413[k] + (valores[15]-y[as.character(-round(pib1[k],1)),"P2134"])*Prob2134[k] + (valores[16]-y[as.character(-round(pib1[k],1)),"P1342"])*Prob1342[k] + (valores[17]-y[as.character(-round(pib1[k],1)),"medio3241"])*Prob3241[k] + (valores[18]-y[as.character(-round(pib1[k],1)),"medio2341"])*Prob2341[k] + (valores[19]-y[as.character(-round(pib1[k],1)),"P4123"])*Prob4123[k] + (valores[20]-y[as.character(-round(pib1[k],1)),"P4231"])*Prob4231[k] + (valores[21]-y[as.character(-round(pib1[k],1)),"medio3421"])*Prob3421[k] + (valores[22]-y[as.character(-round(pib1[k],1)),"P4213"])*Prob4213[k] + (valores[23]-y[as.character(-round(pib1[k],1)),"medio2314"])*Prob2314[k] + (valores[24]-y[as.character(-round(pib1[k],1)),"medio3214"])*Prob3214[k]) / (valores[1]*Prob1234[k] + valores[2]*Prob4321[k]+ valores[3]*Prob1324[k] +valores[4]*Prob4312[k] +valores[5]*Prob2431[k]  + valores[6]*Prob1432[k] + valores[7]*Prob1243[k] + valores[8]*Prob3142[k] + valores[9]*Prob3412[k] + valores[10]*Prob3124[k] + valores[11]*Prob4132[k] + valores[12]*Prob2143[k] + valores[13]*Prob1423[k] + valores[14]*Prob2413[k] + valores[15]*Prob2134[k] + valores[16]*Prob1342[k] + valores[17]*Prob3241[k] + valores[18]*Prob2341[k] + valores[19]*Prob4123[k] + valores[20]*Prob4231[k] + valores[21]*Prob3421[k] + valores[22]*Prob4213[k] + valores[23]*Prob2314[k] + valores[24]*Prob3214[k])
    
  }
}


#Comparators generation
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


probt1 <- Prob2b
probt2 <- Prob3b
probt3 <- Prob4b



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




