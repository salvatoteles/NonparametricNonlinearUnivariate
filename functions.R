#functions


auc_manual<-function(real,estimacion){
  
  #1)Primero calculo el AUROC
  pROC_obj_1 <- roc(real,estimacion)
  curvaroc_1 <- t(as.data.frame(coords(pROC_obj_1, seq(0, 1, 0.0001),transpose=TRUE)))
  #ya sea o con paquete pracma 
  #AUC = abs(trapz(1-curvaroc_1[,2],curvaroc_1[,3]))
  
  #o a mano (equivalente)
  
  x <- 1-curvaroc_1[,2]
  y <- curvaroc_1[,3]
  id <- order(x)
  
  AUC <- sum(diff(x[id])*rollmean(y[id],2))
  
  #2)Calculo el TNR
  FPR_mano <- 1-curvaroc_1[,2]
  
  dist_mano <- sqrt((0-FPR_mano)^2 + (1-curvaroc_1[,3])^2)
  which.min(dist_mano)[[1]]
  opt_mano <- curvaroc_1[,1][which.min(dist_mano)[[1]]]
  
  TNR_mano <- curvaroc_1[which.min(dist_mano)[[1]],2]
  TPR_mano <- curvaroc_1[which.min(dist_mano)[[1]],3]
  
  #3)Por ?ltimo, Calculo Kappa 
  #primero creo la matriz de confusi?n
  
  predictions <- factor(ifelse(estimacion>opt_mano, "Rec","Exp"))
  Realidad <- factor(ifelse(real>opt_mano, "Rec","Exp"))
  
  matrizconf <- confusionMatrix(predictions,Realidad)
  
  #y ahora el c?lculo
  #Pra <- 
  #PrA <- 
  #PrB <-#
  #Pre <- 
  #Kappa <- (Pra-Pre)/(1-Pre)
  Kappa <- matrizconf$overall[2]
  
  
  return(list(AUC,TNR_mano,Kappa,FPR_mano,opt_mano,matrizconf,TPR_mano))
}






auc_manual_v2<-function(real,estimacion,thres){
  
  #1)Primero calculo el AUROC
  #pROC_obj_1 <- roc(real,estimacion)
  #curvaroc_1 <- t(as.data.frame(coords(pROC_obj_1, seq(0, 1, 0.0001),transpose=TRUE)))
  
  for(w in 1:length(thresholds)){
    
    TP <- 0
    FN <- 0
    TN <- 0
    FP <- 0
    for( ww in 1:length(real)){
      if(real[ww] == 0 && estimacion[ww]<=thresholds[w]){
        TN <- TN + 1
      }
      if(real[ww] == 0 && estimacion[ww]>thresholds[w]){
        FP <- FP + 1
      }
      if(real[ww] == 1 && estimacion[ww]>thresholds[w]){
        TP <- TP + 1
      }
      if(real[ww] == 1 && estimacion[ww]<=thresholds[w]){
        FN <- FN + 1
      }
      
    }
    sensi[w] <- TP/(TP + FN)
    speci[w] <- TN/(TN + FP)
    
  }
  
  sensi[1] <- 1
  speci[1] <- 0
  sensi[length(thresholds)] <- 0
  speci[length(thresholds)] <- 1
  
  mediaTPR <- mean(sensi)
  mediaTNR <- mean(speci)
  mediaFPR <- mean(1-speci)
  mediaFNR <- mean(1-sensi)
  
  curvaroc_2 <- data.frame(thresholds,speci,sensi)
  #a mano (equivalente)
  
  x <- 1-curvaroc_2[,2]
  y <- curvaroc_2[,3]
  id <- order(x)
  
  AUC_2 <- sum(diff(x[id])*rollmean(y[id],2))
  
  
  #2)Calculo el TNR
  FPR_mano_2 <- 1-curvaroc_2[,2]
  
  dist_mano_2 <- sqrt((0-FPR_mano_2)^2 + (1-curvaroc_2[,3])^2)
  which.min(dist_mano_2)[[1]]
  opt_mano_2 <- curvaroc_2[,1][which.min(dist_mano_2)[[1]]]
  #opt_mano_05_pos <- which(curvaroc_2[,1]==0.50)
  opt_mano_05_pos <- which(curvaroc_2[,1]==thres)
  opt_mano_05 <- curvaroc_2[opt_mano_05_pos,1]
  
  TNR_mano_2 <- curvaroc_2[which.min(dist_mano_2)[[1]],2]
  TNR_mano_05 <- curvaroc_2[opt_mano_05_pos,2]
  TPR_mano_2 <- curvaroc_2[which.min(dist_mano_2)[[1]],3]
  TPR_mano_05 <- curvaroc_2[opt_mano_05_pos,3]
  
  #3)Por ?ltimo, Calculo Kappa 
  #primero creo la matriz de confusi?n
  
  #predictions_mano_v2 <- factor(ifelse(estimacion>opt_mano_2, "Rec","Exp"))
  predictions_mano_05 <- factor(ifelse(estimacion>=opt_mano_05, "Rec","Exp"))
  #Realidad_mano_v2 <- factor(ifelse(real>opt_mano_2, "Rec","Exp"))
  Realidad_mano_05 <- factor(ifelse(real>opt_mano_05, "Rec","Exp"))
  
  #matrizconf_mano_v2 <- confusionMatrix(predictions_mano_v2,Realidad_mano_v2)
  matrizconf_mano_05 <- confusionMatrix(predictions_mano_05,Realidad_mano_05)
  
  
  #y ahora el c?lculo
  #Pra <- 
  #PrA <- 
  #PrB <-#
  #Pre <- 
  #Kappa <- (Pra-Pre)/(1-Pre)
  #Kappa_mano_v2 <- matrizconf_mano_v2$overall[2]
  Kappa_mano_05 <- matrizconf_mano_05$overall[2]
  
  
  
  
  for(w in 1:(length(thresholds)-1)){
    
    predi <- factor(ifelse(estimacion>=thresholds[w], "Rec","Exp"))
    #Realidad_mano_v2 <- factor(ifelse(real>opt_mano_2, "Rec","Exp"))
    Reali <- factor(ifelse(real>thresholds[w], "Rec","Exp"))
    matrizconf <- confusionMatrix(predi,Reali)
    
    Kappa[w] <- matrizconf$overall[2]
    
  }
  
  mediaKappa <- mean(Kappa)
  
  
  
  #return(list(AUC_2,TNR_mano_2,Kappa_mano_v2,TPR_mano_2,opt_mano_2))
  return(list(AUC_2,TNR_mano_05,Kappa_mano_05,TPR_mano_05,opt_mano_2,mediaTPR,mediaTNR,mediaFPR,mediaFNR,mediaKappa))
}








TNR_roc<-function(real_1,estimacion_1){
  #Optimum threshold
  #pROC_obj <- roc(EcriRoc251,round(probt251,6))
  pROC_obj <- roc(real_1,estimacion_1)
  FPR <- 1- pROC_obj$specificities
  #plot(FPR,pROC_obj$sensitivities,type="l")
  dist <- sqrt((0-FPR)^2 + (1-pROC_obj$sensitivities)^2)
  
  which.min(dist)
  
  opt <- pROC_obj$thresholds[which.min(dist)]
  
  
  #Calculo para ese opt el TNR
  
  TNR_r <- pROC_obj$specificities[which.min(dist)]
  TPR_r <- pROC_obj$sensitivities[which.min(dist)]
  
  #3)Por ?ltimo, Calculo Kappa 
  #primero creo la matriz de confusi?n
  
  #predictions_r <- factor(ifelse(estimacion_1>opt, "Rec","Exp"))
  #Realidad_r <- factor(ifelse(real_1>opt, "Rec","Exp"))
  
  #matrizconf_r <- confusionMatrix(predictions_r,Realidad_r)
  
  #y ahora el c?lculo
  #Pra <- 
  #PrA <- 
  #PrB <-#
  #Pre <- 
  #Kappa <- (Pra-Pre)/(1-Pre)
  #Kappa_r <- matrizconf_r$overall[2]
  
  
  #return(list(TNR_r,opt,FPR,Kappa_r,TPR_r))
  return(list(TNR_r,opt,FPR,TPR_r))
}
