#setwd('C:/Users/herna/Desktop/DM-EyF')


require(lightgbm)

dataset = read.csv("datasetcomp3.csv")
dataset= dataset[,-1]

dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ ,-c(1)]),
                        label= dataset[ ,1],free_raw_data = FALSE )
modelo  <- lgb.train( data= dtrain,categorical_feature = c("Catastrofe","DataDrifting")
                      ,obj="regression")              



lgb.importance(modelo, percentage = TRUE)

require(rpart)
modelo  <- rpart(formula=   "Private ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dataset,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=       -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  0,     #minima cantidad de registros para que se haga el split
                 minbucket= 1,     #tamaño minimo de una hoja
                 maxdepth=  4 )

varImp <- function(object, surrogates = FALSE, competes = TRUE, ...){
  tmp <- rownames(object$splits)
  
  allVars <- colnames(attributes(object$terms)$factors)
  if(is.null(tmp))
  {
    out<-NULL
    zeros <- data.frame(x = rep(0, length(allVars)),
                        Variable = allVars)
    out <- rbind(out, zeros)
  }
  
  else {
    
    rownames(object$splits) <- 1:nrow(object$splits)
    splits <- data.frame(object$splits)
    splits$var <- tmp
    splits$type <- ""
    
    frame <- as.data.frame(object$frame)
    index <- 0
    for(i in 1:nrow(frame))
    {
      if(frame$var[i] != "<leaf>")
      {
        index <- index + 1
        splits$type[index] <- "primary"
        if(frame$ncompete[i] > 0)
        {
          for(j in 1:frame$ncompete[i])
          {
            index <- index + 1
            splits$type[index] <- "competing"
          }
        }
        if(frame$nsurrogate[i] > 0)
        {
          for(j in 1:frame$nsurrogate[i])
          {
            index <- index + 1
            splits$type[index] <- "surrogate"
          }
        }
      }
    }
    splits$var <- factor(as.character(splits$var))
    if(!surrogates) splits <- subset(splits, type != "surrogate")
    if(!competes) splits <- subset(splits, type != "competing")
    out <- aggregate(splits$improve,
                     list(Variable = splits$var),
                     sum,
                     na.rm = TRUE)
    
    allVars <- colnames(attributes(object$terms)$factors)
    if(!all(allVars %in% out$Variable))
    {
      missingVars <- allVars[!(allVars %in% out$Variable)]
      zeros <- data.frame(x = rep(0, length(missingVars)),
                          Variable = missingVars)
      out <- rbind(out, zeros)
    }
  }
  out2 <- data.frame(Overall = out$x)
  rownames(out2) <- out$Variable
  out2
}
importancia=varImp(modelo)

importancia$vars = colnames(importancia)
importancia[order(importancia$Overall),]
