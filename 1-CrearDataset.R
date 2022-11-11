library(readxl)
datos = read_excel('Estrategias_TerceraCompetencia.xlsx',sheet='TrainStrategy')

vacias = which(is.na(datos$Equipo))
equipos = datos$Equipo[-vacias]
undersamp = datos$`Under sampling`[-vacias]
undersamp[which(is.na(undersamp))] = 1 

train = datos[which(datos$fold=='Train'),6:34]
meses = c(seq(201901,201912),seq(202001,202012),seq(202101,202105))
colnames(train)= paste0('tr_',meses)
train[is.na(train)] = 0



validate = datos[datos$fold == 'Validate',6:34]
validate = validate[-1,]
resp = c()
for(i in 1:length(equipos)){
  a = which(validate[i,]==1)
  resp = c(resp,ifelse(length(a)!=0,a,NA))  
}

validate = meses[resp]

test = datos[datos$fold == 'Test',6:34]
test = test[-1,]
resp = c()
for(i in 1:length(equipos)){
  a = which(test[i,]==1)
  resp = c(resp,ifelse(length(a)!=0,a,NA))  
}

test = meses[resp]

#Final Train
ftrain = datos[which(datos$fold=='Final Train'),6:34]
colnames(ftrain)= paste0('ftr_',meses)
ftrain[is.na(ftrain)] = 0

datos = read_excel('Estrategias_TerceraCompetencia.xlsx',sheet='Preparation')

#catastrofe
catastrofe = datos[-1,4:6]
rescatastrofe = c()
opc_cat = c("Ninguno","Interpol","DataScience")
for(i in 1:length(equipos)){
  aux = opc_cat[which(catastrofe[i,]=="1")]
  if(length(aux)==0) aux = NA
  rescatastrofe = c(rescatastrofe,aux)
}
#datadrifting
datadrifting = datos[-1,11:14]
resdatadrifting = c()
opc_dd = c("Ninguno","Rank0f","Rank","deflacion")
for(i in 1:length(equipos)){
  aux = opc_dd[which(datadrifting[i,]=="1")]
  if(length(aux)==0) aux = NA
  resdatadrifting = c(resdatadrifting,aux)
}

FE_Catedra = as.numeric(datos$...8[-1])
FE_Catedra[which(is.na(FE_Catedra))] = 0

FE_Propio = as.numeric(datos$...9[-1])
FE_Propio[which(is.na(FE_Propio))] = 0

lag1 = as.numeric(unlist(datos[-1,15]))
lag1[which(is.na(lag1))] = 0

lag2 = as.numeric(unlist(datos[-1,16]))
lag2[which(is.na(lag2))] = 0

lag3 = as.numeric(unlist(datos[-1,17]))
lag3[which(is.na(lag3))] = 0

lag6 = as.numeric(unlist(datos[-1,18]))
lag6[which(is.na(lag6))] = 0

tend = as.numeric(unlist(datos[-1,19]))
tend[which(is.na(tend))] = 0

vent_tend = as.numeric(unlist(datos[-1,20]))
vent_tend[which(is.na(vent_tend))] = 0

max = as.numeric(unlist(datos[-1,21]))
max[which(is.na(max))] = 0

min = as.numeric(unlist(datos[-1,22]))
min[which(is.na(min))] = 0

avg = as.numeric(unlist(datos[-1,23]))
avg[which(is.na(avg))] = 0

rf = as.numeric(unlist(datos[-1,24]))
rf[which(is.na(rf))] = 0

ca = as.numeric(unlist(datos[-1,25]))
ca[which(is.na(ca))] = 0

ncols = as.numeric(unlist(datos[-1,26]))
ncols[which(is.na(ncols))] = 0

datos = read_excel('Estrategias_TerceraCompetencia.xlsx',sheet='HyperparamTuning')

nrows = as.numeric(unlist(datos[-1,5]))

sup = as.numeric(unlist(datos[-1,6]))
sup[which(sup==0)] = NA


dataset = data.frame("Private" = as.numeric(unlist(datos[-1,3])),
                     "Catastrofe" = rescatastrofe,
                     "FE_Catedra" = FE_Catedra,
                     "FE_Propio" = FE_Propio,
                     "DataDrifting" = resdatadrifting,
                     "lag1" =lag1,
                     "lag2" =lag2,
                     "lag3" =lag3,
                     "lag6" =lag6,
                     "tend" =tend,
                     "vent_tend" =vent_tend,
                     "max" =max,
                     "min" =min,
                     "avg" =avg,
                     "rf" =rf,
                     "ca" =ca,
                     "ncols"= ncols,
                     "nrows"= nrows,
                     "sup" = sup,
                     "validate" = validate,
                     "test" = test)

dataset = cbind(cbind(dataset, train),ftrain)                    
rownames(dataset) = equipos

write.csv(dataset,"datasetcomp3.csv")