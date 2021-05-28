#Tesis

#CondBag

# *********************Simulaci?n de datos*******************************
options(install.packages.compile.from.source = "always")
install.packages(c("MASS", "party","tidyverse","openxlsx","doParallel","foreach"), type = "both")

library(MASS)
library(party)
library(openxlsx)
library(doParallel)
library(foreach)

n.cores <- 4
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "PSOCK"
)
print(my.cluster)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()
foreach::getDoParWorkers()


n<-5000 #datos
mu_y<-0 #media error y
sd_y<-1 #desviaci?n estandar y

mu_x<-0 #media error x
sd_x<-0.4 #desviaci?n estandar x


#Normal bivariante para x9 y x10
mu_x9<-10
mu_x10<-7
sd<-1
cor<-0.9
p<-2 #variables

#Creando el vector de medias
U <- matrix(c(mu_x9 ,mu_x10),nrow = p,  ncol = 1)

#Creando la matriz de varianzas y covarianzas
V <- matrix(c(sd,cor,cor,sd),nrow = p,  ncol = p)



#Generamos los datos x9 y x10
datax<- mvrnorm(n,U, V)
x9<-datax[,1]
x10<-datax[,2]

#Modelo generador de datos

x1 = 0 + 0.1*x9 + 0.1*x10 + 0.08*x9*x10 + rnorm(n,mu_x,sd_x)
x2 = 0 + 0.001*x1 + 0.001*x9 + 0.001*x10 + 0.05*x1*x9 + 0.05*x9*x10 + 0.05*x1*x10 + 0.05*x1*x9*x10 + rnorm(n,mu_x,sd_x)
x3 = 0 + 0.001*x1 + 0.001*x2 + 0.001*x9 + 0.001*x10 + 0.05*x1*x2 + 0.05*x1*x9 + 0.05*x1*x10+ 0.05*x2*x9 + 0.05*x9*x10 +rnorm(n,mu_x,sd_x)
x4 = 0 + 0.001*x1 + 0.001*x2 + 0.001*x3 + 0.001*x9 + 0.001*x10 + 0.05*x1*x2 + 0.05*x1*x3+ 0.05*x1*x9 + 0.05*x1*x10 + 0.05*x2*x3 + 0.05*x9*x10 + rnorm(n,mu_x,sd_x)
x5 = 0 + 0.001*x1 + 0.001*x2 + 0.001*x3 + 0.001*x4 + 0.001*x9 + 0.001*x10 + 0.005*x1*x2+ 0.005*x1*x3 + 0.005*x1*x4 + 0.005*x1*x9 + 0.005*x1*x10 + 0.005*x3*x4+ 0.005*x9*x10 + rnorm(n,mu_x,sd_x)
x6 = 0 + 0.001*x1 + 0.001*x2 + 0.001*x3 + 0.001*x4 + 0.001*x5 + 0.005*x1*x2 + 0.005*x1*x3+ 0.005*x1*x5 + 0.005*x9*x10 + 0.005*x4*x9 + 0.005*x4*x10 + 0.005*x3*x5 + rnorm(n,mu_x,sd_x)
x7 = 0 + 0.001*x1 + 0.001*x2 + 0.001*x3 + 0.001*x4 + 0.001*x5 + 0.001*x6 + 0.001*x9+ 0.001*x10 + 0.005*x1*x2 + 0.005*x2*x3 + 0.005*x1*x6 + 0.005*x1*x9+ 0.005*x6*x9 + 0.005*x9*x10 + rnorm(n,mu_x,sd_x)
x8 = 0 + 0.001*x1 + 0.001*x2 + 0.001*x3 + 0.001*x4 + 0.001*x5 + 0.001*x6 + 0.001*x7+ 0.001*x9 + 0.001*x10 + 0.005*x4*x7 + 0.005*x1*x4 + 0.005*x1*x7 + 0.005*x2*x5+ 0.005*x3*x6 + 0.005*x9*x10 + rnorm(n,mu_x,sd_x)

yi = 0 + 0.5*x1 + 0.5*x2 + 0.5*x3 + 0.5*x8 + 0.5*x9 + 0.5*(x3)^2 + 1*x1*x2 + 1*x8*x9 + rnorm(n,mu_y,sd_y)


#**********************Simulaci?n*************************************
# generamos aleatoriamente los datos de training y prueba

ptraining<- 0.8
ptest<-0.2
datos<- data.frame(yi,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) #datos simulados
rm(list=ls(pattern="x"))

pred=make.predictorMatrix(datos)#matriz de predictores
pred[,"yi"]=0#removemos yi como predictor
rm(yi)

#Agregamos los datos faltantes
#datos faltantes 10%, 20%, 30%, 40%

start.time <- Sys.time()
mse=foreach(n_i=c(0.1,0.2,0.3,0.4),.combine="cbind")%:%  #inicializamos con el porcentajo de datos faltantes
  
  foreach (r= 1:100,.packages = c("party"))%dopar%{
    training_sample<-sample(1:nrow(datos),ptraining*nrow(datos))
    
    training=datos[training_sample,] #variable training con los datos de entrenamiento
    test=datos[-training_sample,]    #variable test con los datos de prueba
    
    
    for (tr in 2:(ncol(training)-2)){ 
      nas=sample(nrow(training),n_i*nrow(training))
      training[nas,tr]=NA  #a?adimos los NA al conjunto de entrenamiento
    }
    
    for (te in 2:(ncol(test)-2)){
      nas=sample(nrow(test),n_i*nrow(test))
      test[nas,te]=NA #a?adimos los NA al conjunto de prueba
    }
    
    
    crforest=cforest(yi~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10,data=training,controls = cforest_unbiased(ntree = 500, mtry = (ncol(datos)-1), maxsurrogate = min(3, ncol(test)-1)))
    crforest.res=predict(crforest,newdata=test)
    mse_cor =mean((crforest.res-test$yi)^2)
    return(mse_cor)
  } #media cuadr?tica del error



mse=apply(mse,2, FUN=as.numeric)
parallel::stopCluster(cl = my.cluster)

#Guardar datos en excel
wb <- createWorkbook()
addWorksheet(wb, "Enfoque Correcto")

writeData(wb, "Enfoque Correcto", mse, startRow = 1, startCol = 1)

saveWorkbook(wb, file = "CondBagg-Surrogates.xlsx", overwrite = TRUE)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken