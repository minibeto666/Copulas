# ///////////////////////////////////////////////////////////////////////////////////////////
# Proyecto - Teoria de Copulas (Rendimientos)
# ///////////////////////////////////////////////////////////////////////////////////////////

library(quantmod)
library(copula)
library(VineCopula)
library(rriskDistributions)  
library(fitdistrplus)
library(MASS) 
library(actuar) 
library(univariateML)
library(VGAM)
library(lcopula)
library(VC2copula)
library(scatterplot3d)
library(cvar)

# Cargamos la serie de datos
cartera=c("KOFUBL.MX","WALMEX.MX")
getSymbols(cartera,src = "yahoo",from="2020-01-01")

#graficos x

chartSeries(WALMEX.MX,type = "candlesticks",subset = "last 1 years",theme = chartTheme("white"),TA=NULL)

chartSeries(KOFUBL.MX,type = "candlesticks",subset = "last 1 years",theme = chartTheme("white"),TA=NULL)
# Guardamos en unas variables los precios de cierre
Kof<-KOFUBL.MX[,4]
Wal<-WALMEX.MX[,4]

# Creamos una tabla donde juntamos ambos precios de cierre
tabla_precios<-cbind(Kof,Wal)


# Definimos la tabla como un dataframe
tabla_precios<-as.data.frame(tabla_precios)


# Calculamos los rendimientos  


rKOF<-dailyReturn(KOFUBL.MX,leading = FALSE) 
rWAL<-dailyReturn(WALMEX.MX,leading = FALSE) 

tablar<-as.data.frame( cbind(rKOF,rWAL) )[-1,]
colnames(tablar)<-c("rKof","rWal")

#######################
# Ultimo precio - Portafolio  
ultimo_precio<-tabla_precios[nrow(tabla_precios),]

#######################
# Revaluacion - Portafolio

tabla_reevaluacion<-data.frame()
for(i in 1:2){
  
  for( j in 1:nrow(tablar)){
    tabla_reevaluacion[j,i]<-ultimo_precio[,i]*(1+tablar[j,i])
  } 
  
}

colnames(tabla_reevaluacion)<-c("reKof","reWal")

#######################
# P&L individual
# Creamos la funcion P&L de cada emisora y las juntamos en un dataframe llamado PL_Portafolio


PL_Portafolio<-data.frame()  


for (i in 1:2){
  
  for (j in 1:nrow(tabla_reevaluacion)){
    PL_Portafolio[j,i]<-ultimo_precio[i]-tabla_reevaluacion[j,i]
    
  }
  
}  



############################################################################################
# Buscamos que distribucion de probabilidad se ajusta a los rendimientos de Kof
hist(tablar$rKof)

fit.cont(tablar$rKof)  
# Logistica

# Hallamos los parametros de la distribucion logistica  

fitdist(tablar$rKof, distr = "logis" ,method =  "mle"  ) #fitdistrplus 
m1<-model_select(tablar$rKof,models="logis",criterion="aic") #univariateML
coef(m1) 

#ligeramente diferentes

#####
# La mejor distribucion que se ajusta: Logistica
bestKof<-model_select(tablar$rKof,criterion="aic")
bestKof


# Calculamos los valores de u con la distribucion logistica
u<-plogis(tablar$rKof,location=coef(m1)[1],scale=coef(m1)[2])



############################################################################################
# Buscamos que distribucion de probabilidad se ajusta a los rendimientos de walmex
hist(tablar$rWal)
fit.cont(tablar$rWal)  
# Logistica


# Hallamos los parametros de la distribucion logistica  
m2<-model_select(tablar$rWal,models="logis",criterion="aic")
coef(m2) 

#####
bestWal<-model_select(tablar$rWal,criterion="aic")
bestWal


# Calculamos los valores de v con la distribucion logistica
v<-plogis(tablar$rWal,location=coef(m2)[1],scale=coef(m2)[2])


##############################################
# Grafico de los puntos u1,u2
plot(u,v)  


##############################################
# Determinamos la mejor copula

BiCopSelect(u,v,familyset=NA,selectioncrit = "AIC")
# Survival Gumbel, con datos de distribuciones logisticas
# par=1.15, tau=0.13


############################### Para checar dependencia

cor.test(x=tablar$rKof,
         y=tablar$rWal,method = "kendall")

valores<-cbind(tablar$rKof,tablar$rWal)
K.plot(valores)

############################### Construccion de la copula

copula<-surGumbelCopula(param = 1.15)


############################### Construccion de la funcion de distribucion conjunta
DistConj<-mvdc(copula,margins=c("logis","logis"),
               paramMargins=list(
                 list(location=coef(m1)[1],scale=coef(m1)[2]),
                 list(location=coef(m2)[1],scale=coef(m2)[2])
               )
)

############################### Graficos de mvdc


# Simulamos 1000 valores de X y Y logisticas
z <- rMvdc(n=1000,DistConj)

# Creamos el grafico de la pdf
pdf<-dMvdc(z,DistConj) 
scatterplot3d(z[,1],z[,2],pdf,highlight.3d=T)

# Creamos el grafico de la cdf
cdf<-pMvdc(z,DistConj) 
scatterplot3d(z[,1],z[,2],cdf,highlight.3d=T)


# gofCopula para pruebas de bondad de ajuste no se puede con surv gumbel

gofCopula(copula = copula,x=valores) # p-value de 0.6239


# Prueba con ua copula random
copulaprueba<-claytonCopula(param=1.5,dim=2)
gofCopula(copulaprueba,valores) # p-value de 0.001499


###########################################################################################  
# Simulacion de rendimientosy obtenci�n del VaR

VaR95<-data.frame()
tVaR95<-data.frame()  



for(i in 1:10){ # Este "for" corresponde al numero de simulaciones, tarda mucho tiempo, se puede correr una sola simulacion para ver los resultados rapidos
  
  rendimientos_sim<-data.frame()
  
  
  for(k in 1:2){# Crear bien el for
    for(j in 1:nrow(tablar)){
      rendimientos_sim[j,k]<-rMvdc(n=nrow(tablar),DistConj)[j,k]
    }
  } 
  
  
  
  #######################
  # Revaluacion
  # Una vez obtenido los rendimientos simulados, procedemos a calcular la revaluacion para cada emisora
  
  tabla_revaluacionSIM<-data.frame()
  for(k in 1:2){
    
    for( j in 1:nrow(rendimientos_sim)){
      tabla_revaluacionSIM[j,k]<-ultimo_precio[,k]*(1+rendimientos_sim[j,k])
    }
    
  }
  
  #######################
  # P&L indivual
  # Construimos la P&L de cada emisora
  PL_EmisorasSIM<-data.frame()
  
  for (k in 1:2){
    
    for(j in 1:nrow(tabla_revaluacionSIM)){
      
      PL_EmisorasSIM[j,k]<-ultimo_precio[k]-tabla_revaluacionSIM[j,k]
    }
  }
  
  
  #######################
  # Para cada emisora, calculamos el VaR y los guardamos en un dataframe, de modo que
  # tendremos 10 valores de VaR al 95% de confianza y se guardan todos en una fila
  # del dataframe VARSM95 (para los otros dos es analogo)
  
  
  #####################################################################################  
  # Todo lo que est� abajo se hzi para calcular el VaR y ES de forma manual
  # Calculamos el tVaR
  
  
  
  # Creamos el vector de probabilidades
  #    prob<-rep(1/nrow(PL_EmisorasSIM),times=nrow(PL_EmisorasSIM))
  
  # Unimos ambas tablas
  #    Tabla_Alisado<-cbind(PL_EmisorasSIM,prob)
  #    colnames(Tabla_Alisado)<-c(cartera,"probabilidades")
  #    head(Tabla_Alisado)
  
  # Creamos una tabla para cada emisora donde se ordenen los valores de su P&L
  # Las siguientes lineas solo calculan el VaR por Alisado exponencial de cada emisora
  
  # ---------------------------------------------------------------------------------
  # TSLA
  #    Tabla_Alisado1<-Tabla_Alisado[,c(1,3)]
  #    Tabla_Alisado1 <- Tabla_Alisado1[with(Tabla_Alisado1, order(-Tabla_Alisado1$TSLA)), ] 
  
  # Creamos la columna de Fx
  #    Alisado1<-Tabla_Alisado1$probabilidades
  
  #    Fx<-vector()
  
  #    for(i in 1:length(Alisado1)){
  #      Fx[i]<-sum(Alisado1[i:length(Alisado1)])
  #    }
  
  #    head(Fx,30)
  
  # Unimos la columna de Fx con la Tabla_Alisado
  #    Tabla_Alisado1<-cbind(Tabla_Alisado1,Fx)
  #    head(Tabla_Alisado1,5)
  
  #Guardamos las observaciones mayores que est�n por arriba del 95%
  #    tabla_emisora1_95<-Tabla_Alisado1[which(Tabla_Alisado1$Fx>=0.95),] # Guarda todos los valores mayores a 0.95
  
  # VaR al 95% (1 dia)
  #    VaRAE_95EMISORA1<-tabla_emisora1_95[length(tabla_emisora1_95$Fx),1] # Se toma el primer valor que sea mayor a 0.95
  #    VaRAE_95EMISORA1
  
  # tVaR AL 95% (1 dia)
  #    tVaR_95<-mean(tabla_emisora1_95$TSLA)
  
  
  #    # Resultados - VaR 1 dia de TESLA
  #    VaRAE_emisora1<-cbind(VaRAE_95EMISORA1,tVaR_95) 
  #    colnames(VaRAE_emisora1) <-c("VaR al 95%","tVaR AL 95%")  
  #   VaRAE_emisora1
  #    
  # ---------------------------------------------------------------------------------  
  
  
  for(j in 1:2){
    VaR95[i,j]<-quantile(PL_EmisorasSIM[,j],probs=0.95)
    tVaR95[i,j]<-ES(dist=PL_EmisorasSIM[,j],p_loss = 0.05)
  }
  
  
}
colnames(VaR95)<-c("KOFUBL","WALMEX")
colnames(tVaR95)<-c("KOFUBL","WALMEX")  


# VaR y tVaR
colMeans(VaR95)
colMeans(tVaR95)

#table<-rbind("VaR al 95%", "tVaR al 95%")  
#table<-cbind(colMeans(VaR95),colMeans(tVaR_95))  

###################################################################################
#Nuevo For Mas chingon 
###################################################################################

#Hallamos VaR y tVaR


VaR_df<-data.frame()
tVaR_df<-data.frame()

for(k in 1:100){ #numero de veces que se calcularan los VaR y tVaR para sacarles promedio

nsims<-1000 #numero de simulaciones sobre las cuales hace P&L

sim_rend<-data.frame()

sim_rend<- rMvdc(n=nsims,DistConj) #x y y sim con la copula

#calculamos reevaluacion
reev_sim<-data.frame()

for(j in 1:2){
  
  for( i in 1:nrow(sim_rend)){
    reev_sim[i,j]<-ultimo_precio[1,j]*(1+sim_rend[i,j])
  }#i
  
}#j

#calculamos P&L

PL_sim<-data.frame()

for (j in 1:2){
  
  for(i in 1:nrow(reev_sim)){
    
    PL_sim[i,j]<-ultimo_precio[1,j]-reev_sim[i,j]
  }#i
}#j

#añadimos PL del portafolio

PL_port<-apply(PL_sim,1,sum) #V1=Kof, V2=Wal

PL_sim<-cbind(PL_sim,PL_port)

#Calculamos VaR 95 y tVaR 95

VaR95port<-apply(PL_sim,2,quantile,probs=.95)
tVaR95port<-apply(PL_sim,2,ES,p_loss=.05) 

VaR_df[k,1]<-VaR95port[1]
VaR_df[k,2]<-VaR95port[2]
VaR_df[k,3]<-VaR95port[3]

tVaR_df[k,1]<-tVaR95port[1]
tVaR_df[k,2]<-tVaR95port[2]
tVaR_df[k,3]<-tVaR95port[3]

}#k

# VaR y tVaR
colMeans(VaR_df)
colMeans(tVaR_df) #al toque papiii
