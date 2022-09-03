#------------------------------------------------------------------------------
#                COPULA DE DISTRIBUCIÓN LOGISTICA
#------------------------------------------------------------------------------

library(ggplot2)
library(plotly)

distLog<-function(x,y){
  
  acumulada<-(1+exp(-x)+exp(-y))^(-1)
  return(acumulada)
  
}


#creamos algoritmo de simulacion de la copula

CopLog<-function(n){ #n es el parámetro del tamaño de simulaciones, matriz resultante de nxn
  
  
#generamos un número aleatorio para U: uniforme en (0,1)
  
  v<-vector()
  v<-runif(n,0,1)
  
#simulamos otro numero aleatorio que será la probabilidad que usaremos para hallar u
  
  p<-vector()
  p<-runif(n,0,1)

#usamos la inversa generalizada (func cuantil) para despejar u de v (depende de v)
#inversa previamente calculada
  
  u<-vector()
  
  for (i in 1:n) {
    
    u[i]<-(v[i]*sqrt(p[i]))/(1+ sqrt(p[i])*(v[i]-1) )
    
  }
  
#Así ya tenemos todas las entradas de la matriz para u y v 
#creamos df de las entradas de u y v  con su respectivo valor
  
  
#creamos for para evaluar cada par 
  
   c<-vector()
  
   for (i in 1:n) {
    
    c[i]<- (u[i]*v[i])/(u[i]+v[i]-u[i]*v[i])
    
  }
  
   df<-cbind(u,v,c)
   df<-as.data.frame(df)
   
   pl <- plot_ly(df, x = ~u, y = ~v, z = ~c)
   pl <- pl %>% add_markers()
   pl <- pl %>% layout(scene = list(xaxis = list(title = 'U'),
                                      yaxis = list(title = 'V'),
                                      zaxis = list(title = 'C(u,v)')))
   
   pl
   
}
CopLog(100)

