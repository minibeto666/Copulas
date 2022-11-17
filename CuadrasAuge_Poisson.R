# simulacion por proceso poisson

#------------------------------------------------------------------------------
#                COPULA DE CUADRAS-AUGE
#------------------------------------------------------------------------------

library(ggplot2)
library(plotly)

#creamos algoritmo de la cópula

CuadrasAugeP<-function(n,alpha){ #alpha el parámetro de las variables aleatorias exponenciales auxiliares
  # para el proceso poisson 
  #simulamos 3 vectores de VA uniformes
  
  r<-vector()
  r<-runif(n,0,1)
  
  s<-vector()
  s<-runif(n,0,1)
  
  t<-vector()
  t<-runif(n,0,1)
  
  #partimos de simular X,Y,Z para hallar u y v
  
  #meter for paara las X y Y, posteriirmente otro para u y v y finalmente evaluar cada par 
  
  x<-vector()
  y<-vector()
  z<-vector()
  
  u<-vector()
  v<-vector()
  
  for(i in 1:n){
    
    x[i]<-(-alpha*log(1-r[i]) )/(1-alpha)
    y[i]<-(-alpha*log(1-s[i]) )/(1-alpha)
    z[i]<-(-log(1-t[i]) )
    
    u[i]<- exp(-(1/alpha)*min(x[i],z[i]) )
    v[i]<- exp(-(1/alpha)*min(y[i],z[i]) )
    
  }
  
  #creamos el vector de cópula
  
  c<-vector()
  
  for (j in 1:n) {
    
    c[j]<- min(u[j],v[j])*max(u[j],v[j])^(1-alpha)
    
  }
  
  df<-cbind(u,v,c)
  df<-as.data.frame(df)
  
  g <- plot_ly(df, x = ~u, y = ~v, z = ~c)
  g <- g %>% add_markers(marker=list(size=2,color = ~c, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
  g <- g %>% layout(title="Copula Cuadras-Augé (Sim Poisson)",scene = list(xaxis = list(title = 'U'),
                                                                             yaxis = list(title = 'V'),
                                                                             zaxis = list(title = 'C(u,v)')))
  
  g
  
  
}
CuadrasAugeP(1000,.6)
