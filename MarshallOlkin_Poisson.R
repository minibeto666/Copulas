# simulacion por proceso poisson

#------------------------------------------------------------------------------
#                COPULA DE MARSHALL-OLKIN
#------------------------------------------------------------------------------

library(ggplot2)
library(plotly)

#creamos algoritmo de la cópula

MarshallOlkinP<-function(n,l1,l2,l12){ #las l1,2,12 son los parámetros de las variables aleatorias exponenciales auxiliares
                                       # para el proceso poisson 
#simulamos 3 vectores de VA uniformes

  r<-vector()
  r<-runif(n,0,1)
  
  s<-vector()
  s<-runif(n,0,1)
  
  t<-vector()
  t<-runif(n,0,1)
  
#partimos de simular X y Y para hallar u y v
  
#meter for paara las X y y, posteriirmente otro para u y v y finalmente evaluar cada par 
  
  x<-vector()
  y<-vector()
  
  u<-vector()
  v<-vector()
  
  for(i in 1:n){
  
    x[i]<-min( -log(r[i])/l1, -log(t[i])/l12 )
    y[i]<-min( -log(s[i])/l2, -log(t[i])/l12 )
  
  
    u[i]<-exp( -(l1+l12)*x[i] )
    v[i]<-exp( -(l2+l12)*y[i] )
  
  }

#creamos el vector de cópula
  
  c<-vector()
  
  alpha<-l12/(l1+l12)
  
  beta<-l12/(l2+l12)
  
  for (j in 1:n) {
  
    c[j]<- min(v[j]*u[j]^(1-alpha), u[j]*v[j]^(1-beta))
    
  }
  
  df<-cbind(u,v,c)
  df<-as.data.frame(df)
  
  g <- plot_ly(df, x = ~u, y = ~v, z = ~c)
  g <- g %>% add_markers(marker=list(size=2,color = ~c, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
  g <- g %>% layout(title="Copula Marshall-Olkin (Sim Poisson)",scene = list(xaxis = list(title = 'U'),
                                                            yaxis = list(title = 'V'),
                                                            zaxis = list(title = 'C(u,v)')))
  
  g
    
  
}
MarshallOlkinP(1000,1,1,2)
