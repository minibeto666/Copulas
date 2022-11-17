#------------------------------------------------------------------------------
#                COPULA DE FRANK
#------------------------------------------------------------------------------

library(ggplot2)
library(plotly)

#creamos algoritmo de la cópula

CopFrank<-function(n,theta){  # parametro theta diferente de 0

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
    
    u[i]=-(1/theta)*log(  ( (1-p[i])*exp(-theta*v[i]) + p[i]*exp(-theta) ) / ( (1-p[i])*exp(-theta*v[i]) + p[i] ) )
    
  }
  
#Así ya tenemos todas las entradas de la matriz para u y v 
#creamos df de las entradas de u y v  con su respectivo valor
  
  
#creamos for para evaluar cada par 
  
  c<-vector()
  
  for (i in 1:n) {
    
    c[i]<- -(1/theta)* log(1 + (( (exp(-theta*u[i]) - 1)* (exp(-theta*v[i]) -1 ) )  / (exp(-theta) -1) ) )
    
  }

  df<-cbind(u,v,c)
  df<-as.data.frame(df)
  
  pl <- plot_ly(df, x = ~u, y = ~v, z = ~c)
  pl <- pl %>% add_markers(marker=list(size=2,color = ~c, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
  pl <- pl %>% layout(title="Copula de Frank",scene = list(xaxis = list(title = 'U'),
                                                            yaxis = list(title = 'V'),
                                                            zaxis = list(title = 'C(u,v)')))
  
  pl
  

}

CopFrank(5000,10)

