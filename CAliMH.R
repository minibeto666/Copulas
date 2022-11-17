# simulacion por proceso poisson

#------------------------------------------------------------------------------
#                COPULA DE ALI-MIKHAIL-HAQ
#------------------------------------------------------------------------------

library(ggplot2)
library(plotly)

#creamos algoritmo de la cópula

AliMikhailHaq<-function(n,theta){ #alpha el parámetro de las variables aleatorias exponenciales auxiliares   theta estre -1 y 1
  #simulamos 1 vector de VA uniformes, de ahí buscamos u condicional y p la probabilidad condicional
  
  v<-vector()
  v<-runif(n,0,1)
  p<-runif(n,0,1)
  
  a<-vector()
  b<-vector()
  c<-vector()
  
  u<-vector()
  
  for(i in 1:n){
    
    a[i]<- theta^2 * p[i] - 2*v[i]*p[i]*theta^2 + v[i]^2 *p[i]*theta^2 - theta
    b[i]<- 2*theta*p[i]- 2*v[i]*p[i]*theta -2*p[i]*theta^2 + 4*v[i]*p[i] - 2*v[i]^2 * p[i]*theta^2 + theta -1
    c[i]<- p[i]- 2*p[i]+ 2*v[i]*p[i]*theta + p[i]*theta^2 - 2*v[i]*p[i]*theta^2 + p[i]*v[i]^2 * theta^2
    
    aux<-max(sqrt(b[i]^2 -4*a[i]*c[i]),sqrt(b[i]^2 -4*a[i]*c[i]))
    
    u[i]<-(-b[i]+sqrt(b[i]^2 -4*a[i]*c[i]))/(2*a[i])
    
  }
  
  #creamos el vector de cópula
  
  c<-vector()
  
  for (j in 1:n) {
    
    c[j]<- (u[j]*v[j])/(1-theta*(1-u[j])*(1-v[j]))
    
  }
  
  df<-cbind(u,v,c)
  df<-as.data.frame(df)
  
  g <- plot_ly(df, x = ~u, y = ~v, z = ~c)
  g <- g %>% add_markers(marker=list(size=2,color = ~c, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
  g <- g %>% layout(title="Copula Ali-Mikhail-Haq",scene = list(xaxis = list(title = 'U'),
                                                                           yaxis = list(title = 'V'),
                                                                           zaxis = list(title = 'C(u,v)')))
  
  g 
  
}

AliMikhailHaq(10000,.1) #OJO QUE NO SALE



  
  
  