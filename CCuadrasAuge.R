#------------------------------------------------------------------------------
#                COPULA DE CUADRAS AUGE
#------------------------------------------------------------------------------

library(ggplot2)
library(plotly)

#creamos algoritmo de la cópula

CopCuadras<-function(n,a){  #n tamaño de muestra, a de alpha el parámetro de la copula alpha entre 0 y 1
 
#creamos vector uniforme para v
  
  v<-vector()
  v<-runif(n,0,1)
 
#simulamos vector de probabilidades para hallar u 
  
  p<-vector()
  p<-runif(n,0,1)
  
#usamos la inv generalizada de la acumulada condicional para u:
  
  u<-vector() #tenemos que dividir por casos o a trozos ya que así se define la derivada de la acumualda
  
  for (i in 1:n) {
    
   if(p[i] <= ( (1-a)*v[i]^(1-a)) ){
     
     u[i]=(p[i]*v[i]^a)/(1-a)
     
   }else if( p[i]>= v[i]^(1-a)  ){
     
     u[i]=p[i]^(1-a)
     
   }else{u[i]=p[i]}      #ojo a la decision cuando no esta bien definida
     
  }
  
  
  #creamos for para evaluar cada par 
  
  c<-vector()
  
  for (i in 1:n) {
    
    c[i]<- min(u[i],v[i])*max(u[i],v[i])^(1-a)
    
  }
  
  df<-cbind(u,v,c)
  df<-as.data.frame(df)
  
  pl <- plot_ly(df, x = ~u, y = ~v, z = ~c)
  pl <- pl %>% add_markers(marker=list(size=2,color = ~c, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE) )
  pl <- pl %>% layout(title="Copula de Cuadras-Auge",scene = list(xaxis = list(title = 'U'),
                                   yaxis = list(title = 'V'),
                                   zaxis = list(title = 'C(u,v)')))
  
  pl
  
}

CopCuadras(1000,.5)




