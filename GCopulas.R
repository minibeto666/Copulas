#Grafica de copulas Máxima(W), Mínima (M) y producto (pi)

library(plot3D)
library(ggplot2)
library(plotly)

#Mínima

#creamos matrices con 100 puntos para el plot

M<-matrix(0,nrow = 100,ncol = 100)

for (i in 1:100) {
  
  for (j in 1:100) {
    
    M[i,j]=max( (i/100) + (j/100) -1, 0)
    
  }
  
}

#plot3d
copmin<-persp3D(z = M, theta = 80 ,phi=15,curtain = FALSE,bty = "g")

#base
mincop<-persp(M,main = "Cópula Mínima (M)",xlab = "U",ylab = "V",zlab = "C(u,v)",
      col = "cyan",shade = .6,theta =35,box = T ,ticktype = "detailed" )

#plotly
pmin<-plot_ly(z = M, type = "surface",colors = c("blue","red"))%>%
  layout(title="Cópula Mínima (M)",scene=list(xaxis = list(title = 'U'), 
         yaxis = list(title = 'V'),zaxis=list(title="C(u,v)")) )
pmin


#Máxima

#creamos matrices con 100 puntos para el plot

W<-matrix(0,nrow = 100,ncol = 100)

for (i in 1:100) {
  
  for (j in 1:100) {
    
    W[i,j]=min((i/100),(j/100))
    
  }
  
}

#plot3d
copmax<-persp3D(z = W, theta = 80 ,phi=15,curtain = FALSE,bty = "g")

#base
maxcop<-persp(W,main = "Cópula Máxima (W)",xlab = "U",ylab = "V",zlab = "C(u,v)",
      col = "yellow",shade = .16,theta =35,box = T ,ticktype = "detailed" )

#plotly
pmax<-plot_ly(z = W, type = "surface",colors = c("blue","red"))%>%layout(title="Cópula Máxima (W)",
                                                                         scene=list(xaxis = list(title = 'U'),
                                                                                    yaxis = list(title = 'V'),
                                                                                    zaxis=list(title="C(u,v)")))
pmax


#Plot de ambas cópulas o de los límites de las cópulas


copslim<-plot_ly(z = M, type = "surface",colors = c("blue","red"))%>%layout(title="Cópula Mínima y Máxima (Límites de las Cópulas)",
                                                                            scene=list(xaxis = list(title = 'U'), 
                                                                                       yaxis = list(title = 'V'),
                                                                                       zaxis=list(title="C(u,v)")))%>%
  add_surface(W,showscale=F)
copslim


#Plot de la cópula producto



Pi<-matrix(0,nrow = 100,ncol = 100)

for (i in 1:100) {
  
  for (j in 1:100) {
    
    Pi[i,j]= (i*j)/10000
    
  }
  
}

#plot3d
copp<-persp3D(z = Pi, theta = 80 ,phi=15,curtain = FALSE,bty = "g")

#base
pcop<-persp(Pi,main = "Cópula Producto",xlab = "U",ylab = "V",zlab = "C(u,v)",
              col = "blue1",shade = .6,theta =35,box = T ,ticktype = "detailed" )

#plotly
pp<-plot_ly(z = Pi, type = "surface",colors = c("blue","red"))%>%
  layout(title="Cópula Producto",scene=list(xaxis = list(title = 'U'), 
                                              yaxis = list(title = 'V'),zaxis=list(title="C(u,v)")) )
pp
