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
pmin<-plot_ly(z = M, type = "surface",colors = c("blue","red"))%>%layout(title="Cópula Mínima (M)")
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
pmax<-plot_ly(z = W, type = "surface",colors = c("blue","red"))%>%layout(title="Cópula Máxima (W)")
pmax


#Plot de ambas cópulas o de los límites de las cópulas


copslim<-plot_ly(z = M, type = "surface",colors = c("blue","red"))%>%layout(title=
                                                                              "Cópula Mínima y Máxima (Límites de las Cópulas)")%>%
  add_surface(W)
copslim
