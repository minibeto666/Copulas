library(copula)
library(scatterplot3d)

normalCopula(param = c(.3), dim=2)

L<-normalCopula(param = c(.3), dim=2)

marg<-mvdc(copula = L, margins = c("gamma","t"),paramMargins = list(list(shape=2,scale=1),list(df=5)) )
#buscamos marginales de X y Y con dist gamma y t-student
marg

z<-rMvdc(1000,marg)
z #valores simulados para X y Y

x<-z[,1]
y<-z[,2]

#generamos los valores de u y v

u<-pgamma(x, shape = 2,scale = 1)
v<-pt(y,df=5)

vec<-cbind(u,v)

#distribuciones de la copula

c<-rCopula(1000, L) #valores sim de la copula de u y v
dens<-dCopula(c,L)
scatterplot3d(x,y,dens,highlight.3d = TRUE,main = "Densidad de la Cópula")

distCop<-pCopula(vec,L) #distribucion

densCop<-dCopula(vec,L) #densidad

#distribucion de la copula con x y y
scatterplot3d(x,y,distCop,highlight.3d = TRUE,main = "Distribuci?n de la C?pula")

#densidad de la copula con x y y
scatterplot3d(x,y,densCop,highlight.3d = TRUE,main = "Densidad de la C?pula")


#densidad de la copula con u y v
scatterplot3d(u,v,densCop,highlight.3d = TRUE,main = "Densidad de la C?pula")

#distribucion de la copula con u y v
scatterplot3d(u,v,distCop,highlight.3d = TRUE,main = "Distribuci?n de la C?pula")
