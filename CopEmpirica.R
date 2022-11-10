##########################################################################################################################################
#                                                 Copula empirica
##########################################################################################################################################
library(graphics)


CopEmp<-function(x,y){

  #tomamos n como el lenght de los vectores que debe ser igual
  
  n<-length(x)
  
  #Dados los vectores de X y Y obtenemos los vectores ordenados
  
  x_ord<-sort(x)
  y_ord<-sort(y)
  
  #creamos copula empirica
  
  C<-matrix(0,nrow = n,ncol = n)
  u<-vector()
  v<-vector()
  
  #contamos pares que cumplen
   
  for (i in 1:n) {#para x

    u[i]<-i/n
    v[i]<-i/n
    
    for (j in 1:n) {#para j
      
      cuenta<-0
      
      #para calcular el valor de cada elemento de la matriz
      for (k in 1:n) {
        
        if(x[k]>x_ord[i]){
          
          cuenta<-cuenta
          
        }else if(y[k]>y_ord[j]){
          
          cuenta<-cuenta
          
        }else{cuenta<-(cuenta+1)}
        
      }#k
  
      #asignamos valor a la matriz
      
      C[i,j]<- cuenta/n
      
      
    }#j
        
  }#i
  
  #creamos df para poder graficar alv
return(C)  
  
}

x<-c(1,6,7,6,4,3,0,1,5,4,8,9) #ejemplo
y<-c(1,6,8,3,1,2,5,7,7,6,3,5) #ejemplo

CopEmp(x,y)

#grafico
############################################################################################################################
# store the current parameter settings in init
init <- par(no.readonly=TRUE)

# specify that 4 graphs to be combined and filled by columns 
par(mfcol = c(2, 2))

# specify the graphs to be combined


#1
persp(x=seq(from=1/length(x),to=1, by=1/length(x) ),y=seq(from=1/length(x),to=1, by=1/length(x) ),CopEmp(x,y), xlab = "u",ylab = "v",
      zlab = "C empirica", main = "Copula Empirica",ticktype = "detailed",col = "coral")
#2
persp(x=seq(from=1/length(x),to=1, by=1/length(x) ),y=seq(from=1/length(x),to=1, by=1/length(x) ),CopEmp(x,y), xlab = "u",ylab = "v",
      zlab = "C empirica", main = "Copula Empirica",ticktype = "detailed",col = "coral",theta = 80)
#3
persp(x=seq(from=1/length(x),to=1, by=1/length(x) ),y=seq(from=1/length(x),to=1, by=1/length(x) ),CopEmp(x,y), xlab = "u",ylab = "v",
      zlab = "C empirica", main = "Copula Empirica",ticktype = "detailed",col = "coral",phi = 10,theta = -50)
#4
contour(CopEmp(x,y),nlevels = 10,lwd = 1)#nel
#################################################################################################################################
#con color
filled.contour(CopEmp(x,y),nlevels = 10) 
