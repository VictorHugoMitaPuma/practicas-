#1
1000%%3

#2
4560%%3==0

#3
V1<-2:87
for(x in V1){
 if(x%%7==0){
   print(x)
 }
}
#4
Z<-7:3
T<-1:50
for(x in T){
  if(x%%5==0){
    print(x)
  }
}
W<-c(5,10,15,20,25)
A<-Z%%2==0
B<-W>10
#EN LA 4TA PPOSICION

#5
sum(1:100)

#6
J<-c(1,-4,5,9,-4)
for(x in 1:length(J)){
  if(min(J)==J[x]){
    print(x)
  }
} 

#7
factorial(8)
1*2*3*4*5*6*7*8

#8
sum(exp(3:7))

#9
log10((1:10)^1/2)
#ES IGUALM A 0

#10
CORONACircular<-function(R,r){
  P<-pi*(R^2-r^2)
  return(P)
}


#11
D<-1:50
G<-50:1
G==rev(D)

#12
sum((10:100)^3)+4*sum((10:100)^2)

#13
sum((2^(1:25))/(1:25))+sum((3^(1:25))/((1:25)^2))

#14
d_f<-read.csv("https://raw.githubusercontent.com/fhernanb/datos/master/Paises.txt",sep="",dec=".")
  #a  
length(d_f)
  #b
length(d_f[,1])
  #c
d_f[d_f$poblacion==max(d_f[,2]),]
  #d
d_f[d_f$alfabetizacion==min(d_f[,3]),]
#15
d.f<-mtcars
d.f[d.f$mpg<18.0,]

d.f[d.f$cyl==4,]

d.f[d.f$wt>2.500&d.f$am==1,]

#16
x<-0:365
y<-pi*2*(x-81)/365
funcion<-9.87*sin(2*y)-7.35*cos(y)-1.5*sin(y)
plot(funcion)
