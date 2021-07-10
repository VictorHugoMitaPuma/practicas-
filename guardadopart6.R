load("C:/Users/mitap/Downloads/ei1012-1516-la-s1-datos.RData")
#parte1

#1
#a
(0.3*0.15)/(0.3*0.15+0.2*0.8+0.5*0.12)
#b
(5^6/factorial(6))*exp(-5)
(5^6/factorial(6))+exp(-5)
#c
factorial(20)*(0.4^7)*(0.6^13)/(factorial(7)*factorial(13))

#2
#a
sum(1:1000)
#b
1+sum(1:512)*2
#3
#A
length(grupo)
#b
posicionesdeA<-c()
for(x in 1:length(grupo)){
  if("A"==grupo[x]){
    posicionesdeA<-c(posicionesdeA,x)
  }
}
posicionesdeA
#4
#a
sum(nota)
#b
sum(nota)/length(nota)
mean(nota)
#c
pos_M7<-c()
for(x in 1:length(nota)){
  if(7<nota[x]){
    pos_M7<-c(pos_M7,x)
  }
}
pos_M7

#d
p<-order(nota)
m<-c()
for(x in p){
  m<-c(nota[p])
}
rev(m)
#e
for(x in 1:length(nota)){
  if(max(nota)==nota[x]){
    print(x)
  }
}

#5 
resultados<-data.frame(grupo,nota)
#a
sum(nota[1:10])
#b
mew<-resultados[resultados$grupo=="C",]
length(mew$grupo)
#c
aprobados<-c()
for(x in 1:length(nota)){
  if(5<=nota[x]){
    aprobados<-c(aprobados,x)
  }
}
length(aprobados)

#d
new2<-resultados[resultados$grupo=="B"&resultados$nota>5,]
length(new2$grupo)
#e
new3<-resultados[resultados$grupo=="C"&resultados$nota>5,]
mew<-resultados[resultados$grupo=="C",]
length(new3$grupo)/length(mew$grupo)

#f
resultados[resultados$nota==max(nota),]

resultados[resultados$nota==min(nota),]

#g
resultados[resultados$grupo==c("A","B"),]
g_m<-resultados[resultados$grupo==c("A","B")&resultados$nota>5,]
mean(g_m$nota)

#6
p66T<-quantile(resultados$nota,0.66)
p66C<-quantile(mew$nota,0.66)

#7
mq5<-length(nota[nota<=4.9])
alum<-length(nota)
100*mq5/alum
#8
boxplot(nota ~ grupo, data = resultados)

#9
conc
#a
max(conc)
#b
M40<-c()
for(x in 1:length(conc)){
  if(40.0<conc[x]){
    M40<-c(M40,x)
  }
}
length(M40)
#c
median(conc)
#d
p<-order(conc)
m<-c()
for(x in p){
  m<-c(conc[p])
}
m[1:10]
#e
medido<-length(conc)
h<-24/medido
min<-h*60
for(x in 1:length(conc)){
  if(max(conc)==conc[x]){
    print(x)
  }
}
mindemedicion<-(min*142)%%60
horademedicion<-(min*142-mindemedicion)/60

minutos<- seq(
  as.POSIXct("2020-01-01 00:00", format("%y-%m-%d- %H:%M")),
  as.POSIXct("2020-01-01 23:59", format("%y-%m-%d- %H:%M")),
  by="5 min"
)

#parte2

#1
x<-c(1,2,3,4,5,6,7,8,9,10)
y<-c(1,4,6,8,25,36,49,61,81,100)
plot(x,y)
#2
i<-c(1,2,3,4,2,4,6,8,3,6,9,12)
A<-matrix(i,ncol = 3,nrow = 4)
#3
3*diag(3)
#4
matrizN<-function(f,c){
  P<-matrix(0,nrow = f,ncol = c)
  return(P)
}
matrizN(5,3)
#5
B<-diag(4)
B[1,1]<-0
B[2,2]<-2
B[3,3]<-3
B[4,4]<-4
B

#6
t(A)

#7
A+B #no tienen las mismas dimenciones
A-B #no tienen las mismas dimenciones
3*B
A%*%B #no se puede por que las filas 
      #y columnas son diferentes

#8
me<-function(e){
  d<-c(1,-2,1,2,4,0,3,-2,1)
  m<-matrix(d,ncol = 3)
  r<-m^e
  return(r)
}
me(6)

#9
SS3v<-function(v,r){
  m<-matrix(v,ncol = 3)
  ds<-det(m)
  for (n in 1:length(r)) {
    if(n==1){
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dx<-det(m)
    }else if(n==2){
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dy<-det(m)
    }else{
      m<-matrix(v,ncol = 3)
      m[,n]<-r
      dz<-det(m)
    }
  }
  x<-dx/ds
  y<-dy/ds
  z<-dz/ds
  r<-c(x,y,z)
  return(r)
}
v1<-c(3,9,3,-1,-2,1,1,1,-2)
rs1<-c(-1,-9,-9)
SS3v(v1,rs1)

#10
?eigen
?det

#11
C1<-seq(1,10,1)
C2<-seq(1,10,1)*2
C3<-seq(1,10,1)*3
C4<-seq(1,10,1)*4
C5<-seq(1,10,1)*5
B<-cbind(C1,C2,C3,C4,C5)
B

A<-matrix(rep(c(0,1)), ncol = 5,nrow = 5)
rep(c(0,1,0),2)
A[4:5,]<-c(0,1,1,0,0,1,0,1,1,0)
A
A%*%B # columnas de A=5 y filas de B=10 no se peude mutiplicar
      # por lo tanto no se puede saber la traspuesta
#12
x<-matrix(1,5,2)
x[2,2]<--2
x[3,2]<-0
x[5,2]<-2
x
y<-matrix(1,5,1)
y[1:2,1]<-0
y[5,1]<-3
y
inr<-t(x)%*%x
mi<-solve(inr)
cf<-mi%*%t(x)
cf%*%y

#13
data("co2")
means= aggregate(co2, FUN=mean)
year = as.vector(time(means))
co2= as.vector(means)
co2
year

diferenciasco2<-c()
for(x in 1:length(year)){
  r<- co2[x+1]-co2[x]
    diferenciasco2<-c(diferenciasco2,r)
}
diferenciasco2
plot(year,diferenciasco2, type = "b",pch=4 )

#14 no entiendo :"v