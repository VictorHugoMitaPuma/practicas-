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

resultados<-data.frame(grupo,nota)
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



#7


#8



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
A+B
B
eigen(diag(4))
det(diag(4))