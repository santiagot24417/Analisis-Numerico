#########################
#Horner reto 1.1
#########################

#Variables 
n=4                          #Grado maximo + 1
m=3
coeficientes=c(4,2,3,1)      #Coeficientes del polinomio     
coeficientesDerivada=c()     #Coeficientes derivando el polinomio
coeficientesSegundaDerivada=c()   #Coeficientes segunda derivada del polinomio
resultado=c()
resultado2=c()
res=0
j=n-1
l=m-1
x=3




#####
##derivar##
i1=1;
while (i1<n){
  coeficientesDerivada[i1]=coeficientes[i1]*(j)
  i1=i1+1
  j=j-1
}
#####
##Segundaderivada##
k1=1;
while (k1<m){
  coeficientesSegundaDerivada[k1]=coeficientesDerivada[k1]*(l)
  k1=k1+1
  l=l-1
}
##Horner##

j=1
while(j<n){
  res=res*x+coeficientesDerivada[j]
  resultado[j]= res;
  j=j+1;
}
##HornerSegundaDerivada##
res=0
l=1
while(l<m){
  res=res*x+coeficientesSegundaDerivada[l]
  resultado2[l]= res;
  l=l+1;
}


TablaHorner = data.frame(resultado) 
TablaHorner = data.frame(resultado2) 
#Fin

