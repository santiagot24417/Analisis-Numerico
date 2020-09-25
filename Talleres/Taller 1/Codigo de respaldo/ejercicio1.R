
resultado = 0;
i = 0;
x = 1.00000000001;

#polinomio P(x)

repeat{
  resultado = resultado + x^i;
  i = i + 1;
  if(i > 50)
    break;
}

#derivada del p(x)

n=51                          #Grado maximo + 1
coeficientes=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
               1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)           
coeficientesDerivada=c()     #Coeficientes derivando el polinomio
resultadoD=c()
res=0
j=n-1

i1 = 1;
while (i1<n){
  coeficientesDerivada[i1] = coeficientes[i1]*(j)
  i1=i1+1
  j=j-1
}

j = 1
while(j<n){
  res = res * x + coeficientesDerivada[j]
  resultadoD[j] = res;
  j=j+1;
}

#funcion Q

Q = function(y)((y^51) - 1)/(y - 1)

rDirecto = Q(x);


#derivada de Q
dq = function(x) 50*x^49+49*x^48+48*x^47+47*x^46+46*x^45+45*x^44+44*x^43+43*x^42+42*x^41
                +41*x^40+40*x^39+39*x^38+38*x^37 +37*x^ 36 +36*x^ 35 +35*x^ 34 +34*x^33 
                +33*x^ 32 +32*x^ 31 +31*x^ 30 +30*x^ 29 +29*x^ 28 +28*x^ 27 +27*x^ 26
                +26*x^ 25 +25*x^ 24 +24*x^ 23 +23*x^ 22 +22*x^ 21 +21*x^ 20 +20*x^ 19 
                +19*x^ 18 +18*x^ 17 +17*x^ 16 +16*x^ 15 +15*x^ 14 +14*x^ 13 +13*x^ 12 
                +12*x^ 11 +11*x^ 10+10*x^9+9*x^8+8*x^7+7*x^6+6*x^5+5*x^4+4*x^3+3*x^2+2*x
                +1

rDirectoDerivado = dq(x);

#errores
#error absoluto p(x)
errorA = abs(rDirecto - resultado);
#error absoluto p'(x)
errorDA = abs(rDirectoDerivado - res);


#error relativo p(x)
errorR=((resultado/(resultado+rDirecto))*(errorA/resultado))+(
  (rDirecto/(resultado+rDirecto))*(errorA/rDirecto));

#error relativo p'(x)
errorDR = ((res/(res+rDirectoDerivado))*(errorDA/res))+(
  (rDirecto/(res+rDirectoDerivado))*(errorDA/rDirectoDerivado));


Datos = data.frame(i,errorA,errorR,resultado,rDirecto,res,errorDA,errorDR)
print(Datos);

