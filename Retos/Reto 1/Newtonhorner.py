# -*- coding: utf-8 -*-
"""
Created on Wed Sep  9 17:10:59 2020

@author: Tefa0
"""

def newtonhorner (n,xo,poli):
    ite=0
    tol=1e-20  #Tolerancia
    e=100
    nr=n        #Controla el bucle for
    listaaux1=poli
    listaaux2=poli
    j=1         #Controla posiciones de lista auxiliar
    r=n         #R controla el numero de elementos de la lista auxiliar
    
    for k in range (nr):
        listaaux1 = listaaux2  #listaaux1 sea p(x) y listaaux2 sea q(x)
        listaaux2 = [1]*r
        listaaux1[0] = poli[0]
        listaaux2[0] = poli[0]
        while e>tol:           #Bucle que hace newtonhorner, controla el error
            y=listaaux1[0]
            z=listaaux2[0]
            i=1              #En 1 porque el elemento coe[0] ya fue determinado
            while i<r:         #Bucle de metodo de horner
                    y=(xo*y)+listaaux1[i]    #Valor de p(xo)
                    listaaux2[j]=y
                    j+=1
                    z=(xo*z)+y               #Valor de q(x0)
                    i+=1
            y=(xo*y)+listaaux1[-1]     #mas el ultimo termino
            xnuevo=xo-(y/z) 
            e=abs((xnuevo-xo))
            xo=xnuevo
            ite+=1
            j=1                        #reinicio
           
        r-=1      #Eliminamos un termino de la lista auxiliar 2
        print("La raiz ", k+1,"es", xo)
        print("Las iteraciones fueron: ", ite)
        print(" ")
     
        e=100
        ite=0
n=int(input("Ingrese el grado n del polinomio: "))

polinomio=[1]*(n+1)     #Lista que guarda los coeficientes del polinomio

i=0
while i<(n+1):
    print("Ingrese el coeficiente",i+1)
    polinomio[i]=float(input())
    i+=1
    
x0=complex(input("Ingrese el valor x0 para la primera aproximacion"))

newtonhorner(n,x0,polinomio)
               
        