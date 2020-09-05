from matplotlib import pyplot
def f(x): return x**3 - 2*x**2 + 4/3*x - 8/27
def df(x): return 3*x**2 - 4*x + 4/3
x = range(-10, 10)
def newtonRaphson(x,tol=1.0e-9):
    for i in range(70):
        dx = -f(x)/df(x)
        x = x + dx
        if abs(dx) < tol: return x,i
    print ('Too many iterations\n')
root,numIter = newtonRaphson(0.66655999879)
print ('Root =',root)
print ('Number of iterations =',numIter)


pyplot.plot(x, [f(i) for i in x])

#Establecemos el color de los ejes.
pyplot.axhline(0, color="black")
pyplot.axvline(0, color="black")

#Especificamos los limites de los ejes.
pyplot.xlim(-11, 11)
pyplot.ylim(-11, 11)

#Etiquetas
pyplot.title("Funcion polinomica")
pyplot.xlabel("X")                      
pyplot.ylabel("Y")  

#Guardamos el grafico en una imagen "png".
pyplot.savefig("función_Polinomica.png")

# Mostramos el gráfico.
pyplot.show()