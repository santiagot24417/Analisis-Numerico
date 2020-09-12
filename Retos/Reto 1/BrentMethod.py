from matplotlib import pyplot
from scipy import optimize
import decimal
x = range(-10, 10)
def f(x):
    return (x**3 - 2*x**2+4/3*x-8/27)


root = decimal.Decimal((optimize.brentq(f, -1, 1,xtol = 1e-90)))
print("Raiz: ",root)

pyplot.plot(x, [f(i) for i in x])

#Establecemos el color de los ejes.
pyplot.axhline(0, color="pink")
pyplot.axvline(0, color="pink")

#Especificamos los limites de los ejes.
pyplot.xlim(-40, 40)
pyplot.ylim(-40, 40)

#Etiquetas
pyplot.title("Metodo de brent")
pyplot.xlabel("X")                      
pyplot.ylabel("Y")  

#Guardamos el grafico en una imagen "png".
pyplot.savefig("función_Polinomica.png")

# Mostramos el gráfico.
pyplot.show()


#############################################################