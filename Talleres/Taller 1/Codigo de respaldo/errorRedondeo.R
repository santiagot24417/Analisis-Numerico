
binario=c(0,0,1,1,1,1,1,
          0,1,1,
          0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,1) #0.4 en punto flotante

x = 0.4

signo = binario[1]

exponente = 0
i = 0
j = 9

while(i < 8){
  exponente = exponente + binario[j]*(2^i)
  i = i + 1
  j = j - 1
    
}

entero = 0;
i = 10
k = 0
#calcular el entero

while(k < 2){
  entero = entero + binario[i]*(2^k)
  i = i - 1
  k = k + 1
}

#calcular el decimal

decimal = 0

i = 32
j = -1

while(i > 10){
  decimal = decimal + binario[i]*(2^j)
  j = j - 1
  i = i - 1
}

valor = (entero + decimal)/10

#calcular error

x = 0.4

error = abs(valor - x)/abs(x)

epsilon = 0.5*(.Machine$double.eps^(1/2))

tablita = data.frame(error,epsilon)
