# Sumar los n al cuadrado numeros

numero = c(seq(2,100,by=2))

suma <- function (numero) {
  
  total <- c()
  for (i in numero) {
    
    aux = 0
    
    for (j in 1 : i) {
      
      aux = aux + j^2
    }
    
    total = c (total, aux)
  }
  print(total)
  return (total)
}

graficar = suma (numero)
plot (numero , graficar , type = 'l', col ="red",main ="  Sumar numeros cuadrados")




