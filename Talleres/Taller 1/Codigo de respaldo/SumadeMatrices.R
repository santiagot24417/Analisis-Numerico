# Matriz

numeros = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)

matriz_triangular <- function (numeros) {
  
  total = c()
  
  for (i in numeros) {
    
    matriz_triangular = matrix (1,ncol = i, nrow = i)
    aux = 0
    
    for (j in 1:i) {
      for (k in 1:j) {
        
        aux = aux + matriz_triangular[j,k]
      }   
    }
    total = c(total , aux)
    aux = 0
  }
  print(total)
  return (total)
}

grafica = matriz_triangular (numeros)

plot (numeros , grafica , type = 'l',col = "red",main = "Matriz Triangular ")




