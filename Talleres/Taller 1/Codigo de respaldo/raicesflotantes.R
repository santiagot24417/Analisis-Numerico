
##ecuacion modificada


raicesd = function(a,b,c){
  raiz = c()
  if (b^2 - 4*a*c >= 0){
    raizd1 = (-b - sign(b)*sqrt(b^2 - 4*a*c))/(2*a)
    raizd2 = (2*c)/(-b - sign(b)*sqrt(b^2 - 4*a*c))
  }
  else{
    raizd1 = (-b - sign(b)*sqrt(as.complex(b^2 - 4*a*c)))/(2*a)
    raizd2 = (2*c)/(-b - sign(b)*sqrt(as.complex(b^2 - 4*a*c)))
  }
  raiz[1] = raizd1
  raiz[2] = raizd2
  
  return(raiz)

}

resultados = raicesd(1, 9^12, -3)

sprintf("%.60f",resultados[1])
sprintf("%.60f",resultados[2])


