# Problema del Cohete 
f = function()
{
  t = 0
  posicion = function(t){6+(2.13 * (t^2)) - (0.0013 * (t^4))}
  funcion = 6+(2.13 * (t^2)) - (0.0013 * (t^4))
  resultado = 6+2.13*((t+1)^2)-0.0013*((t+1)^4)
  arreglos = c(0)
  
  while(funcion < resultado){
    
    t =t+1
    
    funcion = 6+2.13*(t^2)-0.0013*(t^4)
    
    resultado = 6+2.13*((t+1)^2)-0.0013*((t+1)^4)
    
  }
  plot(posicion, xlim = c(0,2), main = "Trayectoria del Cohete",col ="orange")
  
  cat("La altura mayor alcanzada es :",funcion," metros")
  
  
}
f()




