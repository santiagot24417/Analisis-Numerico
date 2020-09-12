##################################################
####  TOLERANCIA 1e-8  ######
start_time <- Sys.time()
poli1 <- c(1.0, -5.0, -9.0, 155.0, -250.0)
laguerre(poli1, 3.3,25,.Machine$double.eps^(1/2))   
end_time <- Sys.time()
 
cat("Tiempo de ejecucion")
print(end_time - start_time)
cat("tolerancia")
print(.Machine$double.eps^(1/2))

start_time <- Sys.time()
poli2 <- c(1.0, -5.0, -9.0, 155.0, -250.0)
laguerre(poli2, -6.2,25,.Machine$double.eps^(1/2)) 
end_time <- Sys.time()


cat("Tiempo de ejecucion")
print(end_time - start_time)
cat("tolerancia")
print(.Machine$double.eps^(1/2))

#################################################
####  TOLERANCIA 1e-16  ######
start_time <- Sys.time()
poli1 <- c(1.0, -5.0, -9.0, 155.0, -250.0)
laguerre(poli1, 3.3,25,1e-16)
end_time <- Sys.time()

cat("Tiempo de ejecucion")
print(end_time - start_time)
cat("tolerancia")
print(.Machine$double.eps^(1/2))

poli2 <- c(1.0, -5.0, -9.0, 155.0, -250.0)
laguerre(poli2, -6.2,25,1e-16)

################################################
####  TOLERANCIA 1e-32  ######
start_time <- Sys.time()
poli1 <- c(1.0, -5.0, -9.0, 155.0, -250.0)
laguerre(poli1, 3.3,25,1e-32)
end_time <- Sys.time()

cat("Tiempo de ejecucion")
print(end_time - start_time)
cat("tolerancia")
print(.Machine$double.eps^(1/2))

poli2 <- c(1.0, -5.0, -9.0, 155.0, -250.0)
laguerre(poli2, -6.2,25,1e-32)
