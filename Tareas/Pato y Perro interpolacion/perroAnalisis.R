require(PolynomF)
require(graphics)
library(graphicsQC)
rm(list=ls())

x = c( 00.50 , 01.01 , 05.85 , 07.46 , 11.28 , 15.20 , 18.46 , 21.25 , 24.15 , 25.80 , 28.00 , 30.80 , 30.81 , 29.40 , 27.40 , 26.21 , 24.97 , 20.32 , 19.54 , 18.80 , 14.04 , 12.54 , 11.68 , 09.55 , 08.30 , 09.10 , 08.85 , 07.80 , 00.50)
y = c( 02.40 , 02.95 , 03.86 , 05.41 , 07.45 , 06.30 , 04.49 , 07.15 , 07.05 , 05.80 , 05.85 , 04.50 , 02.40 , 01.20 , 00.80 , 00.44 , 00.54 , 01.01 , 00.80 , 01.08 , 00.98 , 01.08 , 01.33 , 01.00 , 01.64 , 02.65 , 02.70 , 02.24 , 02.40)

x = x*0.95 

print(x)
print(y)

y1 = y[1:7]   ; x1=
x[1:7]
y2 = y[7:12]  ; x2 = x[7:12]
y3 = y[12:14] ; x3 = x[12:14]
y4 = y[14:15] ; x4 = x[14:15]
y5 = y[15:18] ; x5 = x[15:18]
y7 = y[18:20] ; x7 = x[18:20]
y8 = y[20:25] ; x8 = x[20:25]
y10 = y[25:26];x10 = x[25:26]
y11 = y[26:28];x11 = x[26:28]
y12 = y[28:29];x12 = x[28:29]
n <- length(x)
dog_c = 2

p = spline(y3,x3, n = 201)
i = p$x
p$x = p$y
p$y = i

plot(x, y, pch=20,main = paste("Interpolacion del perrito con", n-1, "puntos"),xlim=c(0,31),ylim=c(0,9))
lines(spline(x1, y1, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x2, y2, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
#lines(spline(x3, y3, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x4, y4, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x5,y5, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
#lines(spline(x6, y6, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x7, y7, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x8, y8, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x10, y10, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x11, y11, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(spline(x12, y12, n = 201), col = dog_c,xlim=c(0,31),ylim=c(0,9))
lines(p, col = dog_c)




