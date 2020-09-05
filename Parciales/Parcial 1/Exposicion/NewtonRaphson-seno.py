# -*- coding: utf-8 -*-
"""
Created on Thu Aug 27 16:05:52 2020

@author: Julian
"""
import matplotlib.pyplot as plt
import numpy as np

def func( x ): 
    return x*np.sin(x) - 1
  

def derivFunc( x ): 
    return x*np.cos(x)+ np.sin(x) 
   
def newtonRaphson( x ): 
    h = func(x) / derivFunc(x)
    i = 0
    while abs(h) >= 1.e-15: #funciona hasta 1.e-15 
        h = func(x)/derivFunc(x) 
        x = x - h 
        i += 1
    print("El valor de la raiz es: ",x,"\n iteraciones: ",i) 

x0 = 1  #no admite números muy cercanos a 0, como 0.001
newtonRaphson(x0) 
  



# 100 linearly spaced numbers
x = np.linspace(-7,7,100)

# the function, which is y = x^2 here
y = x*np.sin(x) - 1

# setting the axes at the centre
fig = plt.figure()
ax = fig.add_subplot(1, 1, 1)
ax.spines['left'].set_position('center')
ax.spines['bottom'].set_position('zero')
ax.spines['right'].set_color('none')
ax.spines['top'].set_color('none')
ax.xaxis.set_ticks_position('bottom')
ax.yaxis.set_ticks_position('left')

# plot the function
plt.plot(x,y, 'r')

# show the plot
plt.show()
