import math
import matplotlib.pyplot as plt
import numpy as np
def f(x): return (math.cos(2*x) + x)* math.cos(2*x)-x 
def df(x): return -2*math.sin(4*x)-2*x

def newtonRaphson(x,tol=1.0e-32):
    for i in range(50):
        dx = -f(x)/df(x)
        x = x + dx
        if abs(dx) < tol: return x,i
    print ('Too many iterations\n')
root,numIter = newtonRaphson(0.514933018868)
print("--Metodo Newton Raphson--")
print ('Root =',root)
print ('Number of iterations =',numIter)




# 100 linearly spaced numbers
x = np.linspace(-3.1,3.1,100)

# the function, which is y = x^2 here
y = (np.cos(2*x) + x)*(np.cos(2*x) - x)

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