
def samesign(a, b):
        return a * b > 0

def bisect(func, low, high):
    'Find root of continuous function where f(low) and f(high) have opposite signs'

    assert not samesign(func(low), func(high))

    for i in range(54):
        midpoint = (low + high) / 2.0
        if samesign(func(low), func(midpoint)):
            low = midpoint
        else:
            high = midpoint
    print("Iteraciones: ",i)        
    return midpoint

def f(x):
        return x**3 - 2*x**2 + 4/3*x - 8/27

print("-- Metodo de biseccion --")
x = bisect(f, 0, 1)
print (x, f(x))
