#Elaborado por Camilo Maldonado, Pablo Veintemilla, Jose Zuluaga y Sergio Peñaranda
import numpy as np
import sympy as sym

# INGRESO
x = sym.Symbol('x')
fx = sym.cos(x)
x0 = 0          
n  = 3  

k = 0 
polinomio = 0
while (k <= n):
    derivada   = fx.diff(x,k) #
    derivadax0 = derivada.subs(x,x0)
    divisor   = np.math.factorial(k)
    terminok  = (derivadax0/divisor)*(x-x0)**k
    polinomio = polinomio + terminok
    k = k + 1
print(polinomio)
