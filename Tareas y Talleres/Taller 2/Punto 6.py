#Camilo Maldonado, Pablo Veintemillas, Jose Zuluaga y Sergio Peñaranda
import matplotlib.pyplot as plt
import numpy as np
from scipy.interpolate import lagrange
#Particio de la forma xi=sigma*k
def func(x):
  return np.tan(x)

def p6():
  ini = -1.4
step = 0.8
#Aplicando lagrange
xs = np.arange(ini, ini + (step * 10), step)
f = lagrange(xs, func(xs))
while abs(func(0) - f(0)) > 10e-2:
  xs = np.arange(ini, ini + (step * 10), step)
f = lagrange(xs, func(xs))
step -=0.06
step +=0.06
xi = np.arange(ini, ini + (step * 10), 0.1)

plt.plot(xs, func(xs), 'x', xi, f(xi))
plt.show()
return step
#Imprimir sigma 
print("Sigma que minimiza el error: ", p6())
#Fin del programa