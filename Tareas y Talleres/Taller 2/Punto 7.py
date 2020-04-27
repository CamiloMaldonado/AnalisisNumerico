#Elaborado por Camilo Maldonado, Pablo Veintemillas, Jose Zuluaga y Sergio Peñaranda
from scipy.interpolate import lagrange
import numpy as np
import math

#e^x en el intervalo de [0,1] y error por debajo a 10^-5 toca utilizar lagrange
def func(x):
  return math.e**(x)

def p7():
  ini=1
#Intervalo [0,1]
x=np.arange(0,1,ini)
y=math.e**(x)
f=lagrange(x,y)
print(f(0.5), func(0.5))
#Condición de error menor a 10e^-5
while abs(f(0.5)-func(0.5)) > 10e-5:
  ini -=0.01
print(ini)
x = np.arange(0, 1, ini)
y = math.e ** (x)
f = lagrange(x, y)
return ini
print(p7())
#Fin del programa