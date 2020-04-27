from scipy.interpolate import lagrange
import numpy as np
import matplotlib.pyplot as plt 

Tk  =[ 100, 200, 300, 400, 450,  500,  600]
Bmol=[-160, -35,-4.2, 9.0, 13.5  , 16.9, 21.3
]

p=lagrange(Tk,Bmol)
x1=np.linspace(min(Tk),max(Tk),10)
y1=p(x1)

plt.plot(x1,y1,label='interpolación')
plt.plot(Tk,Bmol,'x', mew=2, label='Datos importados')

plt.xlabel("X")
plt.ylabel("Y")
plt.legend()
print(p(450))

#error para el dato B(t = 450)
plt.plot(450, p(450), 'r.')

plt.show()