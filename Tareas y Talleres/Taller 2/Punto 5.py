#Camilo Maldonado, Pablo Veintemillas, Jose Zuluaga y Sergio Peñaranda
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import lagrange


#Parte derecha mano
x=[14.6, 14.7, 14.8, 15.2, 15.6, 
    
    #Dedo meñique, anular, medio, indice y pulgar en ese orden
    
    #Meñique
    17.0, 17.6, 17.5, 17.3, 16.8, 15.4, 14.8, 
    
    #Punto medio
    14.4, 
    
    #Anular
    14.3, 15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.7, 
    
    #Punto medio
    13.1, 
    
    #Medio
    13.0, 13.3, 13.2, 13.15, 12.9, 12.4, 11.9, 11.7, 11.6, 
    
    #Punto medio
    11.3, 

    #Indice
    10.9, 10.7, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3, 
    
    #Punto medio
    10.0, 

    #Pulgar
    9.50, 9.10, 8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 
    
    #Parte izquierda de la mano
    8.10, 8.40, 9.00, 9.30, 10, 10.2, 10.3]   

    #Parte derecha de la mano
y=[14.7, 13.4, 12.3, 11.0, 10.5, 
    
    #Meñique

    8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 9.20, 
    
    #Punto medio
    9.30,
    
    #Anular
    8.70, 6.30, 5.50, 5.00, 4.70, 4.40, 4.50, 4.90, 5.40, 5.80, 6.90, 
    
    #Punto medio
    8.20, 
    
    #Medio
    7.60, 5.80, 4.50, 4.20, 3.90, 4.20, 5.70, 7.00, 7.90, 
    
    #Punto Medio
    8.20, 
    
    #Indice 
    7.30, 6.70, 5.50,  4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0, 
    
    #Punto Medio
    10.7, 
    
    #Pulgar
    11.0, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 
    
    
    
    12.5, 13.0, 13.9, 14.9, 16, 16.4, 16.8]  

plt.plot(x, y, 'x', mew=2)    

#Aplicación lagrange
lag_pol = lagrange(x[0:3],y[0:3])
xe = np.linspace(min(x[0:3]),max(x[0:3]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[2:5],y[2:5])
xe = np.linspace(min(x[2:5]),max(x[2:5]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[4:7],y[4:7])
xe = np.linspace(min(x[4:7]),max(x[4:7]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[6:9],y[6:9])
xe = np.linspace(min(x[6:9]),max(x[6:9]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[8:11],y[8:11])
xe = np.linspace(min(x[8:11]),max(x[8:11]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[10:13],y[10:13])
xe = np.linspace(min(x[10:13]),max(x[10:13]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[12:14],y[12:14])
xe = np.linspace(min(x[12:14]),max(x[12:14]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[13:16],y[13:16])
xe = np.linspace(min(x[13:16]),max(x[13:16]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[15:22],y[15:22])
xe = np.linspace(min(x[15:22]),max(x[15:22]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[21:24],y[21:24])
xe = np.linspace(min(x[21:24]),max(x[21:24]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[23:26],y[23:26])
xe = np.linspace(min(x[23:26]),max(x[23:26]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[25:27],y[25:27])
xe = np.linspace(min(x[25:27]),max(x[25:27]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[26:30],y[26:30])
xe = np.linspace(min(x[26:30]),max(x[26:30]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[29:34],y[29:34])
xe = np.linspace(min(x[29:34]),max(x[29:34]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[33:36],y[33:36])
xe = np.linspace(min(x[33:36]),max(x[33:36]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[35:37],y[35:37])
xe = np.linspace(min(x[35:37]),max(x[35:37]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[36:38],y[36:38])
xe = np.linspace(min(x[36:38]),max(x[36:38]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[37:41],y[37:41])
xe = np.linspace(min(x[37:41]),max(x[37:41]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[40:42],y[40:42])
xe = np.linspace(min(x[40:42]),max(x[40:42]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[41:43],y[41:43])
xe = np.linspace(min(x[41:43]),max(x[41:43]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[42:45],y[42:45])
xe = np.linspace(min(x[42:45]),max(x[42:45]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[44:47],y[44:47])
xe = np.linspace(min(x[44:47]),max(x[44:47]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[46:51],y[46:51])
xe = np.linspace(min(x[46:51]),max(x[46:51]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[50:54],y[50:54])
xe = np.linspace(min(x[50:54]),max(x[50:54]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[53:55],y[53:55])
xe = np.linspace(min(x[53:55]),max(x[53:55]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[54:60],y[54:60])
xe = np.linspace(min(x[54:60]),max(x[54:60]))
ye = lag_pol(xe)
plt.plot(xe, ye)

lag_pol = lagrange(x[59:65],y[59:65])
xe = np.linspace(min(x[59:65]),max(x[59:65]))
ye = lag_pol(xe)
plt.plot(xe, ye)

plt.show()

#Fin programa