import matplotlib.pyplot as plt
import numpy as np

dat=np.loadtxt('trace.txt')
print dat
x= np.linspace(0,10,1000)
x,y = np.meshgrid(x,x)

plt.pcolormesh(x,y,dat)
plt.show()

