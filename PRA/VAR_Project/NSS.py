from nelson_siegel_svensson import NelsonSiegelSvenssonCurve
import numpy as np
from matplotlib.pyplot import plot

y = NelsonSiegelSvenssonCurve(0.028, -0.03, -0.04, -0.015, 1.1, 4.0)
t = np.linspace(0, 20, 100)
plot(t, y(t))



from matplotlib import pyplot as plt
plt.show()

plt.subplot(221)

# equivalent but more general
ax1=plt.subplot(2, 2, 1)

# add a subplot with no frame
ax2=plt.subplot(222, frameon=False)

# add a polar subplot
plt.subplot(223, projection='polar')

# add a red subplot that shares the x-axis with ax1
plt.subplot(224, sharex=ax1, facecolor='red')

# delete ax2 from the figure
plt.delaxes(ax2)

# add ax2 to the figure again
plt.subplot(ax2)
plt.close()



