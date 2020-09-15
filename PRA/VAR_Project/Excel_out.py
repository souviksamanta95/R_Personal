import os
cwd = os.getcwd()
cwd = '/media/souvik/Analytics/R'
dataloc = cwd + '/PRA/VAR_Project/ZCYC'

os.chdir(dataloc)
filenames = os.listdir(dataloc)


from nelson_siegel_svensson import NelsonSiegelSvenssonCurve
import numpy as np
from matplotlib.pyplot import plot

y = NelsonSiegelSvenssonCurve(0.028, -0.03, -0.04, -0.015, 1.1, 4.0)
t = np.linspace(0, 20, 100)
plot(t, y(t))
























