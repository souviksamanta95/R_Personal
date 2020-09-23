from nelson_siegel_svensson import NelsonSiegelSvenssonCurve
import numpy as np
from matplotlib.pyplot import plot

y = NelsonSiegelSvenssonCurve(0.028, -0.03, -0.04, -0.015, 1.1, 4.0)
t = np.linspace(0, 20, 100)
plot(t, y(t))

import numpy as np
from nelson_siegel_svensson.calibrate import calibrate_ns_ols
from nelson_siegel_svensson.calibrate import calibrate_nss_ols

t = np.array([0.0, 0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 10.0, 15.0, 20.0, 25.0, 30.0])
y = np.array([0.01, 0.011, 0.013, 0.014, 0.018, 0.020, 0.025, 0.03, 0.035, 0.037, 0.038, 0.04])

curve, status = calibrate_ns_ols(t, y, tau0=1.0)  # starting value of 1.0 for the optimization of tau
assert status.success
print(curve)

curve.beta



# NSS Model fit
curve, status = calibrate_nss_ols(t, y)











