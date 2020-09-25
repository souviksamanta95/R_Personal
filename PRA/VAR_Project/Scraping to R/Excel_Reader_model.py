import os
import xlrd
import numpy as np
import pandas as pd
from nelson_siegel_svensson.calibrate import calibrate_nss_ols
dataloc = '/media/souvik/ZCYC'
os.chdir(dataloc)
filenames = os.listdir(dataloc)


# Defining function for making array of maturity and yield from excel
def get_array(ws):
    t = np.array([ws.cell_value(i, 0) for i in range(8, 69)])
    y = np.array([ws.cell_value(i, 1) for i in range(8, 69)])
    return(t, y)

error_files = []
beta0, beta1, beta2, beta3, tau1, tau2 = [], [], [], [], [], []
ebeta0, ebeta1, ebeta2, ebeta3, etau1, etau2 = [], [], [], [], [], []
dat = []

for i in range(0, 5):
    try:
        workbook = xlrd.open_workbook(filenames[i])
        worksheet = workbook.sheet_by_name('ZCYC comparison')
        dat.append(worksheet.cell_value(0, 1))
        beta0.append(worksheet.cell_value(1, 1))
        beta1.append(worksheet.cell_value(2, 1))
        beta2.append(worksheet.cell_value(3, 1))
        beta3.append(worksheet.cell_value(5, 1))
        tau1.append(worksheet.cell_value(4, 1))
        tau2.append(worksheet.cell_value(6, 1))

        # Estimating by NSS model
        t, y = get_array(worksheet)
        curve, status = calibrate_nss_ols(t, y)
        ebeta0.append(curve.beta0)
        ebeta1.append(curve.beta1)
        ebeta2.append(curve.beta2)
        ebeta3.append(curve.beta3)
        etau1.append(curve.tau1)
        etau2.append(curve.tau2)
    except:
        error_files.append(filenames[i])

df = pd.DataFrame({'Date': dat,
                   'CCIL_Beta 0': beta0,
                   'CCIL_Beta 1': beta1,
                   'CCIL_Beta 2': beta2,
                   'CCIL_Beta 3': beta3,
                   'CCIL_Tau 1': tau1,
                   'CCIL_Tau 2': tau2,
                   'Est_Beta 0': ebeta0,
                   'Est_Beta 1': ebeta1,
                   'Est_Beta 2': ebeta2,
                   'Est_Beta 3': ebeta3,
                   'Est_Tau 1': etau1,
                   'Est_Tau 2': etau2})

df.to_csv("/media/souvik/Analytics/R/PRA/VAR_Project/Estimated_Output.csv", index=False)
