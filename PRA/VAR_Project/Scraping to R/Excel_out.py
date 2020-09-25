import os
import xlrd
# from datetime import datetime
import pandas as pd
dataloc = '/media/souvik/ZCYC'
os.chdir(dataloc)
filenames = os.listdir(dataloc)

error_files = []
beta0 = []
beta1 = []
beta2 = []
beta3 = []
tau1 = []
tau2 = []
dat = []
day = []
mon = []
year = []
error = []

for i in range(0, len(filenames)):
    try:
        er = 0
        workbook = xlrd.open_workbook(filenames[i])
        worksheet = workbook.sheet_by_name('ZCYC comparison')
        dat.append(worksheet.cell_value(0, 1))
        dt = worksheet.cell_value(0, 1)
        y, m, d, h, n, s = xlrd.xldate_as_tuple(dt, workbook.datemode)
        day.append(d)
        mon.append(m)
        year.append(y)
        beta0.append(worksheet.cell_value(1, 1))
        beta1.append(worksheet.cell_value(2, 1))
        beta2.append(worksheet.cell_value(3, 1))
        beta3.append(worksheet.cell_value(5, 1))
        tau1.append(worksheet.cell_value(4, 1))
        tau2.append(worksheet.cell_value(6, 1))
        if worksheet.cell_value(1, 1) == 'Beta 0':
            er = er + 1
        if worksheet.cell_value(2, 1) == 'Beta 1':
            er = er + 1
        if worksheet.cell_value(3, 1) == 'Beta 2':
            er = er + 1
        if worksheet.cell_value(5, 1) == 'Beta 3':
            er = er + 1
        if worksheet.cell_value(4, 1) == 'Tau 1':
            er = er + 1
        if worksheet.cell_value(6, 1) == 'Tau 2':
            er = er + 1
        error.append(er)
    except:
        error_files.append(filenames[i])

df = pd.DataFrame({'Raw Date': dat,
                   'Day': day,
                   'Month': mon,
                   'Year': year,
                   'Beta 0': beta0,
                   'Beta 1': beta1,
                   'Beta 2': beta2,
                   'Beta 3': beta3,
                   'Tau 1': tau1,
                   'Tau 2': tau2,
                   'Ext_Error': error})

df_er = pd.DataFrame({'Error Files': error_files})

df.to_csv("/media/souvik/Analytics/R/PRA/VAR_Project/Output.csv", index=False)
df_er.to_csv("/media/souvik/Analytics/R/PRA/VAR_Project/Error.csv", index=False)
