import os
import xlrd
import numpy as np
import pandas as pd

dataloc = '/media/souvik/ZCYC'
os.chdir(dataloc)
filenames = os.listdir(dataloc)


# Defining function for making array of maturity and yield from excel
def get_array(ws):
    t = list([ws.cell_value(i, 0) for i in range(8, 69)])
    y = list([ws.cell_value(i, 1) for i in range(8, 69)])
    return (t, y)

error_files = []
beta0, beta1, beta2, beta3, tau1, tau2 = [], [], [], [], [], []
y0 = []
y1 = []
y2 = []
y3 = []
y4 = []
y5 = []
y6 = []
y7 = []
y8 = []
y9 = []
y10 = []
y11 = []
y12 = []
y13 = []
y14 = []
y15 = []
y16 = []
y17 = []
y18 = []
y19 = []
y20 = []
y21 = []
y22 = []
y23 = []
y24 = []
y25 = []
y26 = []
y27 = []
y28 = []
y29 = []
y30 = []
y31 = []
y32 = []
y33 = []
y34 = []
y35 = []
y36 = []
y37 = []
y38 = []
y39 = []
y40 = []
y41 = []
y42 = []
y43 = []
y44 = []
y45 = []
y46 = []
y47 = []
y48 = []
y49 = []
y50 = []
y51 = []
y52 = []
y53 = []
y54 = []
y55 = []
y56 = []
y57 = []
y58 = []
y59 = []
y60 = []
t0 = []
t1 = []
t2 = []
t3 = []
t4 = []
t5 = []
t6 = []
t7 = []
t8 = []
t9 = []
t10 = []
t11 = []
t12 = []
t13 = []
t14 = []
t15 = []
t16 = []
t17 = []
t18 = []
t19 = []
t20 = []
t21 = []
t22 = []
t23 = []
t24 = []
t25 = []
t26 = []
t27 = []
t28 = []
t29 = []
t30 = []
t31 = []
t32 = []
t33 = []
t34 = []
t35 = []
t36 = []
t37 = []
t38 = []
t39 = []
t40 = []
t41 = []
t42 = []
t43 = []
t44 = []
t45 = []
t46 = []
t47 = []
t48 = []
t49 = []
t50 = []
t51 = []
t52 = []
t53 = []
t54 = []
t55 = []
t56 = []
t57 = []
t58 = []
t59 = []
t60 = []
dat = []

for i in range(0, len(filenames)):
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
        t, y = get_array(worksheet)
        y0.append(y[0])
        y1.append(y[1])
        y2.append(y[2])
        y3.append(y[3])
        y4.append(y[4])
        y5.append(y[5])
        y6.append(y[6])
        y7.append(y[7])
        y8.append(y[8])
        y9.append(y[9])
        y10.append(y[10])
        y11.append(y[11])
        y12.append(y[12])
        y13.append(y[13])
        y14.append(y[14])
        y15.append(y[15])
        y16.append(y[16])
        y17.append(y[17])
        y18.append(y[18])
        y19.append(y[19])
        y20.append(y[20])
        y21.append(y[21])
        y22.append(y[22])
        y23.append(y[23])
        y24.append(y[24])
        y25.append(y[25])
        y26.append(y[26])
        y27.append(y[27])
        y28.append(y[28])
        y29.append(y[29])
        y30.append(y[30])
        y31.append(y[31])
        y32.append(y[32])
        y33.append(y[33])
        y34.append(y[34])
        y35.append(y[35])
        y36.append(y[36])
        y37.append(y[37])
        y38.append(y[38])
        y39.append(y[39])
        y40.append(y[40])
        y41.append(y[41])
        y42.append(y[42])
        y43.append(y[43])
        y44.append(y[44])
        y45.append(y[45])
        y46.append(y[46])
        y47.append(y[47])
        y48.append(y[48])
        y49.append(y[49])
        y50.append(y[50])
        y51.append(y[51])
        y52.append(y[52])
        y53.append(y[53])
        y54.append(y[54])
        y55.append(y[55])
        y56.append(y[56])
        y57.append(y[57])
        y58.append(y[58])
        y59.append(y[59])
        y60.append(y[60])
        t0.append(t[0])
        t1.append(t[1])
        t2.append(t[2])
        t3.append(t[3])
        t4.append(t[4])
        t5.append(t[5])
        t6.append(t[6])
        t7.append(t[7])
        t8.append(t[8])
        t9.append(t[9])
        t10.append(t[10])
        t11.append(t[11])
        t12.append(t[12])
        t13.append(t[13])
        t14.append(t[14])
        t15.append(t[15])
        t16.append(t[16])
        t17.append(t[17])
        t18.append(t[18])
        t19.append(t[19])
        t20.append(t[20])
        t21.append(t[21])
        t22.append(t[22])
        t23.append(t[23])
        t24.append(t[24])
        t25.append(t[25])
        t26.append(t[26])
        t27.append(t[27])
        t28.append(t[28])
        t29.append(t[29])
        t30.append(t[30])
        t31.append(t[31])
        t32.append(t[32])
        t33.append(t[33])
        t34.append(t[34])
        t35.append(t[35])
        t36.append(t[36])
        t37.append(t[37])
        t38.append(t[38])
        t39.append(t[39])
        t40.append(t[40])
        t41.append(t[41])
        t42.append(t[42])
        t43.append(t[43])
        t44.append(t[44])
        t45.append(t[45])
        t46.append(t[46])
        t47.append(t[47])
        t48.append(t[48])
        t49.append(t[49])
        t50.append(t[50])
        t51.append(t[51])
        t52.append(t[52])
        t53.append(t[53])
        t54.append(t[54])
        t55.append(t[55])
        t56.append(t[56])
        t57.append(t[57])
        t58.append(t[58])
        t59.append(t[59])
        t60.append(t[60])


    except:
        error_files.append(filenames[i])

df = pd.DataFrame({'Date': dat,
                   'CCIL_Beta 0': beta0,
                   'CCIL_Beta 1': beta1,
                   'CCIL_Beta 2': beta2,
                   'CCIL_Beta 3': beta3,
                   'CCIL_Tau 1': tau1,
                   'CCIL_Tau 2': tau2,
                    't0': t0,
                    't1': t1,
                    't2': t2,
                    't3': t3,
                    't4': t4,
                    't5': t5,
                    't6': t6,
                    't7': t7,
                    't8': t8,
                    't9': t9,
                    't10': t10,
                    't11': t11,
                    't12': t12,
                    't13': t13,
                    't14': t14,
                    't15': t15,
                    't16': t16,
                    't17': t17,
                    't18': t18,
                    't19': t19,
                    't20': t20,
                    't21': t21,
                    't22': t22,
                    't23': t23,
                    't24': t24,
                    't25': t25,
                    't26': t26,
                    't27': t27,
                    't28': t28,
                    't29': t29,
                    't30': t30,
                    't31': t31,
                    't32': t32,
                    't33': t33,
                    't34': t34,
                    't35': t35,
                    't36': t36,
                    't37': t37,
                    't38': t38,
                    't39': t39,
                    't40': t40,
                    't41': t41,
                    't42': t42,
                    't43': t43,
                    't44': t44,
                    't45': t45,
                    't46': t46,
                    't47': t47,
                    't48': t48,
                    't49': t49,
                    't50': t50,
                    't51': t51,
                    't52': t52,
                    't53': t53,
                    't54': t54,
                    't55': t55,
                    't56': t56,
                    't57': t57,
                    't58': t58,
                    't59': t59,
                    't60': t60,
                    'y0': y0,
                    'y1': y1,
                    'y2': y2,
                    'y3': y3,
                    'y4': y4,
                    'y5': y5,
                    'y6': y6,
                    'y7': y7,
                    'y8': y8,
                    'y9': y9,
                    'y10': y10,
                    'y11': y11,
                    'y12': y12,
                    'y13': y13,
                    'y14': y14,
                    'y15': y15,
                    'y16': y16,
                    'y17': y17,
                    'y18': y18,
                    'y19': y19,
                    'y20': y20,
                    'y21': y21,
                    'y22': y22,
                    'y23': y23,
                    'y24': y24,
                    'y25': y25,
                    'y26': y26,
                    'y27': y27,
                    'y28': y28,
                    'y29': y29,
                    'y30': y30,
                    'y31': y31,
                    'y32': y32,
                    'y33': y33,
                    'y34': y34,
                    'y35': y35,
                    'y36': y36,
                    'y37': y37,
                    'y38': y38,
                    'y39': y39,
                    'y40': y40,
                    'y41': y41,
                    'y42': y42,
                    'y43': y43,
                    'y44': y44,
                    'y45': y45,
                    'y46': y46,
                    'y47': y47,
                    'y48': y48,
                    'y49': y49,
                    'y50': y50,
                    'y51': y51,
                    'y52': y52,
                    'y53': y53,
                    'y54': y54,
                    'y55': y55,
                    'y56': y56,
                    'y57': y57,
                    'y58': y58,
                    'y59': y59,
                    'y60': y60})

df.to_csv("/media/souvik/Analytics/R/PRA/VAR_Project/For_R.csv", index=False)
