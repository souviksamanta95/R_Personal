from time import sleep
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import pandas as pd

chrome_options = webdriver.ChromeOptions()
driver = webdriver.Chrome('chromedriver', chrome_options=chrome_options)
driver.maximize_window()
url = "https://www.ccilindia.com/RiskManagement/SecuritiesSegment/Pages/CCILRupeeYieldCurveDaily.aspx"

driver.get(url)


st_date = '01/01/2004'
en_date = '31/08/2009'
driver.find_element_by_xpath('//*[@id="ctl00_m_g_93916551_7907_49df_aefa_073f3c85ee79__GreaterDate"]').send_keys(st_date)
driver.find_element_by_xpath('//*[@id="ctl00_m_g_93916551_7907_49df_aefa_073f3c85ee79__LowerDate"]').send_keys(en_date)
driver.find_element_by_xpath('//*[@id="ctl00_m_g_93916551_7907_49df_aefa_073f3c85ee79__Search"]').click()

for j in range(0,1000):
    elements = driver.find_elements_by_class_name('ms-vb2')
    k = 0
    for i in range(0,10):
        elements[k].click()
        #sleep(1)
        k = k + 2
    driver.find_element_by_xpath('//*[@id="ctl00_m_g_93916551_7907_49df_aefa_073f3c85ee79"]/table[2]/tbody/tr/td[3]/a').click()
    sleep(1)

driver.close()
