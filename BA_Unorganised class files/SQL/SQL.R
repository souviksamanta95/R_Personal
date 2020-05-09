getwd()
setwd("C:/Users/souvi/Documents/R/BA")

library(sqldf)


# Clean environment
rm(list=ls())

#install.packages("sqldf")

#supress warnings
options(warn=-1)

#Referencing the sqld£ library
library(sqldf)

CAS <-read.csv("CAS.csv")

APAC <-read.csv("APAC.csv")


# Create a database

sqldf("CREATE DATABASE learnsql")

# Creating a table and inserting into it 

CAS <- sqldf(c("create table CAS(CM15 varchar, COUNTRY_CD char, TRANS_DT date, AMOUNT float, APPR_DENY_CD smallint, PRODUCT_TYP char, ONE_EXPSR_AGE smallint)",
              "insert into CAS values(398552711774003, 'US', '2019-01-28', 2.86, 0,'O',0),
              (305643187411008, 'US', '2019-01-02', 546.08, 0, 'O', 0),
              (305647361311008, 'US', '2019-01-23', 53.08, 0, 'M', 0),
              (305643565727866, 'US', '2019-01-13', 6.08, 0, 'O', 7),
              (305643197637472, 'US', '2019-01-21', 56.08, 0, 'O', 0),
              (305643172257864, 'US', '2019-01-22', 46.08, 0, 'M', 2),
              (305643890567214, 'US', '2019-01-01', 65.08, 0, 'O', 0),
              (303572475868455, 'US', '2019-01-15', 35.08, 0, 'O', 0),
              (305656765738356, 'US', '2019-01-18', 635.08, 0, 'O', 0)
              ","select * from CAS"))

# Return records from CAS for 1st jan 2019 for US

sqldf("select CM15,COUNTRY_CD,TRANS_DT,AMOUNT from CAS where TRANS_DT = '2019-01-01' and COUNTRY_CD = 'US' order by AMOUNT desc")

sqldf("select CM15,COUNTRY_CD,TRANS_DT,AMOUNT from CAS where TRANS_DT = '2019-01-01' and COUNTRY_CD like 'U%' order by AMOUNT desc")   # Asu, Afk all are allowed

sqldf("select CM15,COUNTRY_CD,TRANS_DT,AMOUNT from CAS where TRANS_DT = '2019-01-01' and COUNTRY_CD like 'U?' order by AMOUNT desc")   # onle one character after A is allowed

sqldf("select CM15,COUNTRY_CD,TRANS_DT,max(AMOUNT) as MAX_SPND from CAS where APPR_DENY_CD = 'o' and COUNTRY_CD = 'US' group by PRODUCT_TYP")

APAC <- sqldf(c("create table APAC(CM11, AS_OF_DT, ACCOUNT_TENURE, AGE_CD, NET_SEPNDING_AMT)",

"insert into APAC values(38287840521, '2019-01-01',37,1,1314.69)
", "Select * from APAC"))

# Dropping a table 

sqldf("drop table CAS")


# Date - 22/02/2020
# Telecom company

# Subscriber    -Cust it        Name              Address     no of connections     email id
# Connections   -Cust id        Mobile id         Plan type   current balance
# Calls         -callmobile     receivermobile    duration    date                  time

# SELECT * --means select all


# SELECT * from connections where date= 1st jan 2020

# total number of calls on 1st jan 2020 --- select count

# SELECT count of distinct caller id no from calls where date = 1st jan 2020 >> distinct callers

# Total duration of calls made from prepaid mobile numbers during jan 2020
# select sum(duration) from calls where date between '2020/01/01' and '2020/01/31'
# and callmobile in
# (select mobile id from connections where plan type = 'prepaid')

# Total number of calls made on valentine's day from numbers which received a call the prior day
# select count(*) where date = '2020/02/14' and callmobile in
# (select receivermobile from calls where date = '2020/02/13')

# 1) Number of subscribers with more than 2 mobile numbers
# 2) Total number of calls made by subscriber during Jan 2020 with more than 2 mobiles

# 1> code to count number of mobiles for each customer -
# select cust id, count(*) as conn_count
# from connections group by custid having conn_count >2











