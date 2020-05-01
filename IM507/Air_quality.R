# Write  R code to call the (built-in) dataset airquality. 

aq <- airquality

# Check whether it is a data frame or not? 

is.data.frame(aq)

# How many records and variables are there in the dataset?

dim(aq)

# Identify the datatypes of each variable.

str(aq)

# Are there any missing values in the data set? If yes, then identify which variable has missing value.

summary(aq)     # Ozone and Solar.R shows NA values, i.e. missing data

# Total number of missing values

sum(is.na(aq))

# Replace the missing values with the mean of the variable. How many values are you replacing for each variable and with what value of mean?

aq$Ozone[is.na(aq$Ozone)] <- mean(aq$Ozone,na.rm = T)
aq$Solar.R[is.na(aq$Solar.R)] <- mean(aq$Solar.R,na.rm = T)

# Identify min, maximum, mean, median standard deviation, variance, and NA values for each variable. Explain each variable with the help of these values.

summary(aq)

# Order the entire data frame by Temp and Wind column.

aq1 <- aq[order(aq$Temp),]
aq2 <- aq[order(aq$Wind),]
aq1
aq2

# Create a new dataframe by removing the variables 'Solar.R' and 'Wind' and display the new data frame.

aq
aq3 <- aq[c(-2,-3)]
aq3

# Write a code to display all the values of temperature variable only.

aq$Temp

# Print the portion of the data set for 6th month where the temperature is >80.
#
#              for(i in 1:length(aq$Temp))
#               {
#            if(aq$Month[i]==6)
#             {
#             if(aq$Temp[i]>80)
#            {
#             print(aq$Temp[i])
#          }}}

aqs <- subset(aq,aq$Month==6&aq$Temp>80)
aqs