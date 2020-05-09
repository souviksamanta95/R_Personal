
v <- c(1,55,61,66,80,90,95,115,170)
summary(v)
IQR <- IQR(v, type = 2)
IQR

boxplot(v, horizontal = T)
MAX_bp <- as.numeric(Q3+(1.5*IQR)):MAX_bp


outval <- boxplot(v)$out
outval

which(v %in% outval)
