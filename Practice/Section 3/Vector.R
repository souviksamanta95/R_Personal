# TO define vector : *vector* <- c(**,**,**)   : <- like this

myvect1<- c(12, 34, 2, 686)   # c stands for COMBINE
is.double(myvect1)

myvect2<- c(12L, 45L, 48L, 32L)
is.integer(myvect2)

myvect3<- c("Any", "Thing like", "69", "or something else")
is.character(myvect3)

# VECTOR indexing in R starts with "1" instead of "0"

# Sequence - seq(start,end,interval)
seq(1,23,3)   # seq(1,12) and 1:12 are equivalent

# Replicate - rep(number or "character", times of replication)
rep(7,4)      # 7 will be replicatred 4 times

# Combined usage example :

x <- seq(1,6,2)
y <- rep(x,3)
print(y)

# Using square brackets [] for indexing :

x <- c(12,34,56,74)     #Combine
y <- seq(240,260,4)     #Sequence
z <- rep("Hi!",4)       #Repetition
w <- c("a","b","c","d","e")
w[-2]           # returns the whole vector except the 2nd element
w[2:5]          # retuns elements from 2 to 5
w[c(1,3,4)]     # returns value of 1st, 3rd and 4th elements
w[c(-2,-4)]     # returns elements except 2nd and 4th elements

#-----------------------------------------------------------------------------------

# Addition of inequal vectors : R creates elements to make them same length
x+y             #Leaves a warning as size of the bigger one is not multiple of the size of the smaller one
k <- c(6,7,8)
k+y             #No warning because size of y is a multiple of size of k, yet k is repeated 3 times

# Looping with vectors :

x <- rnorm(5)

# Conventional Way ----------


for(i in 1:5)
{
  print(x[i])           # Manual indexing is required for vector or rather array operations
}

# The R Way -----------------

for(i in x)
{
  print(i)              # vector operation occurs automatically
}

#------------------------------------------------------------------------------------------------------------------

N <- 1000
x <- rnorm(N)
y <- rnorm(N)

# Operations with vector forms ---------------------------

# Vectorized or the R approach

c <- x*y
c

# De-vectorized or the conventional approach -------------

d <- rep(NA,N)          # allocating memory with null values for d
for(i in 1:N)
{
  d[i] <- x[i]*y[i]
}
print(d)