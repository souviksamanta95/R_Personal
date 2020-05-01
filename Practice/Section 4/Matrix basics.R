# Basic Matrix formation :
my.data <- 1:30

mat <- matrix(my.data, 5, 6)              # By default, matrix is filled up by column
mat


mat <- matrix(my.data, 5, 6, byrow = T)   # Manually enforcing for entry by row
mat

v1 <- 1:6
v2 <- 7:12
v3 <- 13:18

# Matrix by binding ROWS
mat2 <- rbind(v1,v2,v3)
mat2

# Matrix by binding COLUMNS
mat3 <- cbind(v1,v2,v3)
mat3

# To assign name in row and column as headings : rownames(), colnames()
# To remove a vector from environment selectively : rm(**VECTOR**)

rname <- c("r1", "r2", "r3")
cname <- c("c1", "c2", "c3", "c4")
mat4 <- matrix(1:12, 3, 4)
rownames(mat4) <- rname
colnames(mat4) <- cname
mat4["r1","c3"]
mat4[2,"c2"]
mat4

# To assign names in verctors : names()
vtr <- 4:7
names(vtr) <- c("a", "b", "c", "d")
vtr
vtr["b"]
vtr[3]

# Transpose of matrix : t()

mat4
t(mat4)
