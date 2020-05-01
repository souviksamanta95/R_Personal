


myplot <- function(rows)
{
  Data <- MinutesPlayed[rows,,drop=F]
  matplot(t(Data),type="b", pch=15:20, col=c(1:4,6))
  legend("bottomleft", inset=0.01, legend=Players[rows], pch=15:20, col=c(1:4,6), horiz=F)
}

myplot(1)


myplot <- function(data,rows)
{
  Data <- data[rows,,drop=F]
  matplot(t(Data),type="b", pch=15:20, col=c(1:4,6))
  legend("bottomleft", inset=0.01, legend=Players[rows], pch=15:20, col=c(1:4,6), horiz=F)
}

myplot(Salary,1:3)

#Assigning default parameters - Put values in the function (1st line)


myplot <- function(data,rows=1:10)
{
  Data <- data[rows,,drop=F]
  matplot(t(Data),type="b", pch=15:20, col=c(1:4,6))
  legend("bottomleft", inset=0.01, legend=Players[rows], pch=15:20, col=c(1:4,6), horiz=F)
}

myplot(Salary,3)


