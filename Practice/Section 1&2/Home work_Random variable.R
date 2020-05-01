count=0
sampl=100000
for(i in rnorm(sampl))
  {
    if(i>=-1)
      {
        if(i<=1)
         {
         count=count+1
         }
      } 
  }
avg=count*100/sampl
print(avg)