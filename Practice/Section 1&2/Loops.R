# WHILE Loop
count<-1
while(count<=10){
  print(count)
  count<-count+1
}

# FOR Loop
for(i in 1:7){
  print("I am RStudio !")
}

# To remove a variable use "rm"
# IF and ELSE Loops

rm(rslt)
x<-rnorm(1)
if(x>1){
  rslt<-"Greater than 1"
  print(rslt)
}else{
    if(x>=-1){
      rslt<-"Between -1 and 1"
      print(rslt)
    }else{
      rslt<-"Less than -1"
      print(rslt)
    }
}

# ELSE IF statement in place of nested IF

rm(rslt)
x<-rnorm(1)
if(x>1){
  rslt<-"Greater than 1"
  print(rslt)
}else if(x>=-1){
    rslt<-"Between -1 and 1"
    print(rslt)
  }else{
    rslt<-"Less than -1"
    print(rslt)
}