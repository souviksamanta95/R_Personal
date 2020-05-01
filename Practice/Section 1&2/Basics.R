#Comments only
#Integer   Mention "L" otherwise it will be treated as double
x<-5L
typeof(x)

#Double
y=2.59
typeof(y)

###   y=2.5  ;  y<-2.5  ;  2.5->y   are analogous

#Complex
z<- 3+2i
typeof(z)

#Character
a="Hello!"
typeof(a)

#Logical   T, TRUE : True   ;   F, FALSE : False
b<-T
typeof(b)



A<- 5.4
B<- 2.7
C<- B / A
C
sq<-A**3
sq


s1<-"Hello!"
s2<-"I am Souvik"
sp=paste(s1,s2)
print(sp)
sp


# Logical :
# TRUE, "T" means same remember to put it in caps.
# Same for FALSE and "F".
#   "="   means assignment same as "<-" and "->"
#   "=="  literally means equals in value.
#   "!="  means NOT equal to.
#   "|"   means OR
#   "!"   means NOT
#   "&"   means AND

r1 = T
r2 = T
r3 = F
r4 = F

r1 | r2
r1 | r3
r4 & r2
r1 & r2

isTRUE(r3)
isFALSE(r3)