B=matrix(0, nrow=12, ncol=12)
B[1,1]=1
for (i in 2:12){
  B[i,]=B[i-1,]
  B[i,i]=1
}

C=cbind(B,B) #MATRIX FOR S

A=rbind(diag(24), C, diag(24))

f=c(63, 60, 57, 54, 51, 48, 45, 42, 39, 36, 33, 30, 78,
    75, 72, 69, 66, 63, 60, 57, 54, 51, 48, 45)

ineq=c(rep(">=",35), "=", rep("<=",24))

b=c(rep(0,24), 100, 250, 450, 850, 1450, 2350, 3250, 4100, 4600,
    4900, 5150, 5250, rep(600,12), rep(300,12))
#ERROR
#I forgot to add up the total demand at the end of each month, and instead 
#only listed that specific month's demand as a constraint 

lp("min", f, A, ineq, b)$solution
#NOW RETURNS 250 SURPLUS FOR MONTH 8