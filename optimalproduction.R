#creating a general function for optimizing production costs

#k is the normal prod cost/K the additional prod cost
#r is max prod per time block/a is max additional prod
#d demand vector and s storage costs

optimal.production=function(d, r, k, a, K, s){
  length(d)= n
  
  B=matrix(0, nrow=n, ncol=n)
  B[1,1]=1
  for (i in 2:n){
    B[i,]=B[i-1,]
    B[i,i]=1
  }
  
  C=cbind(B,B) #MATRIX FOR s
  
  A=rbind(diag(2*n), C, diag(2*n))
  
  #what is being minimized
  f1=rep(NA, n)
  f2= rep(NA, n)
  for ( i in 1:n){
    f1[i]= (n-i)*s + k
    f2[i]= (n-i)*s + K
  } 
  
  f= c(f1, f2)
  
  
  ineq=c(rep(">=",(2* n + n -1 )), "=", rep("<=",(2*n)))
  
  #create a loop for the middle chunk of 
  #create two for loops 
  newd=rep(NA, n)
  newd[1]=d[1]
  for (i in 2:n){
    newd[i]=newd[i-1]+d[i]
  }
  b=c(rep(0,(2*n)), newd, rep(r,n), rep(a,n))
  
  lp("min", f, A, ineq, b)$solution
  
}

#loops for b vector 
testd=c(100, 150, 200, 400, 600, 900, 900, 850, 500, 300, 250, 100)
newd=rep(NA,12)
newd[1]=testd[1]
for ( i in 2:12){
 newd[i]=newd[i-1]+testd[i]
}

optimal.production(testd, 600, 30, 300, 45, 3)
optimal.production(testd, 500, 38, 200, 41, 6)

#test for the inequality direction
n=11
ine=c(rep(">=",(2* n + n -1 )), "=", rep("<=",(2*n)))
a= 1:12
c= c(1,2,3, a)

#test for the f loop
f11=rep(NA, n)
f21= rep(NA, n)
for ( i in 1:12){
  f11[i]= (12-i)*3 + 30
  f21[i]= (12-i)*3 + 45
} 

f= c(f11, f21)
