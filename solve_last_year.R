# problem-01 --------------------------------------------

A = matrix(c(9, 7, 13, 0, 2, 1, 6, 8, 3), 3, 3, T)

sub = function(x){

row = c()
for(i in 1:nrow(x)){
if(i %% 2 == 1) {row[i] = i}
}

row = na.omit(row)

col = c()
for(i in 1:ncol(x)){
if(i %% 2 == 1) {col[i] = i}
}

col = na.omit(col)

return(x[row, col])

}

sub(A)

#----------------- Problem-02 -----------------------
curve(dnorm(x), -4, 4)
rect(-1, 0, 1, dnorm(1), col = "grey")

# Problem-03 ----------------------------------------

x = 1:10; y = 11:20

MP = function(x, y){

MAD = function(a){
mean(abs(a - median(a)))
}

X = (x - median(x))/MAD(x)
Y = (y - median(y))/MAD(y)

return(median(X * Y))

}

MP(x, y)


#################### 2019 #########################
#-------------------- problem-01 ------------------
A = matrix(1:9, 3, 3, T)

range = function(x){
  range = c()
  for(i in 1:nrow(x)){
  range[i] = (max(x[i, ]) - min(x[i, ]))
  }
  return(range)
}
range(A)


#------------------- Simulation -------------------
v = c()
for(x in 1:1000){
  u = c()
  u = runif(1, 0, 1)
  i = 1
  while(u >= exp(-1)){
    
    u = cumprod(u)
    
    u = runif(1, 0, 1)
    i = i + 1
  }
  y = i - 1
  
  v[x] = y
  
}

v
hist(v)


#---------------- problem-03 ----------------------
A = matrix(1:9, 3, 3, T)

sums = function(x){
  rowSums(x^3)
}
sums(A)


#  incourse -------------
x = rpois(10, 7.6)
x
A.sort = function(x){
  sort(x)
}
A.sort(x)

own.med = function(x){
  median(A.sort(x))
}
own.med(x)

y = rexp(100, .05)
y
own.med(y)


#---------------------Final 2022-------------------------
# problem-05 ---------------------------------------

N = function(n, lambda){
  x = rpois(n, lambda)
  y = sqrt(n) * (mean(x) - lambda)
  
  return(y)
}
N(101001000, 1.5)

v = c()
for (i in 1:20) {
  v[i] = N(101001000, 1.5)
}
v
mean(v); var(v)

#  problem-06 ------------------------------------

x = rbinom(35, 100, 0.05)

A = matrix(x, 7, 5, T)
A

fx = function(x){
  
  count = c()
  
  for(i in 1:ncol(x)){
    count[i] = sum(x[, i] > 5)
  }
  return(count)
}

fx(A)

chickwts

med = function(x){
  
  n = length(x)
  y = sort(x)
  
  median = if(n %% 2 == 0) {(y[n/2]+y[n/2 + 1])/2} else{y[(n+1)/2]}
  
  return(median = median)
}
med(chickwts$weight)
median(chickwts$weight)

