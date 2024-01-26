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

# Problem-02 ----------------------------------------

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



