# A1 ----
# part (b) general rejection algorithm
# define pdf f_X(x) and g(x),m(x)
f_X <- function(x) 1/2*sin(x)
g <- 1/pi
m <- 1/2
# rejection algorithm
V <- runif(1,min=0,max=g)
U <- runif(1)
# while loop (note that m is constant function so has same value for each V on [0,pi])
# loop until statement is False(<= condition satisfied)
X <- V
while (U>f_X(V)/m) {
  V <- runif(1,min=0,max=g)
  U <- runif(1)
  X <- V
}
X
