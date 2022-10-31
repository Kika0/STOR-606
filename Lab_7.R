# Lab 7

# set the variable values
phi <- 0.9
sigma <- 1
y0 <- 50

# calculate optimal cut off
dOptimal <- AR1MSE(mu=200, phi=phi, sigma=sigma, Y0=y0, n=1, m=2000)$d
# Y <- AR1(mu=200, phi=0.7, sigma=1,Y0=0, m=2000)
# dEstimated <- MSER(Y)$d

# optimum will not change but estimate will
# calculate 10 estimates
dest <- rep(0,10)
for (x in 1:10) {
dest[x] <- MSER(AR1(mu=200, phi=phi, sigma=sigma,Y0=y0, m=2000))$d
}

dOptimal
# report bias
mean(dest)-dOptimal
# report relative variability
sd(dest)/dOptimal
