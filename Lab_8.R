set.seed(12345)
# SAN(5)
SANSubset(X=X,n=100,alpha=0.05)
SANSubset(X=X,n=200,alpha=0.05)

# try with different starting seed
set.seed(1989)
SANSubset(X=X,n=100,alpha=0.05)
SANSubset(X=X,n=300,alpha=0.05)

set.seed(12345)
KN(n0=10,alpha=0.05,delta=0.1)
set.seed(1989)
KN(n0=10,alpha=0.05,delta=0.1)
