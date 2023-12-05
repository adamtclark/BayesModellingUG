# calculate the standard error of the mean for a variable
mu = 0
std = 1

niter = 1e3
boot_summary = matrix(nrow = niter, ncol = 1)
colnames(boot_summary) = c("muX")

n = 30
for(i in 1:niter) {
  X = rnorm(n, mu, std)
  boot_summary[i] = mean(X)
}

#hist(boot_summary)
sd(boot_summary)

std/sqrt(n-1)








set.seed(1234)
n = 50

require(mvtnorm)
mu = c(0, 0)
Sig = cbind(c(1, 0.95), c(0.95, 1))
mvout = rmvnorm(n, mu, Sig)
X = mvout[,1]; Z = mvout[,2]

plot(X, Z)

Y = rnorm(n, 0, 1) + X

mod = lm(Y~X + Z)
summary(mod)


resid_Z = resid(lm(Z~X))
plot(X, resid_Z)

mod = lm(Y~X + resid_Z)
summary(mod)




resid_X = resid(lm(X~Z))
plot(resid_X, Z)

mod = lm(Y~resid_X + Z)
summary(mod)



XpZ = X + Z

mod_ad = lm(Y~XpZ)
summary(mod_ad)

