sigma = 1.5
m = 30
n = 3

sd_randef = 1
slope = 2

niter = 1000
datout = data.frame(sd_randeff = rep(NA, niter),
                    sd_true = NA,
                    fixef = NA)

for(i in 1:niter) {
  # most common type of shrinkage
  dat = data.frame(block = rep(1:m, each=n),
                   x = rnorm(m*n,0,1),
                   y = NA)

  #int = rnorm(m,0,sd_randef)
  int = rnorm(m,tapply(dat$x, dat$block, mean),sd_randef)
  
  dat$y = 3 + int[dat$block] + slope*dat$x + rnorm(m*n,0,sigma)
  #plot(dat$x, dat$y)
  
  require(nlme)
  
  #mod = lme(y~x, random = ~1|block, dat)
  #plot(int, ranef(mod)$`(Intercept)`)
  #abline(a=0,b=1,lty=2)
  
  mod = "error"
  mod = try(
    lme(y~x, random = ~1+x|block, dat)
    , silent = TRUE)
  if(!is.character(mod)) {
    datout$fixef[i] = fixef(mod)[2]
    
    datout$sd_randeff[i] = sd(ranef(mod)$`(Intercept)`)
    datout$sd_true[i] = sd(int)
    
    # badly specified random effect
    #plot(int[dat$block], dat$x)
    cov(int[dat$block], dat$x)
  }
  if(i/100 == floor(i/100))
    print(i/niter)
}

hist(datout$sd_randeff, breaks = 20); abline(v=sd_randef, lty=2, lwd = 2, col = 2)
abline(v=mean(datout$sd_randeff,na.rm=TRUE), col = "blue", lwd = 2, lty=2)

#hist(datout$sd_true, breaks = 20); abline(v=sd_randef, lty=2, lwd = 2, col = 2)
#abline(v=mean(datout$sd_true), col = "blue", lwd = 2, lty=2)

hist(datout$fixef, breaks = 20); abline(v=slope, lty=2, lwd = 2, col = 2)
abline(v=mean(datout$fixef,na.rm=TRUE), col = "blue", lwd = 2, lty=2)

