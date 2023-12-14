##variance of regression coefficients' estimators (covariance matrix) and impact of multicollinearity

#the idea is to simulate two multiple regression scenarios: one with nearly orthogonal (non correlated) predictors, one with highly correlated predictors

library(car)

#---------------non-correlated predictors

#set seed for reproducibility
set.seed(340)

#create non-correlated predictors
X_1 <- rnorm(100, 20, 2)
X_2 <- rnorm(100, 40, 4)

#check their correlation 
cor(X_1, X_2) # ~ 0

#put predictors in a data.frame
Dtf_ortho <- data.frame(X1 = X_1, X2 = X_2)

set.seed(353)

#simulate response
Dtf_ortho$Y_ortho <- with(Dtf_ortho, rnorm(n = 100, mean = 3 + 10*X1 + 10*X2, sd = 25))

Ortho_mod <- lm(Y_ortho ~ X1 + X2, data = Dtf_ortho)

summary(Ortho_mod)

#extract var-cov matrix of regression coefficients' estimators
vcov(Ortho_mod)

#extract diagonal elements, i.e. variances
diag(vcov(Ortho_mod))

#manually derive variances - note that these can be derived like this only in case of truly orthogonal predictors
#correlation among X1 and X2 is not exactly 0, so numbers computed by hand are slightly different from what you
#get in the diagonal of vcov

#variance for X1
(sigma(Ortho_mod)^2)/sum((Dtf_ortho$X1 - mean(Dtf_ortho$X1))^2)

#variance for X2
(sigma(Ortho_mod)^2)/sum((Dtf_ortho$X2 - mean(Dtf_ortho$X2))^2)


#---------------correlated predictors

set.seed(365)

#create correlated predictors
X_1 <- rnorm(100, 20, 2)
X_2 <- X_1 + rnorm(100, 0, 2) #add white noise

cor(X_1, X_2)

#put predictors in a data.frame
Dtf_cor <- data.frame(X1 = X_1, X2 = X_2)

set.seed(383)

#simulate response
Dtf_cor$Y_cor <- with(Dtf_cor, rnorm(n = 100, mean = 3 + 10*X1 + 10*X2, sd = 25))

Cor_mod <- lm(Y_cor ~ X1 + X2, data = Dtf_cor)

summary(Cor_mod)

#extract var-cov matrix of regression coefficents' estimators
vcov(Cor_mod)

#extract diagonal elements, i.e. variances
diag(vcov(Cor_mod))

#extract model matrix to then manually derive var-cov matrix of regression coefficients' estimators
Dtf_cor_modmat <- model.matrix(Cor_mod)

#compute var-cov matrix multiplying sigma^2 times the inverse of the crossproduct of the model matrix with itself, i.e. solve((X'X))
#note that solve is the function to compute the inverse of X'X
Handmade_cov_mat <- (sigma(Cor_mod)^2) * solve(crossprod(Dtf_cor_modmat))

#check numbers are equal
all.equal(vcov(Cor_mod), Handmade_cov_mat) #T

#now derive variance of each regression coefficient using the variance inflation factor

#you can get this measure using car::vif
VarInflFact <- vif(Cor_mod)

#it's also possible to compute the variance inflation factors by-hand

#vif for X1
X1_X2_rsq <- summary(lm(X1 ~ X2, data = Dtf_cor))$r.squared

#vif for X2
#summary(lm(X2 ~ X1, data = Dtf_cor))$r.squared

all.equal((1/(1 - X1_X2_rsq)), unname(VarInflFact[1]))

#as you can see, the diagonal elements of the covariance matrix are equal to what I derived manually using the vif
Var_X1 <- ((sigma(Cor_mod)^2)/sum((Dtf_cor$X1 - mean(Dtf_cor$X1))^2)) * unname(VarInflFact[1])
Var_X2 <- ((sigma(Cor_mod)^2)/sum((Dtf_cor$X2 - mean(Dtf_cor$X2))^2)) * unname(VarInflFact[2])

all.equal(c(Var_X1, Var_X2), unname(diag(vcov(Cor_mod))[c(2, 3)])) #TRUE

