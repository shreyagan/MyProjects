# Shreya Ganeshan
# Bootstrapping Practice 

# ---------- Nonparametric Bootstrap -----------
install.packages('boot', dependencies=TRUE)
library(boot)

# read in the data - contains observations of random variable X
bootsobs = read.table("bootsobs.txt", header = F)
bootsobs

# objective: estimate the expected value of sin(X) or B = E(sin(X))

# use a few thousand (i.e., more than 2000) resamples to investigate sampling dist. 
# the statistics we want to bootstrap: E(sinX), mean, std. error, 95% CI, bias
theta = sin(bootsobs)
# The boot function requires a statistic that accepts both a data vector AND
# a vector of indices. BUT the mean() function does not do that at the moment!
# So we need to write a function to fix that.

Xbar = function(x, d){
  mean(sin(x[d,]))
}

# replicated thetas 
theta.boots.obj.np = boot(data = bootsobs, statistic = Xbar, R=3000) # 3000 replicates
theta.boots.obj.np
# "t" in the boot object is the pile of 3000 phats computed on each replicate
# "t0" - line I'm drawing on the histogram at value phat should be if the data equally dist.

# plenty of ways to compute statistics esp. in BOOT pacakge
hist(theta.boots.obj.np$t)
hist(theta.boots.obj.np$t, main = "E(sin(X)) Non-Parametric Bootstrap", xlab = "E(sin(x))", breaks = 50)

# mean 
thetaMean = mean(theta.boots.obj.np$t)
thetaMean

# standard error
thetaStdErr = sd(theta.boots.obj.np$t)
thetaStdErr

# bias
thetaBias = theta - thetaMean
thetaBias

# 95% CI
thetaCI = quantile(theta.boots.obj.np$t,c(0.025,0.975))
thetaCI

# ---------- Parametric Bootstrap -----------
# need a function that generates observations from normal dist.
# use rnorm
# suppose that you know that the sample in  previous question was from a normal dist. 
# use this knowledge to improve bootstrap estimates
# no longer have to rely only on empirical distribution! 
# bootstrapping from parametric dist. (like the normal) rather than empirical dsit. = parametric bootstrap

# If you know data come from a normal dist., would use N(mean, sd)
# which normal distribution is best with these data?

# do same analysis as question 1 but with parametric bootsrap

# establish variables
mean = mean(x)
sd = sd(x)

# rnorm(n, mean, sd)
normaldist = sin(rnorm(50, mean, sd)) # n = 50

# bootstrap function - finding ybar
normal.boots.obj.p = function(normaldist,d) {
  mean(normaldist[d])
}

# generating a normal dist. random sample
random_normal = function(d,p) {
  sin(rnorm(50, mean, sd))
}

# now the bootstrap
normal.boots.p = boot(data = normaldist, statistic = normal.boots.obj.p, R = 3000, sim = "parametric", ran.gen = random_normal)
normal.boots.p

# plotting estimates
hist(normal.boots.p$t, main = "E(X) - Parametric Bootstrap", xlab = "E(X)", breaks = 50)

# mean 
normalMean = mean(normal.boots.p$t)
normalMean

# standard error
normalStdErr = sd(normal.boots.p$t)
normalStdErr

# bias
normalBias = mean - normalMean
normalBias

# 95% CI
normalCI = quantile(normal.boots.p$t,c(0.025,0.975))
normalCI

# ---------- Comparing Parametric and Nonparametric Bootstrap -----------
# repeat Questions 1 and 2 such that: 
# theta = max (from 1<= i <= n) (xi) --> technically order statistic
# n = 20

# REPEAT QUESTION 1

# parameter
theta2 = max(bootsobs)
theta2

# boot function
Xbar2 = function(x, d){
  mean(max(x[d,]))
}

# replicating thetas 
theta.boots.obj.np2 = boot(data = bootsobs, statistic = Xbar2, R=3000) # 3000 replicates
theta.boots.obj.np2

# plotting bootstrap
hist(theta.boots.obj.np2$t)
hist(theta.boots.obj.np2$t, main = "E(max(X)) Non-Parametric Bootstrap 2", xlab = "E(max(x))", breaks = 50)

# mean 
thetaMean2 = mean(theta.boots.obj.np2$t)
thetaMean2

# standard error
thetaStdErr2 = sd(theta.boots.obj.np2$t)
thetaStdErr2

# bias
thetaBias2 = theta2 - thetaMean2
thetaBias2

# 95% CI
thetaCI2 = quantile(theta.boots.obj.np2$t,c(0.025,0.975))
thetaCI2

# REPEAT QUESTION 2

# establish variables
mean2 = mean(x)
sd2 = sd(x)

# rnorm(n, mean, sd)
normaldist2 = rnorm(20, mean, sd) # n = 20

# bootstrap function - finding ybar2
normal.boots.obj.p2 = function(normaldist2,d) {
  mean((normaldist2[d]))
}

# generating a normal dist. random sample
random_normal = function(d,p) {
  max(rnorm(20, mean2, sd2))
}

# now the bootstrap
normal.boots.p2 = boot(data = normaldist2, statistic = normal.boots.obj.p2, R = 3000, sim = "parametric", ran.gen = random_normal)
normal.boots.p2

# plotting estimates
hist(normal.boots.p2$t, main = "E(max(X)) - Parametric Bootstrap 2", xlab = "E(max(X))", breaks = 50)

# mean 
normalMean2 = mean(normal.boots.p2$t)
normalMean2

# standard error
normalStdErr2 = sd(normal.boots.p2$t)
normalStdErr2

# bias
normalBias2 = mean2 - normalMean2
normalBias2

# 95% CI
normalCI2 = quantile(normal.boots.p2$t,c(0.025,0.975))
normalCI2

# nonparametric bootstrapping better for large samples 
# parametric bootstrapping better for smaller samples 
# parametric - using parametric models in place of empirical dist. (like in nonparametric)
# parametric may fail if model doesn't contain true distribution
# parametric provides more power due to arbitrary model choice
# BUT small sample sizes in parametric bootstrapping doesn't capture full behavior 
  # of data and higher population moments
# AND inherent contradiction with parametric bootstrapping:
  # relies on MLE which is typically reserved for large samples 