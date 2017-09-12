samples = 100
trueP = 0.6
xRange = seq(-10,9.99,by=0.4)
trueMu1 = -2; trueSd1 = 1.1
trueMu2 = 4; trueSd2 = 1.2
dist1 = rnorm(samples * trueP, mean = trueMu1, sd = trueSd1)
dist2 = rnorm(samples * (1-trueP), mean = trueMu2, sd = trueSd2)
distAll = c(dist1, dist2)
distAllLabels = c(rep(1,length(dist1)),rep(2,length(dist2)))

# What we don't know
hist1 = hist(dist1,breaks = xRange, plot = F)
hist1$density = hist1$density * trueP
hist2 = hist(dist2, breaks = xRange, plot = F)
hist2$density = hist2$density * (1-trueP)
histAll = hist(distAll, breaks = xRange, plot = F)
#plot(histAll, col=rgb(0,1,0,0.4), main="Multiple Distributions", freq=F, ylim=c(0,1))
#plot(hist1, col=rgb(1,0,0,0.6), add = TRUE, freq=F)
#plot(hist2, col=rgb(0,0,1,0.6), add = TRUE, freq=F)
#lines(x=xRange, y = dnorm(xRange, mean = mean(dist1), sd = sd(dist1)))
#lines(x=xRange, y = dnorm(xRange, mean = mean(dist2), sd = sd(dist2)))

# initial guess
mus = sample(distAll,2)
mu1 = min(distAll); sd1 = sd(distAll)
mu2 = max(distAll); sd2 = sd(distAll)
p = 0.5

plot(hist1, col=rgb(1,0,0,0.4), freq=F, ylim=c(0,1), main = "First Guess")
plot(hist2, col=rgb(0,0,1,0.4), add = TRUE, freq=F)
lines(x=xRange, y = dnorm(xRange, mean = mu1, sd = sd1), col = "brown")
lines(x=xRange, y = dnorm(xRange, mean = mu2, sd = sd2), col = "dark green")
lines(x=xRange, y = dnorm(xRange, mean = trueMu1, sd = trueSd1), col = "red")
lines(x=xRange, y = dnorm(xRange, mean = trueMu2, sd = trueSd2), col = "blue")

# EM Algorithm  from "Machine Learning An Algorithmic Perspective" 
for (i in 1:50) {
  numerator   = p * exp(-(distAll-mu1)**2/2*sd1)
  denominator = p * exp(-(distAll-mu1)**2/2*sd1) + (1-p) * exp(-(distAll-mu2)**2/2*sd2)
  gamma = numerator / denominator
  
  mu1 = sum((1 - gamma) * distAll) / sum(1-gamma)
  mu2 = sum(gamma * distAll) / sum(gamma)

  sd1 = sum((1-gamma)*(distAll-mu1)**2)/sum(1-gamma)
  sd2 = sum(gamma*(distAll-mu2)**2)/sum(gamma)

  p = sum(gamma)/length(gamma)
}


plot(hist1, col=rgb(1,0,0,0.4), freq=F, ylim=c(0,1), main = "Truth")
plot(hist2, col=rgb(0,0,1,0.4), add = TRUE, freq=F)
lines(x=xRange, y = dnorm(xRange, mean = trueMu1, sd = trueSd1)*trueP, col = "red")
abline(v=trueMu1, col="red")
lines(x=xRange, y = dnorm(xRange, mean = trueMu2, sd = trueSd2)*(1-trueP), col = "blue")
abline(v=trueMu2, col="blue")

# What we do know
plot(histAll, col=rgb(0,1,0,0.2), main="What we guess", freq=F, ylim=c(0,1))
lines(x=xRange, y = dnorm(xRange, mean = mu1, sd = sd1)*p, col = "brown")
abline(v=mu1, col="brown")
lines(x=xRange, y = dnorm(xRange, mean = mu2, sd = sd2)*(1-p), col = "dark green")
abline(v=mu2, col="dark green")


plot(histAll, col=rgb(0,1,0,0.2), main="What we see", freq=F, ylim=c(0,1))
