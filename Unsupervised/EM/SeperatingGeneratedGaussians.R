library(mclust)
library(ggplot2)

# the true distribution parameters
m = c(33,40,60)
s = c(3,2,5)

# sample from true distributions
c1 = rnorm(50,m[1],s[1])
c2 = rnorm(50,m[2],s[2])
c3 = rnorm(50,m[3],s[3]) 
c4 = c(c1,c2,c3)
labels = factor(c(rep(1,50),rep(2,50),rep(3,50)))
c5 = data.frame(labels, c4)

# labelled
qplot(c4, data = c5, fill =labels)
# unlabelled
qplot(c4, data = c5)

# plot the true population distributions
l = min(m[1] - 4 * s[1], m[2] - 4 * s[2], m[3] - 4 * s[3])
u = max(m[1] + 4 * s[1], m[2] + 4 * s[2], m[3] + 4 * s[3])
x = seq(l,u,length.out = 50)
og1 = dnorm(x,m[1],s[1])*50
og2 = dnorm(x,m[2],s[2])*50
og3 = dnorm(x,m[3],s[3])*50
og4 = c(og1,og2,og3)
og5 = data.frame(labels,og4)

# attempt to recover the contributing distributions
fit = Mclust(c4)
gm = fit$parameters$mean
gs = sqrt(fit$parameters$variance$sigmasq)
g1 = dnorm(x,gm[1],gs[1])*50
g2 = dnorm(x,gm[2],gs[2])*50
g3 = dnorm(x,gm[3],gs[3])*50


plot(x,og1,type="l",ylim=c(min(og4),max(og4)),col="red",main="Estimated vs True Distributions")
lines(x,og2,type="l",col="green")
lines(x,og3,type="l",col="blue")
lines(x,g1, type = "l",col="dark red")
lines(x,g2,type="l",col="light green")
lines(x,g3,type="l",col="light blue")
legend(68,10,c("Dist. 1", "Dist. 2", "Dist. 3",
              "Est. Dist. 1", "Est. Dist. 2", "Est. Dist. 3"),
       lty=c(1,1,1,1,1,1),
       col = c("red","green","blue","dark red", "light green","light blue"))
