#http://blog.revolutionanalytics.com/2016/02/multivariate_data_with_r.html

library(MASS)

mu <- c(0,0) # mean
Sigma <- matrix(c(1, 0.5, 0.5, 1), 2) # Covariance matrix
Sigma

# Generate sample from N(mu, Sigma)
bivn <- mvrnorm(5000, mu = mu, Sigma = Sigma )  # from Mass package
head(bivn)                                      
# Calculate kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)   # from MASS package

# Contour plot overlayed on heat map image of results
image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package

# Classic Bivariate Normal Diagram
library(ellipse)
rho <- cor(bivn)
y_on_x <- lm(bivn[,2] ~ bivn[,1])    # Regressiion Y ~ X
x_on_y <- lm(bivn[,1] ~ bivn[,2])    # Regression X ~ Y
plot_legend <- c("99% CI green", "95% CI red","90% CI blue",
                 "Y on X black", "X on Y brown")

plot(bivn, xlab = "X", ylab = "Y",
     col = "dark blue",
     main = "Bivariate Normal with Confidence Intervals")
lines(ellipse(rho), col="red")       # ellipse() from ellipse package
lines(ellipse(rho, level = .99), col="green")
lines(ellipse(rho, level = .90), col="blue")
abline(y_on_x)
abline(x_on_y, col="brown")
legend(3,1,legend=plot_legend,cex = .5, bty = "n")


