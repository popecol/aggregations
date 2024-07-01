
# GAMM modeling.
# Authors: micwaw2@st.amu.edu.pl & lechu@amu.edu.pl


# Setup -------------------------------------------------------------------

library(mgcv)
library(parallel)
library(ncf)

source("R/step.R")


# The data ----------------------------------------------------------------

load("data/data.RData")
summary(data)

data <- subset(data, n > 2)
data <- transform(data, log_n = log(n))
# data <- transform(data, dispersion = dispersion / 100)
data <- droplevels(data)

hist(data$aggregation)
hist(data$dispersion, breaks = 30)
hist(log1p(data$dispersion), breaks = 30)
# hist((data$dispersion)^0.75, breaks = 30)
hist(data$n)
hist(log1p(data$n), breaks = 30)

plot(dispersion ~ log_n, data)
plot(dispersion ~ n, data)
plot(dispersion ~ log_dens, data)
plot(dispersion ~ dens, data)
plot(dispersion ~ Grassland, data)
plot(dispersion ~ Wetness, data)
plot(dispersion ~ year, data)
plot(log_n ~ year, data)
plot(Crow ~ year, data)


# GAMM --------------------------------------------------------------------

ctrl <- list(maxit = 1000, epsilon = 1e-05)
# nthreads <- detectCores(logical = FALSE)
nthreads <- 8L  # no. of cores to be used in 'bam'

# List of predictors:
# dput(names(data))

v <- c("log_dens", "log_dens_lag", "Crow", "Crow_lag", "Cropland", "Grassland")

f <- formula(paste0("dispersion ~ ", paste0("s(", v, ", k = k, bs = 'cr')", collapse = " + ")))
# f01 <- update(f, ". ~ . + s(observer_id, bs = 're') + s(plot_id, bs = 're') + s(fyear, bs = 're') + s(year, k = 10, bs = 'gp', m = c(3, 1)) + s(x, y, k = 20, bs = 'gp')")
f01 <- update(f, ". ~ . + s(plot_id, bs = 're') + s(fyear, bs = 're')")
f01

k <- 5

# Full model
full <- bam(f01, data, family = tw(), discrete = TRUE, nthreads = nthreads, control = ctrl)
summary(full)

# Response curves
op <- par(mar = c(5, 4, 1, 1))
plot.gam(full, scale = 0, scheme = 2, pages = 1, seWithMean = FALSE, residuals = TRUE, trans = I, pch = 21)
par(op)

# Backward elimination 
fit <- backward(full, p = 0.05)

# The final model
summary(fit)

# Some diagnostics:
op <- par(mfrow = c(2, 2)); gam.check(fit, rep = 100); par(op)

# Response curves
op <- par(mar = c(5, 4, 1, 1))
plot.gam(fit, scale = 0, scheme = 2, pages = 1, seWithMean = FALSE, residuals = TRUE, trans = I, pch = 21)
par(op)

save(full, file = "data/full.RData")
save(fit, file = "data/fit.RData")

# Spatial autocorrelation -------------------------------------------------

dat <- full$model
idxy <- data[c("plot_id", "x", "y")]
idxy <- idxy[!duplicated(idxy), ]
dat <- merge(dat, idxy, by = "plot_id", all.x = TRUE)
# summary(dat)

x <- dat$x; y <- dat$y
dat$res <- z <- residuals(full)
# z <- dat$dispersion
corrgram <- spline.correlog(x, y, z, na.rm = TRUE, xmax = 100, df = 10, resamp = 1000)
plot(corrgram)

# Plot
b <- corrgram$boot[2]$boot.summary$predicted
xx <- b$x[1, ] # distance
yy <- b$y[6, ]; lwr <- b$y[2, ]; upr <- b$y[10, ] # median & 95% quantiles
plot(xx, yy, ylim = range(yy, lwr, upr), type = "n", xlab = "Distance", ylab = "Correlation")
lines(xx, yy); lines(xx, lwr, lty = 2); lines(xx, upr, lty = 2)
abline(h = 0)
