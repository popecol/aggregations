
# Plots generated for publication.
# Authors: micwaw2@st.amu.edu.pl & lechu@amu.edu.pl


# setup -------------------------------------------------------------------
dir <- dir()

if(sum(dir == "figures") == 0) {
  dir.create("figures")
} 

source("R/setup.R")
library(mgcv)

col <- adjustcolor("black", alpha.f = 1/2)
# models & data -----------------------------------------------------------
load("data/full.RData")
load("data/fit.RData")
load("data/data.RData")
data <- subset(data, n > 2)
data <- transform(data, log_n = log(n))
# data <- transform(data, dispersion = dispersion / 100)
data <- droplevels(data)
# Plots ------------------------------------------------------------------

# Spatial distance deviation plot

x1 <- c(0, 0, 20, 0, 0, 0, 0, 0, 0, 0)
x2 <- c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
x3 <- c(0, 5, 3, 2, 0, 0, 0, 7, 3, 0)
x4 <- c(0, 2, 2, 1, 0, 0, 0, 3, 2, 0)

jpeg("figures/01_sdd.jpeg", width = 1200, height = 1200, quality = 100)
op <- par(mfrow = c(2, 2), mar = c(6, 6, 7, 3) + 0.1)
plot_obs(x1, cex = 15,  cex.lab = 3, cex.axis = 2, cex.main = 3, cex.sub = 2.3,)
mtext("A", side = 3, line = 0.5, cex = 3, adj = 0)

plot_obs(x2, cex = 15,  cex.lab = 3, cex.axis = 2, cex.main = 3, cex.sub = 2.3,)
mtext("B", side = 3, line = 0.5, cex = 3, adj = 0)

plot_obs(x3, cex = 15,  cex.lab = 3, cex.axis = 2, cex.main = 3, cex.sub = 2.3,)
mtext("C", side = 3, line = 0.5, cex = 3, adj = 0)

plot_obs(x4, cex = 15,  cex.lab = 3, cex.axis = 2, cex.main = 3, cex.sub = 2.3,)
mtext("D", side = 3, line = 0.5, cex = 3, adj = 0)
par(op)
dev.off()

# rsd histogram

jpeg("figures/02_hist_rsd.jpeg", width = 1200, height = 800, quality = 100)
op <- par(mar = c(6, 9, 5, 3) + 0.1)
hist(data$dispersion, breaks = 30, main = "Histogram of relative spatial dispersion index",
     col = col, xlab = "rsd", cex.lab = 3.8, cex.axis = 2.8, cex.main = 3, ylab = NULL)
mtext("Frequency", side = 2, line = 4, cex = 3.8) 
box(col = "black", lty = "solid")
par(op)
dev.off()

# GAMM response curves

r1 <- plot.gam(fit, select = 1)
r2 <- plot.gam(fit, select = 2)

jpeg("figures/03_rescurvs.jpeg", width = 1300, height = 800, quality = 100)
op <- par(mfrow = c(1, 2), mar = c(7, 8, 4, 8) + 0.1, mgp = c(5, 1.5, 0))

plot.gam(fit, scale = 0, scheme = 1, seWithMean = TRUE, residuals = TRUE, trans = I,
         pch = 21, select = 1, jit = TRUE,
         xlab = "Population log-density", ylab = "Relative spatial dispersion",
         cex.lab = 3.8, cex.axis = 2.8, cex = 3, col = col, lwd = 2.5)
lines(r1[[1]][["x"]], r1[[1]][["fit"]], lwd = 4, col = col)
mtext("A", side = 3, line = 0.5, cex = 4, adj = 0)

plot.gam(fit, scale = 0, scheme = 1, seWithMean = TRUE, residuals = TRUE, trans = I,
         pch = 21, select = 2,
         xlab = "Lagged population log-density", ylab = "Relative spatial dispersion",
         cex.lab = 3.8, cex.axis = 2.8, cex = 3, col = col, lwd = 2.5)
lines(r2[[2]][["x"]], r2[[2]][["fit"]], lwd = 4, col = col)
mtext("B", side = 3, line = 0.5, cex = 4, adj = 0)
par(op)
dev.off()
