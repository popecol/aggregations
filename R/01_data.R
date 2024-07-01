
# Data preparation.
# Author: lechu@amu.edu.pl


# Setup -------------------------------------------------------------------

# library(terra)
# library(fst)

source("R/setup.R")

# Density data
load("../MPPL_data/W/data/data.RData")
dens <- data; rm(data)
dens <- transform(dens, idy = interaction(id_pow, year, sep = "_"))
idy <- sort(unique(dens$idy))


# Raw data ---------------------------------------------------------------

W_all <- readxl::read_xlsx("data/03_W_all.xlsx")

W_all <- subset(W_all, WP == "W")
W_all <- transform(W_all, ID_NAGL = NULL, WP = NULL, gat = NULL, miesiąc = NULL, dzień = NULL, julian = NULL, yday = as.POSIXlt(data)$yday)

W_all <- subset(W_all, (yday > 105) & (yday < 135))
hist(W_all$yday)

W_all <- subset(W_all, ODL < 3)

# dput(names(W_all))
names(W_all) <- c("plot_id", "observer_id", "date", "year", "segment", "distance", "n", "yday")
W_all <- transform(W_all, plot_id = factor(plot_id), observer_id = factor(observer_id), idy = interaction(plot_id, year, sep = "_"))
W_all <- transform(W_all, idy_seg = interaction(idy, segment, sep = "_"))

# Retaining only those which are also present in 'dens'
W_all <- subset(W_all, idy %in% idy)

W_all <- droplevels(W_all)
summary(W_all)

dat <- aggregate(n ~ idy + plot_id + year + date + yday + observer_id + segment + idy_seg, W_all, sum)
summary(dat)
hist(log(dat$n))

save(dat, file = "data/raw_data.RData")

