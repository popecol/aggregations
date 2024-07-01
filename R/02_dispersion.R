
# Data preparation.
# Author: lechu@amu.edu.pl


library(pbapply)

source("R/setup.R")

idx <- combinat::permn(10)


# Tests -------------------------------------------------------------------

n1 <- c(0, 0, 0, 2, 2, 0, 0, 0, 0, 0)
plot_obs(n1)

disp_max <- max_rsd(sum(n1), idx)

system.time(disp_1 <- rsd(n1))
system.time(disp_2 <- rsd(n1, idx))
system.time(disp_3 <- rsd(n1, idx, disp_max))
c(disp_1, disp_2, disp_3)


# Pre-calculating the look-up table ---------------------------------------

load("data/raw_data.RData")
n <- aggregate(n ~ idy, dat, sum)
n <- sort(unique(n$n)); n <- n[-1]
n

mrsd <- pbsapply(n, \(x) max_rsd(x, idx, "none"))
tab <- data.frame(n = n, max_rsd = mrsd)

# save(tab, file = "data/tab.RData")
load("data/tab.RData")


FUN <- rsd

disp <- function(df, FUN, ...) {
  # Helper function calculating dispersion measures for every combination of 'plot_id' and 'year'.
  
  # Arguments:
  #  df: data.frame containing variables: idy, plot_id, year, date, yday, observer_id, n
  # FUN: the function calculating index of dispersion
  # Returns: a data.frame with above mentioned variables, distribution of observations across segments (n_1:n_10), and the variable 'dispersion'.
  
  # if(!is.character(FUN)) stop('FUN must be of type "character".')
  # fn <- match.fun(FUN)
  
  yr <- sort(unique(df$year))
  nd <- expand.grid(segment = 1:10, year = yr)
  nd$plot_id <- df$plot_id[1]
  nd <- transform(nd, idy = interaction(plot_id, year, sep = "_"))
  nd <- transform(nd, idy_seg = interaction(idy, segment, sep = "_"))
  nd <- merge(nd, df[c("idy_seg", "n")], by = "idy_seg", all.x = TRUE)
  nd$n[is.na(nd$n)] <- 0
  n_by_segment <- aggregate(n ~ idy, nd, I)
  n_by_segment <- data.frame(idy = n_by_segment$idy, n_by_segment$n)
  names(n_by_segment)[-1] <- paste("n", 1:10, sep = "_")
  disp_by_idy <- aggregate(n ~ idy, nd, FUN, idx = idx, disp_max = tab)
  names(disp_by_idy)[2] <- "dispersion"
  n_by_idy <- aggregate(n ~ idy + plot_id + year + date + yday + observer_id, df, sum)
  merge(merge(n_by_idy, n_by_segment), disp_by_idy)
}

# Test
(df <- subset(dat, plot_id == "MW14"))
(df <- subset(dat, plot_id == "DS09"))
disp(df, rsd)


# Calculating indexes -----------------------------------------------------

dat_splitted <- split(dat, dat$plot_id, drop = TRUE)
dat_splitted <- lapply(dat_splitted, disp, rsd)

dat <- do.call(rbind.data.frame, dat_splitted)
dat <- droplevels(dat)
dat <- transform(dat, aggregation = 1 - dispersion)

summary(dat)
hist(dat$dispersion, breaks = 30)
hist(dat$aggregation, breaks = 30)
plot(aggregation ~ log(n), dat)


# Merging -----------------------------------------------------------------

# Density data
load("../MPPL_data/W/data/data.RData")
dens <- data; rm(data)
dens <- transform(dens, idy = interaction(id_pow, year, sep = "_"))

# dput(names(dat))
v <- c("idy", "date", "yday", "n", "dispersion", "aggregation")
data <- merge(dens, dat[v], by = "idy")
summary(data)

save(data, file = "data/data.RData")

