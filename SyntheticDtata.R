setwd("C:/Users/Kankana Ghosh/Dropbox/PC/Desktop/Immersion/No1")

library(synthpop) 
library(tidyverse) 
library(cowplot)

h_dat <- read_csv("susa.csv")

head(h_dat)
summary(h_dat)

#h_dat_s <- syn(h_dat, m = 1, seed = 1969)

#compare(h_dat_s, h_dat)

h_dat <-  h_dat %>% select(-Client)
h_dat <-  h_dat %>% select(-imm_median)
h_dat <-  h_dat %>% select(-imm_sd)
h_dat <-  h_dat %>% select(-frustration)

h_dat_syn <- syn(h_dat, method = "parametric",minnumlevels =1, m = 1, k=10000, visit.sequence = c(1:11), seed = 1969,  drop.not.used = TRUE)
compare(h_dat_syn, h_dat)
comparison_result <- compare(h_dat_syn, h_dat)

summary(h_dat_syn)

compare(h_dat_syn, h_dat, vars = "imm_mean")

compare(h_dat_syn, h_dat, vars = "peak")

compare(h_dat_syn, h_dat, vars = "time")

compare(h_dat_syn, h_dat, vars = "purchase")

compare(h_dat_syn, h_dat, vars = "loyalty")

compare(h_dat_syn, h_dat, vars = "age")

pMSE_values <- c(0, 0, 0, 0, 0, 1e-06, 0, 0, 0, 0, 1e-06)

# Calculate mean and variance
mean_S_pMSE <- mean(pMSE_values)
sd_S_pMSE <- sd(pMSE_values)

# Print the results
cat("Mean of S_pMSE:", mean_S_pMSE, "\n")
cat("SD of S_pMSE:", sd_S_pMSE, "\n")

d = dim(h_dat_syn)
head(h_dat_syn)
write.syn(h_dat_syn,file = "susa1_syn", filetype = "csv")

#h_dat_syn <- syn(h_dat, method = "parametric",minnumlevels =1, m = 1, k=10000, visit.sequence = (11:ncol(h_dat)), seed = 1969)
#compare(h_dat_syn, h_dat)
write.syn(h_dat_syn,file = "susa_syn", filetype = "csv")

susa_syn<- read_csv("susa_syn.csv")

plot(susa_syn$peak, susa_syn$time, main = " peak vs time", xlab = "peak", ylab = "time")
abline(lm(susa_syn$time ~ susa_syn$peak, data = susa_syn), col = "red")


x_dat <- read_csv("xerxes.csv")

head(x_dat)
summary(x_dat)

x_dat <-  x_dat %>% select(-Client)
x_dat <-  x_dat %>% select(-imm_median)
x_dat <-  x_dat %>% select(-imm_sd)
x_dat <-  x_dat %>% select(-frustration)

x_dat_syn <- syn(x_dat, method = "parametric",minnumlevels =1, m = 1, k=10000, visit.sequence = c(1:11), seed = 1969,  drop.not.used = TRUE)
#x_dat_syn <- syn(x_dat, m = 1, k=10000, seed = 1969)
compare(x_dat_syn, x_dat)

summary(x_dat_syn)

compare(x_dat_syn, x_dat, vars = "imm_mean")

compare(x_dat_syn, x_dat, vars = "peak")

compare(x_dat_syn, x_dat, vars = "time")

compare(x_dat_syn, x_dat, vars = "purchase")

compare(x_dat_syn, x_dat, vars = "loyalty")

pMSE_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1e-06)

# Calculate mean and variance
mean_S_pMSE <- mean(pMSE_values)
sd_S_pMSE <- sd(pMSE_values)

# Print the results
cat("Mean of S_pMSE:", mean_S_pMSE, "\n")
cat("SD of S_pMSE:", sd_S_pMSE, "\n")

write.syn(x_dat_syn,file = "xerxes_syn", filetype = "csv")

xerxes_syn<- read_csv("xerxes_syn.csv")

plot(xerxes_syn$time, xerxes_syn$purchase, main = " peak vs time", xlab = "peak", ylab = "time")
abline(lm(xerxes_syn$purchase ~ xerxes_syn$time, data = xerxes_syn), col = "red")
