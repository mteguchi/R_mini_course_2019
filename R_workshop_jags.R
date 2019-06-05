# Example script for R workshop for the turtle groups

rm(list=ls())
library(jagsUI)
library(bayesplot)
library(ggplot2)

set.seed(10)
x <- runif(n = 50, 
           min = 44, 
           max = 115)

set.seed(10)
y <- 0.96 + 0.04 * x + rnorm(n=length(x), 0, 0.1)

mass <- exp(y)
df1 <- data.frame(length = x, mass = mass)
summary(df1)

jags.parameters <- c("a", "b", "s") #, "deviance", "log.likelihood")

jags.data <- list(n = nrow(df1),
                  x = df1$length,
                  y = log(df1$mass))

model.file <- "models/model_linearRegression.txt"


MCMC.n.chains <- 5
MCMC.n.samples <- 500000
MCMC.n.burnin <- 350000
MCMC.n.thin <- 50

# MCMC.params <- list(n.chains = 3,
#                     n.samples = 50000,
#                     n.burnin = 10000,
#                     n.thin = 2)

# initialize the model with all other stuff

jm <- jags(data = jags.data,
           inits = NULL,
           parameters.to.save= jags.parameters,
           model.file = model.file,
           n.chains = MCMC.n.chains,
           n.burnin = MCMC.n.burnin,
           n.thin = MCMC.n.thin,
           n.iter = MCMC.n.samples,
           DIC = T, parallel=T)

# Pareto-k statistics to see how good the model is... should be good and this is
# more than you need to know at the moment...
# library(loo)
# n.per.chain <- (MCMC.n.samples - MCMC.n.burnin)/MCMC.n.thin
# loglik.obs <- jm$sims.list$log.likelihood
# Reff <- relative_eff(exp(loglik.obs),
#                      chain_id = rep(1:MCMC.n.chains, 
#                                     each = n.per.chain))
# 
# loo.out <- loo(loglik.obs, r_eff = Reff)

jm

# look at trace and posteriors
mcmc_trace(jm$samples, c("a", "b"))

mcmc_dens(jm$samples, c("a", "b"))


lm1 <- lm(log(mass) ~ length, data = df1)

summary(lm1)
df1$predict <- predict(lm1)


#plot(x, y)
p1 <- ggplot(data = df1, aes(x = length)) +
  geom_point(aes(y = log(mass)),
             colour = "red",
             size = 3) +
  geom_line(aes(y = predict), colour = "black") +
  theme(axis.text = element_text(size = 12)) +
  xlab("Length (cm)") +
  ylab("ln(Mass) (kg)")

print(p1)
