
# See: https://becarioprecario.bitbucket.io/spde-gitbook/ch-risk.html#simulating-from-the-gev-and-gp-distributions
library(INLA)

set.seed(1)
n <- 200
loc.data <- matrix(runif(n * 2), n, 2)
mesh <- inla.mesh.2d(loc = loc.data, cutoff = 0.05, 
                     offset = c(0.1, 0.4), max.edge = c(0.05, 0.5))

spde <- inla.spde2.pcmatern(mesh,
                            prior.range = c(0.5, 0.5),
                            prior.sigma = c(0.5, 0.5))
sigma.u <- 2 
range <- 0.7 
Qu <- inla.spde.precision(spde,
                          theta = c(log(range), log(sigma.u)))

m <- 40 # number of replications in time
set.seed(1)
u <- inla.qsample(n = m, Q = Qu, seed = 1)

A <- inla.spde.make.A(mesh = mesh, loc = loc.data)
u <- (A %*% u)

b_0 <- 1 # intercept
b_1 <- 2 # coefficient for covariate
set.seed(1)
covariate <- rnorm(m*n)
lin.pred <- b_0 + b_1 * covariate + as.vector(u)
