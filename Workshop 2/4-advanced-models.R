## ----setup, include = FALSE----------------------------------------------
library("knitr")
opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  cache = FALSE,
  fig.height = 3,
  fig.width = 6,
  fig.caption = FALSE,
  collapse = TRUE,
  comment = "#"
)
options(digits=2)
library("rmarkdown")
library("devtools")
library("readr")
library("GGally")
library("tidyr")
library("dplyr")
library("purrr")
library("broom")
library("lme4")
library("nlme")
library("HLMdiag")
library("mlmRev")

## ------------------------------------------------------------------------
random_group <- function(n, probs) {
  probs <- probs / sum(probs)
  g <- findInterval(seq(0, 1, length = n), c(0, cumsum(probs)),
    rightmost.closed = TRUE)
  names(probs)[sample(g)]
}

## ------------------------------------------------------------------------
partition <- function(df, n, probs) {
  replicate(n, split(df, random_group(nrow(df), 
                                      probs)), FALSE) %>%
    transpose() %>%
    as_data_frame()
}
boot <- partition(mtcars, 100, c(training = 0.8, test = 0.2))

## ------------------------------------------------------------------------
msd <- function(x, y) sqrt(mean((x - y) ^ 2))

## ------------------------------------------------------------------------
boot <- boot %>% mutate(
  # Fit the models
  models = map(training, ~ lm(mpg ~ wt, data = .)),
  # Make predictions on test data
  preds = map2(models, test, predict),
  diffs = map2(preds, test %>% map("mpg"), msd)
)

unlist(boot$diffs)

## ----echo=FALSE, results='hide', fig.show='hide'-------------------------
library(ggplot2)
library(lme4)     # for modeling
library(HLMdiag)  # for residuals
library(nullabor) # for lineups
library(plyr)
library(reshape2)
library(stringr)
library(mvtnorm)
radon_keep <- radon %>% group_by(county) %>% 
  tally() %>% filter(n > 4)
radon_sub <- radon %>% 
  filter(county %in% radon_keep$county & log.radon > -2)
radon_sub$basement <- 
  factor(radon_sub$basement, levels=c(0,1), 
         labels=c("basement", "first floor"))
BlockZ <- function(object) {
  Z <- getME(object, "Z")
  
  grp.size <- table(object@flist)
  ngrps <- length(grp.size)
  nranef <- dim(ranef(object)[[1]])[2]
  
  base.ord <- seq(from = 1, by = ngrps, length.out = nranef)
  ord <- base.ord + rep(0:(ngrps - 1), each = nranef)
  
  perm.mat <- t(as(ord, "pMatrix"))
  
  return(Z %*% perm.mat)
}

lev2.marginal.var <- function(.model) {
  y <- .model@y
  X <- getME(.model, "X")
  Z <- BlockZ(.model)
  n <- nrow(X)
  ngrps <- unname(sapply(.model@flist, function(x) length(levels(x))))
  
  # Constructing V = Cov(Y)
  sig0 <- attr(VarCorr(.model), "sc") # sigma(.model)
  
  ZDZt <- sig0^2 * crossprod( .model@A )
  R    <- Diagonal( n = n, x = sig0^2 )
  D    <- kronecker( Diagonal(ngrps), bdiag(VarCorr(.model)) )
  V    <- Diagonal(n) + ZDZt
  
  # Inverting V
  V.chol <- chol( V )
  Vinv   <- chol2inv( V.chol )

  bse <- crossprod( chol(Vinv) %*% Z %*% D ) # Marginal COV. used by Lange and Ryan
  bse.diag <- diag(bse)

  semat <- matrix(sqrt(bse.diag), ncol = 2, byrow = TRUE)

  return(semat)
}

std_ranef <- function(.model) {
	res <- ranef(.model)[[1]]
	semat <- lev2.marginal.var(.model)
	
	RVAL <- res / semat
	return(RVAL)
}

sim_env <- function(x, conf = .95){
  n <- length(x)
  P <- ppoints(x)
  z <- qnorm(P)
  a <- as.numeric(HLMdiag:::qqlineInfo(x)[1])
  b <- as.numeric(HLMdiag:::qqlineInfo(x)[2])
  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (b/dnorm(z)) * sqrt(P * (1 - P)/n)
  fit.value <- a + b * z
  upper <- fit.value + zz * SE
  lower <- fit.value - zz * SE
  return(data.frame(lower, upper))
}

sim_t_hlm <- function(.mod) {
	vc <- VarCorr( .mod )
	D  <- as.matrix( bdiag(vc) )
	sig.e <- sigma(.mod)
	
	dims <- .mod@dims
	n <- dims[["n"]]
	m <- dims[["q"]] / nrow(D)

	## normal errors
	e  <- rnorm(n = n, mean = 0, sd = sig.e)

	## mutlivariate t random effects
	b <- rmvt(n = m, sigma = D, df = 3)
	
	## Generating y
	bvec <- c(b[,1], b[,2])
	y <- getME(.mod, "X") %*% fixef(.mod) + getME(.mod, "Z") %*% bvec + e
	
	return( as.numeric(y) )
}

sim_indep_ranef_hlm <- function(.mod, nsim, e.dsn, b0.dsn, b1.dsn, sigma.err, sigma.b0, sigma.b1){
  vc <- VarCorr( .mod )
	
	dims <- .mod@dims
	n <- dims[["n"]]
	m <- dims[["q"]] / dims[["nt"]]
	
	## Simulating error terms
	if(e.dsn == "norm") {
		e  <- rnorm(n = nsim * n, mean = 0, sd = sigma.err)
	} 
	if(e.dsn == "t") {
		e  <- (sigma.err / sqrt(3)) * rt(n = nsim * n, df = 3)
	}
	if(e.dsn == "exp") {
		e  <- sigma.err * ( rexp(n = nsim * n) - 1 )
	}
	e <- matrix(e, nc = nsim)
	
	## Simulating random intercept
	if(b0.dsn == "norm") {
		b0  <- rnorm(n = nsim * m, mean = 0, sd = sigma.b0)
	} 
	if(b0.dsn == "t") {
		b0  <- (sigma.b0 / sqrt(3)) * rt(n = nsim * m, df = 3)
	}
	if(b0.dsn == "exp") {
		b0  <- sigma.b0 * ( rexp(n = nsim * m) - 1 )
	}
	b0 <- matrix(b0, nc = nsim)

	## Simulating random slope
	if(b1.dsn == "norm") {
		b1  <- rnorm(n = nsim * m, mean = 0, sd = sigma.b1)
	} 
	if(b1.dsn == "t") {
		b1  <- (sigma.b1 / sqrt(3)) * rt(n = nsim * m, df = 3)
	}
	if(b1.dsn == "exp") {
		b1  <- sigma.b1 * ( rexp(n = nsim * m) - 1 )
	}
	b1 <- matrix(b1, nc = nsim)
	
	## Generating y
	b <- rbind(b0, b1)
	y <- getME(.mod, "X") %*% fixef(.mod) + getME(.mod, "Z") %*% b + e
	
	y.df <- as.data.frame( as.matrix( y) )
	colnames(y.df) <- paste("sim_", 1:ncol(y.df), sep = "")
	
	return( y.df )

}
fm <- lmer(log.radon ~ basement + uranium + (basement | county), data = radon)

b <- ranef(fm)[[1]] 

sim.y   <- simulate(fm, nsim = 19, seed = 987654321)                        
sim.mod <- apply(sim.y, 2, refit, object = fm)            

sim.b0 <- llply(sim.mod, function(x) ranef(x)[[1]][,1])   
sim.b0 <- melt( do.call("rbind", sim.b0) )[,-2]           
names(sim.b0) <- c("sample", "(Intercept)")                 
sim.b0        <- arrange(sim.b0, sample)                  

sim.b0$.n <- as.numeric( str_extract(sim.b0$sample, "\\d+") )
sim.b0 <- ddply(sim.b0[complete.cases(sim.b0),], .(.n), transform, band = sim_env(`(Intercept)`), 
	x = sort(qqnorm(`(Intercept)`, plot.it=FALSE)$x))

b0 <- transform(b, band = sim_env(`(Intercept)`), 
	x = sort(qqnorm(`(Intercept)`, plot.it=FALSE)$x))

## ----results='hide', fig.show='hide'-------------------------------------
# Lineup of random intercepts
qplot(sample = X.Intercept., data = b0) %+%
	lineup(true = b0, sample = sim.b0, pos=5) + 
	facet_wrap(~ .sample, ncol = 5) + 
	geom_ribbon(aes(x = x, ymin = band.lower, ymax = band.upper), alpha = .25) + 
	xlab("Normal Quantiles") + ylab("Sample Quantiles")


## ----echo=FALSE, fig.width=8, fig.height=8-------------------------------
qplot(sample = X.Intercept., data = b0) %+%
	lineup(true = b0, sample = sim.b0, pos=5) + 
	facet_wrap(~ .sample, ncol = 5) + 
	geom_ribbon(aes(x = x, ymin = band.lower, ymax = band.upper), alpha = .25) + 
	xlab("Normal Quantiles") + ylab("Sample Quantiles")

## ----fig.width=4, fig.height=2.5-----------------------------------------
ggplot(Theoph, aes(x=Time, y=conc, group=Subject)) + 
  geom_line() 

## ------------------------------------------------------------------------
library("gapminder")
gap_lmer <- lmer(lifeExp ~ year + (1 | country), 
                 data=gapminder)
gap_lmer_fit <- augment(gap_lmer)

