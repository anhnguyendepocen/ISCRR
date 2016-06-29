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
library("tidyr")
library("ggplot2")
library("ggthemes")
library("gridExtra")
library("dplyr")
library("lubridate")
library("GGally")
library("rworldmap")
library("ggmap")
library("scales")
library("dichromat")
library("RColorBrewer")
library("viridis")
library("purrr")
library("broom")
library("timeDate")
library("haven")
library("boot")
library("lme4")
library("nlme")
library("HLMdiag")
library("mlmRev")

## ----eval=FALSE----------------------------------------------------------
## library(HLMdiag)
## ?radon
## library(mlmRev)
## ?Gcsemv

## ------------------------------------------------------------------------
ggplot(radon, aes(x=uranium, y=log.radon)) + geom_point()

## ------------------------------------------------------------------------
radon_keep <- radon %>% group_by(county) %>% 
  tally() %>% filter(n > 4)
radon_sub <- radon %>% 
  filter(county %in% radon_keep$county & log.radon > -2)
radon_sub$basement <- 
  factor(radon_sub$basement, levels=c(0,1), 
         labels=c("basement", "first floor"))

## ------------------------------------------------------------------------
ggplot(radon_sub, aes(x=uranium, y=log.radon)) + 
  geom_point() +
  geom_smooth(method="lm", se=F) +
  facet_wrap(~basement)

## ----results='hide'------------------------------------------------------
radon_lm <- glm(log.radon ~ basement + uranium, 
                data = radon_sub)
summary(radon_lm)

## ----echo=FALSE, eval=FALSE----------------------------------------------
## radon_lm_fit <- radon_sub; radon_lm_fit$fit <- fitted(radon_lm)
## ggplot(radon_lm_fit, aes(x=uranium, y=log.radon, colour=basement)) + geom_point() +
##   geom_line(aes(y=fit))

## ----eval=FALSE, echo=FALSE----------------------------------------------
## radon_lm <- glm(log.radon ~ basement*uranium, data = radon_sub)
## summary(radon_lm)
## radon_lm_fit <- radon_sub; radon_lm_fit$fit <- fitted(radon_lm)
## ggplot(radon_lm_fit, aes(x=uranium, y=log.radon, colour=basement)) + geom_point() +
##   geom_line(aes(y=fit))

## ----results='hide'------------------------------------------------------
radon_lmer <- lmer(log.radon ~ basement + uranium + 
  (basement | county.name), data = radon_sub)
summary(radon_lmer)

## ----fig.show='hide'-----------------------------------------------------
radon_lmer_fit <- radon_sub 
radon_lmer_fit$fit <- fitted(radon_lmer)
ggplot(radon_lmer_fit, aes(x=uranium, y=log.radon)) + 
  geom_point(alpha=0.2) + 
  geom_point(aes(y=fit, colour=county.name)) + 
  facet_wrap(~basement) + theme(legend.position="none")

## ----echo=FALSE----------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=uranium, y=log.radon)) + 
  geom_point(alpha=0.2) + 
  geom_point(aes(y=fit, colour=county.name)) + 
  facet_wrap(~basement) + theme(legend.position="none")

## ------------------------------------------------------------------------
ggplot(filter(autism, 
              childid %in% sample(unique(childid), 5)), 
       aes(x=age2, y=vsae, group=childid)) + geom_line()

## ------------------------------------------------------------------------
ggplot(autism, aes(x=factor(age2), y=vsae, colour=bestest2)) + 
  geom_boxplot()

## ------------------------------------------------------------------------
ggplot(autism, aes(x=age2, y=vsae, group=childid, colour=sicdegp)) + 
  geom_line(alpha=0.2) + 
  geom_smooth(aes(group=sicdegp), se=F, size=2)

## ------------------------------------------------------------------------
ggplot(autism, aes(x=age2, y=vsae, group=childid, colour=sicdegp)) + 
  geom_line(alpha=0.2) + facet_grid(gender~race) +
  geom_smooth(aes(group=sicdegp), se=F, size=2)

## ------------------------------------------------------------------------
autism %>% filter(age2==0) %>% group_by(gender, race) %>% 
  tally() %>% spread(race, n)

## ----echo=FALSE, eval=FALSE----------------------------------------------
## autism %>% group_by(childid) %>%
##   tally(sort=TRUE) %>% filter(n<3)

## ------------------------------------------------------------------------
autism_keep <- autism %>% group_by(childid) %>% 
  tally(sort=TRUE) %>% filter(n>2)
autism_sub <- autism %>% 
  filter(childid %in% autism_keep$childid)

## ----results='hide'------------------------------------------------------
autism_lmer <- lmer(vsae ~ age2 + ( age2 - 1 | childid ), 
                    data = autism_sub)
summary(autism_lmer)

## ----fig.show='hide'-----------------------------------------------------
autism_lmer_fit <- autism_sub 
autism_lmer_fit$fit <- fitted(autism_lmer)
ggplot(autism_lmer_fit, aes(x=age2, y=vsae)) + 
  geom_point(alpha=0.2) + 
  geom_line(aes(y=fit, group=childid)) 

## ----echo=FALSE----------------------------------------------------------
ggplot(autism_lmer_fit, aes(x=age2, y=vsae)) + 
  geom_point(alpha=0.2) + 
  geom_line(aes(y=fit, group=childid)) 

## ----fig.show='hide'-----------------------------------------------------
ggplot(autism_lmer_fit, aes(x=vsae, y=fit, group=childid)) + 
  geom_abline(intercept=0, slope=1, color="red", size=2) + 
  geom_point(alpha=0.2) + geom_line(alpha=0.5) + 
  xlim(c(0, 200)) + ylim(c(0, 200)) +
  theme(aspect.ratio=1) 

## ----echo=FALSE----------------------------------------------------------
ggplot(autism_lmer_fit, aes(x=vsae, y=fit, group=childid)) + 
  geom_abline(intercept=0, slope=1, color="red", size=2) + 
  geom_point(alpha=0.2) + geom_line(alpha=0.5) + 
  xlim(c(0, 200)) + ylim(c(0, 200)) +
  theme(aspect.ratio=1) 

## ----results='hide'------------------------------------------------------
autism_lmer2 <- lmer(vsae ~ age2*sicdegp + ( age2 - 1 | childid ), 
                    data = autism_sub)
summary(autism_lmer2)

## ----fig.show='hide'-----------------------------------------------------
autism_lmer2_fit <- autism_sub 
autism_lmer2_fit$fit <- fitted(autism_lmer2)
ggplot(autism_lmer2_fit, aes(x=age2, y=vsae, colour=sicdegp)) + 
  geom_point(alpha=0.2) + facet_wrap(~sicdegp) +
  geom_line(aes(y=fit, group=childid)) 

## ----echo=FALSE----------------------------------------------------------
ggplot(autism_lmer2_fit, aes(x=age2, y=vsae, colour=sicdegp)) + 
  geom_point(alpha=0.2) + facet_wrap(~sicdegp) +
  geom_line(aes(y=fit, group=childid)) 

## ----fig.show='hide'-----------------------------------------------------
autism_lmer2_fit <- augment(autism_lmer2)
ggplot(autism_lmer2_fit, aes(x=age2, y=vsae, colour=sicdegp)) + 
  geom_jitter(alpha=0.2) + 
  geom_line(aes(y=.fixed, group=childid)) 

## ----echo=FALSE----------------------------------------------------------
ggplot(autism_lmer2_fit, aes(x=age2, y=vsae, colour=sicdegp)) + 
  geom_jitter(alpha=0.2) + 
  geom_line(aes(y=.fixed, group=childid)) 

## ------------------------------------------------------------------------
anova(autism_lmer, autism_lmer2)

