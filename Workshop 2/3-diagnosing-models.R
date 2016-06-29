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
library("nortest")

## ----echo=FALSE, results='hide'------------------------------------------
radon_keep <- radon %>% group_by(county) %>% 
  tally() %>% filter(n > 4)
radon_sub <- radon %>% 
  filter(county %in% radon_keep$county & log.radon > -2)
radon_sub$basement <- 
  factor(radon_sub$basement, levels=c(0,1), 
         labels=c("basement", "first floor"))
radon_lmer <- lmer(log.radon ~ basement + uranium + 
  (basement | county.name), data = radon_sub)

## ----fig.show='hide'-----------------------------------------------------
radon_lmer_fit <- radon_sub 
radon_lmer_fit$fit <- fitted(radon_lmer)
radon_lmer_fit$resid1 <-  HLMresid(radon_lmer, 
           level=1)
ggplot(radon_lmer_fit, aes(x=resid1)) + 
  geom_histogram(binwidth=0.5) 

## ----eval=FALSE, echo=FALSE----------------------------------------------
## # p=2 (basement, uranium)
## # q=1 (basement)
## length(unique(radon_sub$county.name)) # g
## radon_sub %>% group_by(county.name) %>% tally()

## ----echo=FALSE----------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=resid1)) + 
  geom_histogram(binwidth=0.5) 

## ----fig.show='hide'-----------------------------------------------------
ggplot_qqnorm(radon_lmer_fit$resid1, line="rlm") +
  theme(aspect.ratio=1)

## ----fig.width=2.8-------------------------------------------------------
ggplot_qqnorm(radon_lmer_fit$resid1, line="rlm") +
  theme(aspect.ratio=1)

## ------------------------------------------------------------------------
radon_lmer_fit %>% group_by(county.name) %>%
  summarise(m = mean(resid1), s = sd(resid1), n = length(resid1)) %>%
  head()

## ----echo=FALSE----------------------------------------------------------
res.sum <- radon_lmer_fit %>% group_by(county.name) %>%
  summarise(m = mean(resid1), s = sd(resid1), n = length(resid1))
ord <- order(res.sum$m)
radon_lmer_fit$county.name <- factor(radon_lmer_fit$county.name, levels=res.sum$county.name[ord])

## ----fig.width=3.5-------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=county.name, y=resid1)) + 
  geom_point(alpha=0.5) + coord_flip()

## ----results='hide'------------------------------------------------------
library("nortest")
ad.test(radon_lmer_fit$resid1)
cvm.test(radon_lmer_fit$resid1)
lillie.test(radon_lmer_fit$resid1)

## ----echo=FALSE----------------------------------------------------------
ad.test(radon_lmer_fit$resid1)

## ------------------------------------------------------------------------
rf <- HLMresid(radon_lmer, level="county.name") 
# same as ranef(radon_lmer)
rf$county.name <- rownames(rf)
rf <- rf %>% rename(resid.basement=`(Intercept)`, 
                    resid.ff=`basementfirst floor`)
radon_lmer_fit <- merge(radon_lmer_fit, rf, 
                        by="county.name")

## ----fig.width=2.2, fig.show='hold'--------------------------------------
ggplot(radon_lmer_fit, aes(x=resid.basement)) + 
  geom_histogram(binwidth=0.05) 
ggplot(radon_lmer_fit, aes(x=resid.ff)) + 
  geom_histogram(binwidth=0.2) 

## ----fig.width=2.2, fig.show='hold'--------------------------------------
ggplot_qqnorm(radon_lmer_fit$resid.basement, line="rlm") +
  theme(aspect.ratio=1)
ggplot_qqnorm(radon_lmer_fit$resid.ff, line="rlm") +
  theme(aspect.ratio=1)

## ------------------------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=resid.basement, y=resid.ff)) + 
  geom_point() + theme(aspect.ratio=1) 

## ----fig.show='hide'-----------------------------------------------------
ggplot(radon_lmer_fit, aes(x=fit, y=log.radon)) + 
  geom_point()  

## ----echo=FALSE----------------------------------------------------------
ggplot(radon_lmer_fit, aes(x=fit, y=log.radon)) + 
  geom_point()  

## ----echo=FALSE----------------------------------------------------------
autism_keep <- autism %>% group_by(childid) %>% 
  tally(sort=TRUE) %>% filter(n>2)
autism_sub <- autism %>% 
  filter(childid %in% autism_keep$childid)

## ----results='hide'------------------------------------------------------
autism_lmer4 <- lmer(vsae ~ age2*sicdegp + 
                       age2*bestest2 + age2*race + 
                       ( age2 - 1 | childid ), 
                    data = autism_sub)
autism_lmer_fit <- augment(autism_lmer4)

## ----fig.show='hide'-----------------------------------------------------
ggplot(autism_lmer_fit, aes(x=age2, y=vsae)) + 
  geom_point(alpha=0.2) +
  geom_line(aes(y=.fitted, group=childid, color=sicdegp)) + 
  facet_grid(race~bestest2)

## ----echo=FALSE----------------------------------------------------------
ggplot(autism_lmer_fit, aes(x=age2, y=vsae)) + 
  geom_point(alpha=0.2) +
  geom_line(aes(y=.fitted, group=childid, color=sicdegp)) + 
  facet_grid(race~bestest2)

## ----fig.show='hide'-----------------------------------------------------
ggplot(autism_lmer_fit, aes(x=age2, y=vsae)) + 
  geom_point(alpha=0.2) +
  geom_line(aes(y=.fixed, color=sicdegp)) + 
  facet_grid(race~bestest2)

## ----echo=FALSE----------------------------------------------------------
ggplot(autism_lmer_fit, aes(x=age2, y=vsae)) + 
  geom_point(alpha=0.2) +
  geom_line(aes(y=.fixed, color=sicdegp)) + 
  facet_grid(race~bestest2)

## ----fig.show='hide'-----------------------------------------------------
ggplot(autism_lmer_fit, aes(x=age2, y=vsae)) + 
  geom_point(alpha=0.2) +
  geom_line(aes(y=.fixed, color=interaction(race,bestest2), linetype=sicdegp)) 

## ----echo=FALSE----------------------------------------------------------
ggplot(autism_lmer_fit, aes(x=age2, y=vsae)) + 
  geom_point(alpha=0.2) +
  geom_line(aes(y=.fixed, color=interaction(race,bestest2), linetype=sicdegp)) 

## ----echo=FALSE, eval=FALSE----------------------------------------------
## autism_lmer_fit$resid1 <- HLMresid(autism_lmer4, level=1)
## ggplot(autism_lmer_fit, aes(x=resid1)) +
##   geom_histogram(binwidth=2.5)

## ------------------------------------------------------------------------
library("mlmRev")
glimpse(Exam)

## ----results='hide'------------------------------------------------------
fm4 <- lmer(normexam ~ standLRT + I(standLRT^2) + 
              I(standLRT^3) + sex + schgend + schavg + 
              (standLRT | school), data = Exam, REML = FALSE)
fm4

## ----fig.show='hide'-----------------------------------------------------
library("HLMdiag")
cooksd_fm4  <- cooks.distance(fm4, group = "school")
dotplot_diag(x = cooksd_fm4, cutoff = "internal",
   name = "cooks.distance") + ylab("Cook s distance") + xlab("school")

## ----echo=FALSE----------------------------------------------------------
dotplot_diag(x = cooksd_fm4, cutoff = "internal",
   name = "cooks.distance") + ylab("Cook s distance") + 
  xlab("school")

## ----out.width=7---------------------------------------------------------
mdffits_fm4 <- mdffits(fm4, group = "school")
sort(mdffits_fm4)

## ------------------------------------------------------------------------
leverage_fm4 <- leverage(fm4, level = "school")
head(leverage_fm4)

