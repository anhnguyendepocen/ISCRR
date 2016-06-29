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

## ------------------------------------------------------------------------
library("gapminder")
glimpse(gapminder)

## ------------------------------------------------------------------------
ggplot(data=gapminder, aes(x=year, y=lifeExp, group=country)) +
  geom_line(alpha=0.5)

## ------------------------------------------------------------------------
gapminder2 <- gapminder %>% mutate(year1950 = year-1950)
by_country <- gapminder2 %>% 
  select(country, year1950, lifeExp, continent) %>%
  group_by(country, continent) %>% 
  nest()

## ----echo=F--------------------------------------------------------------
kable(gapminder2[,c(1, 2, 7, 4)])

## ------------------------------------------------------------------------
by_country <- by_country %>% 
  mutate(
    model = purrr::map(data, ~ lm(lifeExp ~ year1950, 
                                  data = .))
  )

## ------------------------------------------------------------------------
by_country <- by_country %>% 
  unnest(model %>% purrr::map(broom::tidy))
kable(head(by_country))

## ------------------------------------------------------------------------
country_coefs <- by_country %>% 
  select(continent, country, term, estimate) %>% 
  spread(term, estimate)
kable(head(country_coefs))

## ----fig.show='hide'-----------------------------------------------------
ggplot(data=country_coefs, aes(x=`(Intercept)`, y=year1950, 
                               colour=continent)) +
  geom_hline(yintercept=0, colour="grey60") +
  geom_point(alpha=0.7, size=4) + 
  scale_colour_brewer(palette="Dark2") + 
  xlab("Life Expectancy in 1950") +
  ylab("Gain in Life Expectancy per Year")

## ----echo=FALSE----------------------------------------------------------
ggplot(data=country_coefs, aes(x=`(Intercept)`, y=year1950, 
                               colour=continent)) + 
  geom_hline(yintercept=0, colour="grey60") +
  geom_point(alpha=0.7, size=4) + 
  scale_colour_brewer(palette="Dark2") + 
  xlab("Life Expectancy in 1950") +
  ylab("Gain in Life Expectancy per Year") 

## ------------------------------------------------------------------------
oz <- gapminder %>% filter(country=="Australia")
kable(head(oz))

## ------------------------------------------------------------------------
ggplot(data=oz, aes(x=year, y=lifeExp)) + 
  geom_point() + 
  geom_smooth(method="lm")

## ------------------------------------------------------------------------
oz.lm <- lm(lifeExp~year, data=oz)
oz.lm

## ------------------------------------------------------------------------
oz <- oz %>% mutate(year = year-1950)
oz.lm <- lm(lifeExp~year, data=oz)
oz.lm

## ------------------------------------------------------------------------
by_country <- gapminder2 %>% 
  group_by(continent, country) %>% 
  nest()
head(by_country)

## ------------------------------------------------------------------------
head(by_country$data[[1]])

## ------------------------------------------------------------------------
lm(lifeExp~year1950, data=by_country$data[[1]])

## ------------------------------------------------------------------------
by_country$model <- by_country$data %>% 
  purrr::map(~lm(lifeExp~year1950, data=.))
head(by_country)

## ------------------------------------------------------------------------
broom::glance(by_country$model[[1]])
broom::tidy(by_country$model[[1]])

## ------------------------------------------------------------------------
broom::augment(by_country$model[[1]])

## ------------------------------------------------------------------------
by_country_coefs <- by_country %>% 
  unnest(model %>% purrr::map(broom::tidy))
coefs <- by_country_coefs %>% 
  select(country, continent, term, estimate) %>% 
  spread(term, estimate)

## ------------------------------------------------------------------------
ggplot(data=coefs, aes(x=`(Intercept)`, y=year1950, 
                       colour=continent)) +
  geom_point()

## ----fig.show='hide'-----------------------------------------------------
by_country <- by_country %>% 
  unnest(model %>% 
           purrr::map(broom::glance))
by_country$country <- reorder(by_country$country, 
                              -by_country$r.squared)
ggplot(data=by_country, aes(x=country, y=r.squared, 
                            colour=continent)) +
  geom_point() +
  coord_flip() +
  scale_colour_brewer(palette="Dark2")

## ----echo=FALSE, fig.height=8, fig.width=6-------------------------------
ggplot(data=by_country, aes(x=country, y=r.squared, colour=continent)) +
  geom_point() +
  coord_flip() +
  scale_colour_brewer(palette="Dark2")

## ----fig.show='hide'-----------------------------------------------------
country_all <- by_country %>% 
  unnest(data)
ggplot(data=subset(country_all, r.squared <= 0.45), 
       aes(x=year, y=lifeExp)) + 
         geom_line() +
  facet_wrap(~country)

## ----echo=FALSE----------------------------------------------------------
ggplot(data=subset(country_all, r.squared <= 0.45), 
       aes(x=year, y=lifeExp)) + 
         geom_line() +
  scale_x_continuous(breaks=seq(1950,2000,10), labels=c("1950", "60","70", "80","90","2000")) + 
  facet_wrap(~country)

## ----echo=FALSE----------------------------------------------------------
ggplot(data=subset(country_all, between(r.squared, 0.45, 0.75)), 
       aes(x=year, y=lifeExp)) + 
         geom_line() +
  scale_x_continuous(breaks=seq(1950,2000,10), labels=c("1950", "60","70", "80","90","2000")) + 
  facet_wrap(~country)

