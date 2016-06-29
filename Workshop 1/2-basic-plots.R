# Load libraries
library("knitr")
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

# Read data, make plots
rates <- read_csv("data/rates.csv")
rates[1:5,1:8]
ggplot(data=rates, aes(x=date, y=AUD)) + geom_point()
ggplot(data=rates, aes(x=date, y=AUD)) + geom_line()
ggplot(data=rates, aes(x=date, y=AUD)) + 
  geom_line() + geom_point()

ggplot(data=rates, aes(x=date, y=AUD)) + geom_line() +
  geom_line(aes(y=NZD), colour="blue") + 
  geom_line(aes(y=GBP), colour="red")
rates.sub <- select(rates, date, AUD, NZD, GBP)
rates.sub.m <- gather(rates.sub, currency, rate, -date)
ggplot(data=rates.sub.m, aes(x=date, y=rate, colour=currency)) + geom_line() 

rates.sub <- mutate(rates.sub, AUD=scale(AUD), NZD=scale(NZD), GBP=scale(GBP))
rates.sub$date <- as.Date(rates.sub$date)
rates.sub.m <- gather(rates.sub, currency, rate, -date)
ggplot(data=rates.sub.m, aes(x=date, y=rate, colour=currency)) +
         geom_line()
ggplot(data=rates.sub, aes(x=AUD, y=NZD, colour=date)) + geom_path() + theme(aspect.ratio=1)

## AUD rate is mapped to position along the x axis
## NZD rate is mapped to position along the y axis
## first differences are mapped to lines
## date is mapped to colour of line, date at beginning of line

ggplot(data=rates.sub, aes(x=AUD, y=NZD)) + 
  geom_point(alpha=0.2) + geom_rug(colour="red", alpha=0.3) + 
  theme(aspect.ratio=1)

# Internet usage
internet <- read_csv("data/internet.csv")
dim(internet)
colnames(internet)
ggplot(data=internet, aes(x=`Social networks`)) + 
  geom_bar(binwidth=0.5) 
ggplot(data=internet, aes(x=`Social networks`)) + 
  geom_bar(binwidth=0.5) +
  facet_grid(Gender~name)
ggplot(data=internet, aes(x=`Social networks`, fill=Gender)) + 
  geom_bar(binwidth=0.5) +
  facet_wrap(~name, ncol=5) + theme(legend.position="bottom")
ggplot(data=internet) + 
  geom_bar(aes(x=`Social networks`, fill=Gender),
          position="dodge") +
  facet_wrap(~name, ncol=5) + 
  theme(legend.position="bottom")
ggplot(data=internet, aes(x=factor(1), fill=factor(`Social networks`))) + 
  geom_bar(width = 1) + scale_x_discrete("") +
  scale_y_continuous("") +
  scale_fill_hue("Social Network Use") +
  coord_polar(theta = "y")

# Grad programs
grad <- read_csv("data/graduate-programs.csv")
dim(grad)
colnames(grad)
ggplot(data=grad, aes(x=subject, y=AvGREs)) + 
  geom_boxplot()
ggplot(data=grad, aes(x=subject, y=AvGREs)) + 
  geom_violin()
df <- data.frame(x=runif(100), y=runif(100), cl=sample(c(rep("A", 1), rep("B", 99))))
qplot(x, y, data=df, shape=cl) + theme_bw() + theme(legend.position="None", aspect.ratio=1)

# Color schemes
display.brewer.all()
ggplot(data=internet, aes(x=`Social networks`, fill=Gender)) + 
  geom_bar(position="dodge") +
  scale_fill_manual(values=c("Female"="orange", "Male"="darkgreen")) + 
  facet_wrap(~name, ncol=5) + 
  theme(legend.position="bottom")

# Scales
ggplot(data=grad, aes(x=subject, y=AvGREs)) + 
  geom_boxplot() + scale_y_log10()
rates.sub.m$date <- as.POSIXct(rates.sub.m$date)
ggplot(data=rates.sub.m, aes(x=date, y=rate, 
  colour=currency)) + geom_line() + 
  scale_x_datetime(breaks = date_breaks("1 month"), 
                   labels = date_format("%b"))
ggplot(data=rates.sub.m, aes(x=date, y=rate, 
        colour=currency)) + geom_line() +
  xlab("Date") + ylab("Standardized rates") + 
  ggtitle("Cross rates 23/2/2015-11/11/2015")
p <- ggplot(data=rates.sub.m, aes(x=date, y=rate, 
  colour=currency)) + geom_line() + 
  scale_x_datetime(breaks = date_breaks("1 month"), 
                   labels = date_format("%b"))
ggplot(data=rates.sub.m, aes(x=date, y=rate, colour=currency)) +
  geom_line() +
  xlab(expression(Date[i]^2~ mu ~ pi * sigma)) + 
  ylab("Standardized rates") + 
  ggtitle("Cross rates 23/2/2015-11/11/2015")

# Themes
p + theme(legend.position = "bottom")
p + theme_tufte()
p + theme_economist()

# Color blindness
p + scale_color_brewer("", palette = "Dark2")
clrs <- hue_pal()(3)
p + scale_color_manual("", values=clrs) + 
  theme(legend.position = "none")
clrs <- dichromat(hue_pal()(3))
p + scale_color_manual("", values=clrs) + 
  theme(legend.position = "none")
clrs <- brewer.pal(3, "Dark2")
p + scale_color_manual("", values=clrs) + 
  theme(legend.position = "none")
clrs <- dichromat(brewer.pal(3, "Dark2"))
p + scale_color_manual("", values=clrs) + 
  theme(legend.position = "none")

# Proximity
ggplot(data=internet, aes(x=`Social networks`)) + 
         geom_bar() + 
  facet_grid(Gender~name)
internet.m.tb <- internet[,c(1,3,8)] %>%
                     group_by(name, Gender, `Social networks`) %>% 
                     tally(sort=TRUE) 
internet.m.tb <- subset(internet.m.tb, !is.na(`Social networks`))
internet.m.tb.n <- summarise(group_by(internet.m.tb, name, Gender), tot=sum(n)) 
internet.m.tb <- merge(internet.m.tb, internet.m.tb.n)
internet.m.tb.p <- summarise(group_by(internet.m.tb, name, Gender, `Social networks`), p=n/tot)
ggplot(data=internet.m.tb.p, aes(x=`Social networks`, y=p, color=Gender)) +
  geom_line() + 
  facet_wrap(~name, ncol=5) + theme(legend.position="bottom")
ggplot(data=internet.m.tb.p, aes(x=`Social networks`, y=p, color=name)) + 
  geom_line() + 
  facet_wrap(~Gender, ncol=2)

## workers <- read_csv(file="data/Assembled_Workers__Compensation_Claims___Beginning_2000.csv")#, n_max=100000)
## dim(workers)
## colnames(workers)
## table(workers$`Claim Type`)
## workers$`District Name` <- factor(workers$`District Name`,
##   levels=names(table(workers$`District Name`)[order(table(workers$`District Name`), decreasing=TRUE)]))
## ggplot(data=workers, aes(x=`District Name`)) + geom_bar()
## ggplot(data=workers, aes(x=`District Name`, fill=Gender)) +
##   geom_bar(position="fill")
## ggplot(data=workers, aes(x=`Claim Type`, y=`Age at Injury`)) +
##   geom_violin()
