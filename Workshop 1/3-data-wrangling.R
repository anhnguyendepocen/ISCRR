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

# Read workers data
workers <- read_csv(file="data/Assembled_Workers__Compensation_Claims___Beginning_2000.csv")
# Instead of table(workers$Gender)
workers %>%
  group_by(Gender) %>%
  tally()
# Instead of
# mean(workers$`Birth Year`, na.rm=TRUE)
# sd(workers$`Birth Year`, na.rm=TRUE)
workers %>%
  group_by(Gender) %>%
  filter(Gender %in% c("F", "M")) %>%
  summarise(m=mean(`Birth Year`, na.rm=TRUE),
            s=sd(`Birth Year`, na.rm=TRUE))

# Grad programs
grad <- read_csv("data/graduate-programs.csv")
head(grad[c(2,3,4,6)])

# Genes
genes <- read_csv("data/genes.csv")
genes

# Temperature
melbtemp <- read.fwf("data/ASN00086282.dly",
   c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)
head(melbtemp[,c(1,2,3,4,seq(5,128,4))])

# TB
tb <- read_csv("data/tb.csv")
#tail(tb)
colnames(tb)

# Survey
pew <- read.delim(
  file = "http://stat405.had.co.nz/data/pew.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = F
)
pew[1:5, 1:5]

# French fries
data(french_fries, package = "reshape2")
head(french_fries)
ff_long <- gather(french_fries, key = variable, value =
                    rating, potato:painty)
head(ff_long)
french_fries_wide <- spread(ff_long, key = variable,
                            value = rating)

head(french_fries_wide)
ff.m <- french_fries %>%
  gather(type, rating, -subject, -time, -treatment, -rep)
head(ff.m)
ggplot(data=ff.m, aes(x=rating)) + geom_histogram(binwidth=2) +
  facet_wrap(~type, ncol=5)
ggplot(data=ff.m, aes(x=type, y=rating, fill=type)) +
  geom_boxplot()
head(ff.m)
ff.s <- ff.m %>% spread(rep, rating)
head(ff.s)
ggplot(data=ff.s, aes(x=`1`, y=`2`)) + geom_point() +
  theme(aspect.ratio=1) + xlab("Rep 1") + ylab("Rep 2")
ggplot(data=ff.s, aes(x=`1`, y=`2`)) + geom_point() +
  theme(aspect.ratio=1) + xlab("Rep 1") + ylab("Rep 2") +
  scale_x_sqrt() + scale_y_sqrt()
## ggplot(data=ff.s, aes(x=`1`, y=`2`)) + geom_point() +
##   theme(aspect.ratio=1) +
##   xlab("Rep 1") + ylab("Rep 2") + facet_wrap(~type, ncol=5)
## ggplot(data=ff.s, aes(x=`1`, y=`2`)) + geom_point() +
##   theme(aspect.ratio=1) +
##   xlab("Rep 1") + ylab("Rep 2") + facet_grid(treatment~type)

# Billboard data
billboard <- read.csv("data/billboard.csv")
long_billboard <- gather(billboard, key = week, value = rank, X1:X76)
long_billboard$week <- as.numeric(gsub("X", "", long_billboard$week))

ggplot(data = long_billboard, aes(x=week, y=rank, colour = artist, group = track)) + geom_line() + theme(legend.position="bottom")

# Back to french fries
french_fries_split <- group_by(ff_long, variable) # SPLIT
french_fries_apply <- summarise(french_fries_split,
     rating = mean(rating, na.rm = TRUE)) # APPLY + COMBINE
french_fries_apply
french_fries %>%
    filter(subject == 3, time == 1)
french_fries %>%
    arrange(desc(rancid)) %>%
    head
french_fries %>%
    select(time, treatment, subject, rep, potato) %>%
    head
french_fries %>%
    group_by(time, treatment) %>%
    summarise(mean_rancid = mean(rancid),
              sd_rancid = sd(rancid))
french_fries %>%
  select(subject, time, treatment) %>%
  tbl_df() %>%
  count(subject, time) %>%
  spread(time, n) %>% head
french_fries %>%
  select(subject, time, treatment) %>%
  filter(subject==31)
french_fries %>%
  gather(type, rating, -subject, -time, -treatment, -rep) %>%
  select(subject, time, treatment, type) %>%
  tbl_df() %>%
  count(subject, time) %>%
  spread(time, n) %>% head
ggplot(data=ff.m, aes(time, rating, colour=treatment)) +
  geom_point() +
  facet_grid(subject~type)
ff.m %>% filter((type %in% c("buttery", "painty")) & (subject %in% c("3", "10"))) %>%
  ggplot(aes(time, rating, colour=treatment)) +
  geom_point() +
  facet_grid(subject~type)
ff.m.av <- ff.m %>%
  group_by(subject, time, type, treatment) %>%
  summarise(rating=mean(rating))
ggplot(data=ff.m, aes(time, rating, colour=treatment)) +
  geom_point() +  facet_grid(subject~type) +
  geom_line(data=ff.m.av, aes(group=treatment))
ff.m.av <- ff.m %>%
  filter((type %in% c("buttery", "painty")) & (subject %in% c("3", "10"))) %>%
  group_by(subject, time, type, treatment) %>%
  summarise(rating=mean(rating))
ff.m %>% filter((type %in% c("buttery", "painty")) & (subject %in% c("3", "10"))) %>%
  ggplot(aes(time, rating, colour=treatment)) +
  geom_point() +  facet_grid(subject~type) +
  geom_line(data=ff.m.av, aes(group=treatment))

# Genes
genes <- read_csv("data/genes.csv")
head(genes)
gather(genes, variable, expr, -id) %>% head
genes %>%
  gather(variable, expr, -id) %>%
  separate(variable, c("trt", "leftover"), "-") %>%
  head
genes %>%
  gather(variable, expr, -id) %>%
  separate(variable, c("trt", "leftover"), "-") %>%
  separate(leftover, c("time", "rep"), "\\.") %>% head
gtidy <- genes %>%
  gather(variable, expr, -id) %>%
  separate(variable, c("trt", "leftover"), "-") %>%
  separate(leftover, c("time", "rep"), "\\.") %>%
  mutate(trt = sub("W", "", trt)) %>%
  mutate(rep = sub("R", "", rep))
head(gtidy)
gmean <- gtidy %>%
  group_by(id, trt, time) %>%
  summarise(expr = mean(expr))
ggplot(data = gtidy, aes(trt, expr, colour = time)) +
         geom_point() +
  xlab("Type of modification") + ylab("Expression") +
  facet_wrap(~id) +
  geom_line(data = gmean, aes(group = time))

# Temperature
melbtemp.m <- melbtemp %>%
  select(num_range("V", c(1,2,3,4,seq(5,128,4)))) %>%
  filter(V4 %in% c("PRCP", "TMAX", "TMIN")) %>%
  gather(day, value, V5:V125, na.rm = TRUE) %>%
  spread(V4, value) %>%
  mutate(
    tmin = as.numeric(TMIN) / 10,
    tmax = as.numeric(TMAX) / 10,
    t_range = tmax - tmin,
    prcp = as.numeric(PRCP) / 10
  ) %>%
  rename(stn=V1, year=V2, month=V3)
head(melbtemp.m)
melbtemp.m$day <- factor(melbtemp.m$day,
  levels=c("V5","V9","V13","V17","V21","V25","V29",
           "V33","V37","V41","V45","V49","V53","V57",
           "V61","V65","V69","V73","V77","V81","V85",
           "V89","V93","V97","V101","V105","V109",
           "V113","V117","V121","V125"),
  labels=1:31)
melbtemp.m$date <- as.Date(paste(melbtemp.m$day,
     melbtemp.m$month, melbtemp.m$year, sep="-"),
     "%d-%m-%Y")
head(melbtemp.m)

melbtemp.m$wday <- wday(melbtemp.m$date, label=TRUE, abbr=TRUE)

tb_tidy <- tb %>%
  gather(demographic, cases, m_04:f_u, na.rm = TRUE) %>%
  separate(demographic, c("sex", "age"), "_") %>%
  rename(country = iso2) %>%
  arrange(country, year, sex, age)
head(tb_tidy)

# Time
now()
today()
now() + hours(4)
today() - days(2)
ymd("2013-05-14")
mdy("05/14/2013")
dmy("14052013")
ymd_hms("2013:05:14 14:5:30", tz = "Australia/Melbourne")
month(ymd("2013-05-14"))
year(ymd("2013-05-14"))
wday(ymd("2013-05-14"), label=TRUE, abbr=TRUE)
isWeekday(ymd("2013-05-14"))

# Workers
workers$`Accident Date` <- as.Date(workers$`Accident Date`,
                                    format="%m/%d/%Y")
summary(workers$`Accident Date`)
workers$year <- year(workers$`Accident Date`)
summary(workers$year)
workers %>% group_by(year) %>% tally() %>% head
workers$month <- month(workers$`Accident Date`,
                       label=TRUE, abbr=TRUE)
summary(workers$month)
workers$wday <- wday(workers$`Accident Date`,
                     label=TRUE, abbr=TRUE)
summary(workers$wday)
workers$timeindx <- as.numeric(workers$`Accident Date`-
        as.Date("01/01/2000", format="%m/%d/%Y"))
ws <- workers %>% filter(year > 1999)
ggplot(ws, aes(x=year)) + geom_bar()
ggplot(ws, aes(x=month)) + geom_bar()
ws$wday <- factor(ws$wday, levels=levels(ws$wday)[c(2:7,1)])
ggplot(ws, aes(x=wday)) + geom_bar()
ggplot(ws, aes(x=year, fill=`Claim Type`)) + geom_bar()
ws %>% filter(Gender %in% c("F", "M")) %>%
  ggplot(aes(x=year, fill=Gender)) + geom_bar(position="fill")
