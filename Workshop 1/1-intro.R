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

# Read PISA data
student2012.sub <- readRDS("data/student_sub.rds")
world <- getMap(resolution = "low")
extractPolys <- function(p) {
  polys <- NULL
  for (i in 1:length(p)) {
    for (j in 1:length(p[[i]]@Polygons)) {
      x <- p[[i]]@Polygons[[j]]@coords
      polys$lon <- c(polys$lon, x[,1])
      polys$lat <- c(polys$lat, x[,2])
      polys$ID <- c(polys$ID, rep(p[[i]]@ID, nrow(x)))
      polys$region <- c(polys$region, rep(paste(p[[i]]@ID, j, sep="_"), nrow(x)))
      polys$order <- c(polys$order, 1:nrow(x))
    }
  }
  return(data.frame(polys))
}
polys <- extractPolys(world@polygons)

# Map theme
theme_map <- theme_bw()
theme_map$line <- element_blank()
theme_map$strip.text <- element_blank()
theme_map$axis.text <- element_blank()
theme_map$plot.title <- element_blank()
theme_map$axis.title <- element_blank()
theme_map$panel.border <- element_rect(colour = "grey90", size=1, fill=NA)
student2012.sub$ST04Q01 <- factor(student2012.sub$ST04Q01,
  levels=c(1,2), labels=c("Female", "Male"))

# Calculate the statistics
student2012.stats <- student2012.sub %>%
  group_by(CNT) %>%
  summarise(mathgap=mean(PV1MATH[ST04Q01=="Male"], na.rm=T)-
                    mean(PV1MATH[ST04Q01=="Female"], na.rm=T),
            wmathgap=weighted.mean(PV1MATH[ST04Q01=="Male"],
                                   w=SENWGT_STU[ST04Q01=="Male"], na.rm=T)-
                     weighted.mean(PV1MATH[ST04Q01=="Female"],
                                   w=SENWGT_STU[ST04Q01=="Female"], na.rm=T))

# Compute confidence intervals
cifn <- function(d, i) {
  x <- d[i,]
  ci <- weighted.mean(x$PV1MATH[x$ST04Q01=="Male"],
                                   w=x$SENWGT_STU[x$ST04Q01=="Male"], na.rm=T)-
                     weighted.mean(x$PV1MATH[x$ST04Q01=="Female"],
                                   w=x$SENWGT_STU[x$ST04Q01=="Female"], na.rm=T)
  ci
}
bootfn <- function(d) {
  r <- boot(d, statistic=cifn, R=100)
  l <- sort(r$t)[5]
  u <- sort(r$t)[95]
  ci <- c(l, u)
  return(ci)
}

student2012.sub.summary.gap.boot <- student2012.sub %>%
  split(.$CNT) %>% purrr::map(bootfn) %>% data.frame() %>%
  gather(CNT, value)
student2012.sub.summary.gap.boot$ci <-
  rep(c("ml","mu"), length(unique(student2012.sub.summary.gap.boot$CNT)))
student2012.sub.summary.gap.boot.wide <- student2012.sub.summary.gap.boot %>% spread(ci, value)
student2012.sub.summary.gap <- merge(student2012.stats, student2012.sub.summary.gap.boot.wide)

# Match three digit codes to country names
student2012.sub.summary.gap$name <- NA
for (i in 1:length(student2012.sub.summary.gap$name))
  student2012.sub.summary.gap$name[i] <-
  isoToName(as.character(student2012.sub.summary.gap$CNT[i]))
# QCN is Shanghai, not whole of China - Don't know what country TAP is
student2012.sub.summary.gap$name[student2012.sub.summary.gap$CNT == "QCN"] <- isoToName("CHN")
student2012.sub.summary.gap$name[student2012.sub.summary.gap$CNT == "TAP"] <- "TAP"

# Make a categorical gap variable
student2012.sub.summary.gap$wmathgap_cat <- "same"
student2012.sub.summary.gap$wmathgap_cat[student2012.sub.summary.gap$ml > 0] <- "boys"
student2012.sub.summary.gap$wmathgap_cat[student2012.sub.summary.gap$mu < 0] <- "girls"

# Set order of countries by math gap
student2012.sub.summary.gap$CNT <- factor(student2012.sub.summary.gap$CNT,
      levels=student2012.sub.summary.gap$CNT[order(student2012.sub.summary.gap$wmathgap)])
student2012.sub.summary.gap$name <- factor(student2012.sub.summary.gap$name,
      levels=student2012.sub.summary.gap$name[order(student2012.sub.summary.gap$wmathgap)])

# Plot
ggplot(data=student2012.sub.summary.gap) +
  geom_hline(yintercept=0, colour="grey80") + coord_flip() + theme_bw() +
  geom_point(aes(x=name, y=wmathgap, color=wmathgap_cat), size=3) +
  geom_segment(aes(x=name, xend=name, y=ml, yend=mu, color=wmathgap_cat)) +
  xlab("") +
  scale_colour_manual("", values=c("boys"="skyblue", "girls"="pink", "same"="lightgreen")) +
  scale_y_continuous("Girls <----------> Boys", breaks=seq(-30, 30, 10), limits=c(-35, 35),
                     labels=c(seq(30, 0, -10), seq(10, 30, 10))) +
  theme(axis.text.x = element_text(size=5), axis.text.y = element_text(size=5),
        axis.title = element_text(size=7), legend.text = element_text(size=5),
        legend.title = element_text(size=5))
polys <- polys %>% rename(name = ID)
student2012.sub.map <- left_join(student2012.sub.summary.gap, polys)
student2012.sub.map <- student2012.sub.map %>% arrange(region, order)

ggplot(data=polys) +
  geom_path(aes(x=lon, y=lat, group=region, order=order), colour=I("grey90"), size=0.1) +
  geom_polygon(data=student2012.sub.map, aes(x=lon, y=lat, group=region, order=order,  fill=wmathgap_cat)) +
  scale_fill_manual("Diff>5", values=c("boys"="skyblue", "girls"="pink", "same"="lightgreen")) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  coord_equal() + theme_map
CO2.ptb<-read.table("http://scrippsco2.ucsd.edu/sites/default/files/data/flask_co2_and_isotopic/daily_co2/fldav_ptb.csv", sep=",", skip=69)
colnames(CO2.ptb)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.ptb$lat<-71.3
CO2.ptb$lon<-(-156.6)
CO2.ptb$stn<-"ptb"

CO2.ljo<-read.table("http://scrippsco2.ucsd.edu/sites/default/files/data/flask_co2_and_isotopic/daily_co2/fldav_ljo.csv", sep=",", skip=69)
colnames(CO2.ljo)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.ljo$lat<-32.9
CO2.ljo$lon<-(-117.3)
CO2.ljo$stn<-"ljo"

CO2.mlf<-read.table("http://scrippsco2.ucsd.edu/sites/default/files/data/flask_co2_and_isotopic/daily_co2/fldav_mlf.csv", sep=",", skip=69)
colnames(CO2.mlf)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.mlf$lat<-19.5
CO2.mlf$lon<-(-155.6)
CO2.mlf$stn<-"mlf"

CO2.spo<-read.table("http://scrippsco2.ucsd.edu/sites/default/files/data/flask_co2_and_isotopic/daily_co2/fldav_spo.csv", sep=",", skip=69)
colnames(CO2.spo)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.spo$lat<- (-90.0)
CO2.spo$lon<-0
CO2.spo$stn<-"spo"

CO2.ker<-read.table("http://scrippsco2.ucsd.edu/sites/default/files/data/flask_co2_and_isotopic/daily_co2/fldav_ker.csv", sep=",", skip=69)
colnames(CO2.ker)<-c("date", "time", "day", "decdate", "n", "flg", "co2")
CO2.ker$lat<-(-29.2)
CO2.ker$lon<-(-177.9)
CO2.ker$stn<-"ker"

CO2.all<-rbind(CO2.ker,CO2.ljo,CO2.mlf,CO2.ptb,CO2.spo)
CO2.all$date<-as.Date(CO2.all$date)

CO2.all$invlat=-1*CO2.all$lat
CO2.all$stn=reorder(CO2.all$stn,CO2.all$invlat)

CO2.all.loc <- rbind(CO2.ker[1,],CO2.ljo[1,],CO2.mlf[1,],CO2.ptb[1,],CO2.spo[1,])

p1 <- qplot(date, co2, data=subset(CO2.all, flg < 2), colour=stn, geom="line",xlab="Year",ylab="CO2 (ppm)") +
		facet_wrap(~stn, ncol=1) + theme(axis.text.y=element_text(size = 6), legend.position="none")
p2 <- qplot(date, co2, data=subset(CO2.all, flg < 2), colour=stn, geom="line",xlab="Year",ylab="CO2 (ppm)") +
  theme(axis.text.y=element_text(size = 6), legend.position="none")
grid.arrange(p1, p2, ncol=2)
ggplot(data=polys) +
  geom_path(aes(x=lon, y=lat, group=region, order=order), colour=I("grey90"), size=0.1) +
  geom_point(data=CO2.all.loc, aes(x=lon, y=lat, group=1), colour="red",
                      size=2, alpha=0) +
  geom_text(data=CO2.all.loc, aes(x=lon, y=lat, label=stn, group=1),
            colour="orange", size=5) +
  coord_equal() + theme_map

## # devtools::install_github("metacran/crandb")
## # pkgs <- crandb::list_packages(limit = 999999)
## # length(pkgs)
## # [1] 7330

workers <- read_csv(file="data/Assembled_Workers__Compensation_Claims___Beginning_2000.csv", n_max=50)
workers <- read.csv(file="data/Assembled_Workers__Compensation_Claims___Beginning_2000.csv", nrows=50)

dim(workers)
colnames(workers)
typeof(workers$`Claim Identifier`)
typeof(workers$Claim.Identifier)
typeof(workers$`Claim Type`)
set.seed(1000)
x <- rnorm(4)
x
sum(x + 10)
x[1]
x[c(T, F, T, T, F, F)]
x <- list(
  a = 10,
  b = c(1, "2")
)
x$a
x[["a"]]
x["a"]
str(x)
mean(workers$`Birth Year`)
mean(workers$`Birth Year`, na.rm=TRUE)
table(workers$Gender)
table(workers$Gender, workers$`Zip Code`)
## browseVignettes("dplyr")
`+`
"+" <- function(x, y) "I forgot how to add"
1 + 2
rm("+")

## workers <- read_csv(file="data/Assembled_Workers__Compensation_Claims___Beginning_2000.csv", n_max=1000)
## table(workers$`Claim Type`)
## mean(workers$`Birth Year`, na.rm=TRUE)
## sd(workers$`Birth Year`, na.rm=TRUE)
## summary(workers$`Birth Year`)
