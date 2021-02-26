rm(list=ls())

library(splitstackshape)
library(tseries)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)
library(dplyr)
library(ggplot2)
library(nlme)
library(GGally)
library(lme4)
library(foreign)
library(MASS)
library(reshape)
library(glmmTMB)
library(itsadug)
library(sjPlot)
library(sem)
library(rstanarm)
library(lsmeans)
library(emmeans)
library(egg)
library(cowplot)
library(car)
library(DHARMa)


################### EXPLORATORY DATA ANALYSIS ###############
# general questions:
######## 1) how has biomass changed over time (per species)?##########

setwd("~/Desktop")

#load in biomass dataset (1991-2019)
question1data <- read.csv("Biomass_1991_2019.csv")
head(question1data)
str(question1data)
question1data$site_id <- as.factor(question1data$site_id)
summary(question1data) 

#remove sites 17,18,19, and 20 (San Clemente Island)
question1data <- question1data[question1data$site_id != "17" & question1data$site_id != "18" & question1data$site_id != "19" & question1data$site_id != "20",]
#add in SD for av.total.biomass (for error bars in figures - ultimately removed
#to avoid clutter)
xbar <- question1data$av.count.m2
zbar <- question1data$av.wetmass
N <- question1data$Nquads
sx <- question1data$sd.count
sz <- question1data$sd.wetmass
cov <- cov(xbar,zbar)
question1data$SD.biomass <- ((xbar*zbar)/sqrt(N))*sqrt((sx^2/xbar^2)+(sz^2/zbar^2))

#exploratory plot, all sites
ggplot(question1data, aes(x=Year, y=av.total.biomass, shape=Urchin, colour=Urchin)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  facet_wrap(~site_id) +
  labs(y="Average Biomass g/m^2") +
  scale_color_manual(values=c("purple","red"))

#select only sites that transitioned to become marine reserve
sites2.6.9.14 <- subset(question1data, site_id=="2" | site_id=="6" | site_id=="9" | site_id=="14")
head(sites2.6.9.14)

#Figure - Reserve sites only 
ggplot(sites2.6.9.14, aes(x=Year, y=av.total.biomass, shape=Urchin, colour=Urchin)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Name) +
  labs(y=expression("Biomass"~g/m^2)) +
  scale_color_manual(values=c("purple","red")) +
  geom_vline(xintercept=2004) +
  geom_errorbar(aes(ymin=av.total.biomass-SD.biomass, ymax=av.total.biomass+SD.biomass), width=.2) +
  theme_classic() +
  theme(legend.position = c(0.92, 0.3))

###### summary figure with averages ######
annual.average <- question1data %>%
  group_by(Year,Urchin,reserve.status) %>%
  summarize(annual.mean=mean(av.total.biomass), sd.mean=sd(av.total.biomass))
annual.average <- as.data.frame(annual.average)
total.average <- question1data %>%
  group_by(Year,reserve.status) %>%
  summarize(annual.total.mean=mean(av.total.biomass), sd.total.mean=sd(av.total.biomass))
total.average <- as.data.frame(total.average)

full <- merge(annual.average,total.average, by=c("Year", "reserve.status"))
text1 <- c("A","B", "C", "D")
plot1 <- ggplot(annual.average, aes(x=Year, y=annual.mean, color=Urchin)) +
  geom_point() +
  geom_smooth(method="glm") +
  facet_grid(vars(Urchin),vars(reserve.status)) +
  xlim(2004,2019) +
  ylim(0,1000) +
  labs(y=expression(paste("Average Urchin Biomass g ",m^-2)), x="Year") +
  scale_color_manual(values=c("purple","red")) +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust=.1),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border= element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_blank())

plot2 <-  ggplot(total.average, aes(x=Year, y=annual.total.mean)) +
  geom_point() +
  geom_smooth(method="glm") +
  facet_wrap(vars(reserve.status)) +
  xlim(2004,2019) +
  ylim(0,1000) +
  labs(x="Year") +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.title.y = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border= element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_blank())

plot1 <- egg::tag_facet(plot1)
plot2 <- egg::tag_facet(plot2, tag_pool=c("e","f"))
tiff("test.tiff", units="in", width=5, height=5, res=300)
egg::ggarrange(plot1,plot2,heights=c(.67,.33))
dev.off()

##time series plots
all.average <- tapply(question1data$av.total.biomass, question1data$Year, mean)
all.average <- melt(all.average)
names(all.average)[1:2]<-c("Year","Average.Biomass")
plot(all.average, xlab="Year", ylab="Average Urchin Biomass", type="l")

red.average <- tapply(question1data[question1data$Urchin == "red",]$av.total.biomass, question1data[question1data$Urchin == "red",]$Year, mean)
red.average <- melt(red.average)
names(red.average)[1:2]<-c("Year","Average.Biomass")
plot(red.average, type = "l", xlab="Year", ylab="Average Red Biomass g/m^2")

purple.average <- tapply(question1data[question1data$Urchin == "purple",]$av.total.biomass, question1data[question1data$Urchin == "purple",]$Year, mean)
purple.average <- melt(purple.average)
names(purple.average)[1:2]<-c("Year","Average.Biomass")
plot(purple.average, type = "l", xlab="Year", ylab="Average Purple Biomass g/m^2")


# 1a) reserve vs non-reserve (random effect for site)
# recreate column of yes/no
for(i in 1:nrow(question1data)){
  if(question1data$time.reserve[i]==0){
    question1data$reserve.status[i] <- "0"
  }
  else{
    question1data$reserve.status[i] <- "1"
  }
}
question1data$reserve.status <- as.factor(question1data$reserve.status)

lme.reserve <- lme(av.total.biomass ~ Urchin + reserve.status,random = ~ 1 |site_id, data=question1data)
summary(lme.reserve) 
plot(lme.reserve) #cone of residuals
lme.reserve1 <- lme(log(av.total.biomass+.01) ~ Urchin + reserve.status,random = ~ 1 |site_id, data=question1data)
summary(lme.reserve1)
plot(lme.reserve1) #better, no significant reserve effect

###### BACI Analysis - Urchins ########
#select only the first 16 sites
baci.data <- question1data[question1data$site_id=="1" | question1data$site_id=="2" | question1data$site_id=="3" | question1data$site_id=="4" | question1data$site_id=="5" | question1data$site_id=="6"| question1data$site_id=="7"| question1data$site_id=="8"| question1data$site_id=="9"| question1data$site_id=="10" | question1data$site_id=="11"| question1data$site_id=="12"| question1data$site_id=="13"| question1data$site_id=="14"| question1data$site_id=="15"| question1data$site_id=="16",]
head(baci.data)
str(baci.data)
summary(baci.data)
for(i in 1:nrow(baci.data)){
  if(baci.data$Year[i]<=2004){
    baci.data$pre.post[i]<-0
  }
  else{
    baci.data$pre.post[i]<-1
  }
}

#overview plot
ggplot(baci.data,aes(x=Year,y=av.total.biomass, color=Urchin, group=Year >2003)) +
  geom_point() +
  geom_smooth(method="lm") + 
  labs(y="Average Urchin Biomass g/m^2", title="Before/After Marine Reserve Establishment") +
  facet_wrap(~site_id) +
  geom_vline(xintercept=2004) +
  scale_color_manual(values=c("purple","red"))

#impact sites = 2,6,9,14
#get rid of anacapa island sites 12 & 13
baci.data <- baci.data[baci.data$site_id != "12" & baci.data$site_id != "13",]
for(i in 1:nrow(baci.data)){
  if(baci.data$site_id[i]=="2" | baci.data$site_id[i]=="6" | baci.data$site_id[i]=="9" | baci.data$site_id[i]=="14"){
    baci.data$site.class[i]<-"Impact"
  }
  else{
    baci.data$site.class[i]<-"Control"
  }
}
head(baci.data)

##### FIGURE PLOT ALL SITES  ######
#reorder factor levels so that Gull Island, Hare Rock, Scorpion Anchorage, and SE Sea Lion Rookery come first
levels(baci.data$Name)
neworder <- c("Gull Island","Hare Rock","Scorpion Anchorage","SE Sea Lion Rookery","Admiral's Reef","Cat Canyon","Fry's Harbor","Arch Point","Johnson's Lee North","Johnson's Lee South","Rodes Reef","Pelican Bay ","Wycoff Ledge","Yellow Banks")
baci.data2 <- arrange(mutate(baci.data, Name=factor(Name, levels=neworder)),Name)
head(baci.data2)
baci.data2 <- baci.data2[,-19]
baci.data2 <- na.omit(baci.data2)
summary(baci.data2)

tiff("fig2.tiff", units="in", width=5, height=5, res=300)
ggplot(data=baci.data2,aes(x=Year,y=av.total.biomass, color=Urchin, group=interaction(Urchin, pre.post))) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  labs(y=expression(paste("Urchin Biomass g ",m^-2)),x="Year") +
  facet_wrap(~Name) +
  ylim(0,2500) +
  geom_vline(xintercept=2004) +
  scale_color_manual(values=c("purple","red")) +
  theme_classic() +
  theme(legend.position = c(0.75, 0.08)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(strip.text = element_text(size=7))
dev.off()

#repeated measures anova
m1 <- aov(av.total.biomass ~ pre.post, data=baci.data)
summary(m1)
m2 <- aov(av.total.biomass ~ site.class, data=baci.data)
summary(m2)
m3 <- aov(av.total.biomass ~ site.class*pre.post, data=baci.data)
summary(m3)
#site class significant, as is interaction term
m4 <- lme(av.total.biomass ~ pre.post + site.class + site.class*pre.post, random=~1|site_id, data=baci.data)
summary(m4)
#interaction term significant
#include urchin
m5 <- lme(av.total.biomass ~ Urchin + pre.post + site.class + site.class*pre.post, random=~1|site_id, data=baci.data)
summary(m5)
qqPlot(residuals(m5))


### Final full model - repeated measures ANOVA
#Log transform biomass for residuals
m6 <- lme(log(av.total.biomass+1) ~ site.class*pre.post, random=~Urchin|site_id, data=baci.data)
summary(m6)
qqPlot(residuals(m6)) # this looks normal enough
plot(m6) 
#calculate transformed covariate
exp(m6$coefficients$fixed[4]) -1
pacf(residuals(m6))

##BACI with year and site as random effects
m7 <- lme(log(av.total.biomass+1) ~ site.class*pre.post, random=~Urchin|Year/site_id, data=baci.data)
pacf(residuals(m7)) #short lags 0-1
m7_update <- update(m7, correlation=corAR1())
plot(m7)
qqPlot(residuals(m7))
plot(m7_update)
qqPlot(residuals(m7_update))

# calculate percent increase in average urchin biomass
tot.sum <- baci.data %>%
  group_by(pre.post,site.class) %>%
  summarize(overall.mean= mean(av.total.biomass))
tot.sum <- as.data.frame(tot.sum)
diff.control = tot.sum[3,3]-tot.sum[1,3]
diff.impact = tot.sum[4,3] -tot.sum[2,3]
((diff.impact - diff.control)/abs(diff.control))*100

#another way of doing this, estimated means from lsmeans
final.model <- m7_update
final.model.noIntercept <- update(m7_update, . ~ . -1)
estimates <- lsmeans::lsmeans(final.model.noIntercept, ~ site.class:pre.post, type = "response")
#baci effect, from Schwarz (2015)
est <- predict(ref.grid(final.model.noIntercept), type = "response") # get the estimates only (without CI)
names(est) <- c("CB","IB","CA","IA"); est # give names to the estimates vector
baci <- est["CA"]-est["CB"]-(est["IA"]-est["IB"])
baci #large and negative -110.2565 - urchins overall decreased at control (fished) sites and increased at impact (reserve) sites

## two separate models, one for each species
red.baci <- subset(baci.data, Urchin=="red")
purple.baci <- subset(baci.data, Urchin=="purple")

### Modelling Red Urchin BACI
m.red <- lme(av.total.biomass ~ pre.post + site.class + site.class*pre.post, random=~1|site_id, data=red.baci)
summary(m.red)
#add in temporal autocorrelation and year into error structure
m.red2 <- lme(av.total.biomass ~ pre.post + site.class + site.class*pre.post, random=~1|Year/site_id, cor=corAR1(),data=red.baci)
plot(m.red2)
anova(m.red, m.red2)
summary(m.red2)
#significant interaction between BA*CI
#model diagnostics
residuals <- resid(m.red2)
plot(m.red2, main="Red BACI Residuals")
qqnorm(residuals)
qqline(residuals)
# log transform
m.red.3 <- lme(log(av.total.biomass+1) ~ site.class*pre.post, random=~1|Year/site_id, cor=corAR1(),data=red.baci)
plot(residuals(m.red.3))
qqPlot(residuals(m.red.3))
summary(m.red.3)

#BACI contrast
summary.red <- red.baci %>%
  group_by(pre.post,site.class) %>%
  summarize(red.mean= mean(av.total.biomass))
summary.red <- as.data.frame(summary.red)
diff.control = summary.red[3,3]-summary.red[1,3]
diff.impact = summary.red[4,3] -summary.red[2,3]
diff.control - diff.impact #BACI contrast
((diff.impact - diff.control)/abs(diff.control))*100 #percent change in red urchins

#same thing, using lsmeans
final.model1 <- m.red.3
final.model.noIntercept1 <- update(m.red.3, . ~ . -1)
estimates <- lsmeans::lsmeans(final.model.noIntercept, ~ site.class:pre.post, type = "response")
#baci effect, from Schwarz (2015)
est1 <- predict(ref.grid(final.model.noIntercept1), type = "response") # get the estimates only (without CI)
names(est1) <- c("CB","IB","CA","IA"); est # give names to the estimates vector
baci.red1 <- est1["CA"]-est1["CB"]-(est1["IA"]-est1["IB"])
baci.red1 #large and negative -172.2706 - red urchins decreased at control (fished) sites and increased at impact (reserve) sites


#### Modelling Purple Urchin BACI
m.purple<- lme(av.total.biomass ~ pre.post + site.class + site.class*pre.post, random=~1|site_id, data=purple.baci)
summary(m.purple)
#add in temporal autocorrelation
m.purple2 <- lme(av.total.biomass ~ pre.post + site.class + site.class*pre.post, random=~1|site_id, cor=corAR1(),data=purple.baci)
anova(m.purple, m.purple2)
summary(m.purple2)
plot(m.purple2)
#no significant terms
#model diagnostics
residuals.p <- resid(m.purple2)
plot(m.purple2, main="Purple BACI Residuals")
qqnorm(residuals.p)
qqline(residuals.p)
#skewed tails and cone-shaped residuals

#log transform and add in temporal autocorrelation, year in error structure
m.purple3 <- lme(log(av.total.biomass+1) ~ site.class*pre.post, random=~1|Year/site_id, cor=corAR1(),data=purple.baci)
summary(m.purple3)

#model diagnostics
plot(residuals(m.purple3))
qqPlot(residuals(m.purple3))
hist(residuals(m.purple3)) #slight right skew
#since no BACI effect for purples, no BACI contrast calculated for purples
#but for demonstration purposes...
final.model2 <- m.purple3
final.model.noIntercept2 <- update(m.purple3, . ~ . -1)
estimates <- lsmeans::lsmeans(final.model.noIntercept2, ~ site.class:pre.post, type = "response")
est2 <- predict(ref.grid(final.model.noIntercept2), type = "response") # get the estimates only (without CI)
names(est2) <- c("CB","IB","CA","IA"); est # give names to the estimates vector
baci.purple1 <- est2["CA"]-est2["CB"]-(est2["IA"]-est2["IB"])
baci.purple1 #small negative value (slight decrease at control sites and impact sites)

#subset to see slopes for each site (visualized on fig 3)
pre.2004 <- subset(baci.data, pre.post==0)
head(pre.2004)
post.2004 <- subset(baci.data, pre.post==1)
red.pre <- subset(pre.2004, Urchin=="red")
red.post <- subset(post.2004, Urchin=="red")
purple.pre <- subset(pre.2004, Urchin=="purple")
purple.post <- subset(post.2004, Urchin=="purple")

red.pre.lm <- lmList(av.total.biomass ~ Year | site_id, data=red.pre)
summary(red.pre.lm)
red.post.lm <- lmList(av.total.biomass ~ Year | site_id, data=red.post)
summary(red.post.lm)

purple.pre.lm <- lmList(av.total.biomass ~ Year | site_id, data=purple.pre)
summary(purple.pre.lm)
purple.post.lm <- lmList(av.total.biomass ~ Year | site_id, data=purple.post)
summary(purple.post.lm)

######## 1b) does time since a site has been a reserve affect biomass? (exclude non-reserve)
q1data.reserve <- question1data[question1data$time.reserve != 0,]
summary(q1data.reserve)
ggplot(question1data, aes(x=reserve.status, y=av.total.biomass, fill=Urchin)) +
  geom_boxplot() +
  labs(x="Reserve Status", y="Average biomass (g/m^2)")

#break down into young vs old reserves
#with anacapa
for(i in 1:nrow(q1data.reserve)){
  if(q1data.reserve$time.reserve[i] <6){
    q1data.reserve$reserve.age[i] <- "0-5 years"
  }
  else if(q1data.reserve$time.reserve[i] >5 & q1data.reserve$time.reserve[i] <11){
    q1data.reserve$reserve.age[i] <- "6-10 years"
  }
  else{
    q1data.reserve$reserve.age[i] <- "11+ years"
  }
}
ggplot(q1data.reserve, aes(x=reserve.age, y=av.total.biomass, fill=Urchin)) +
  geom_boxplot() +
  labs(x="Reserve Age", y="Average biomass (g/m^2)", title="Urchin Biomass by Reserve Age") +
  scale_x_discrete(limits=c("0-5 years","6-10 years","11+ years")) +
  scale_fill_manual(values = c("purple", "red")) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.9))

####### 2) does urchin species affect the other species biomass? ##############

red <- subset(question1data, Urchin=="red")
str(red)
head(red)
purple <- subset(question1data, Urchin =="purple")
str(purple)

q2 <- reshape(question1data, idvar=c("site_id", "Year"), timevar="Urchin", direction="wide")
head(q2)
ggplot(q2, aes(x=av.total.biomass.red, y=av.total.biomass.purple)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Average Red Biomass (g/m^2)", y="Average Purple Biomass (g/m^2)")
q2.lm <- lm(av.total.biomass.red ~ av.total.biomass.purple, data=q2)
summary(q2.lm)

# 2b) does fishing of red urchins affect either species?

#exploratory plots using annual catch data assigned to corresponding blocks which include sites/transects
ggplot(question1data, aes(x=Annual.catch, y=av.total.biomass, color=Urchin)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~Urchin) +
  labs(y="Average Biomass (g/m^2)", x="Annual Catch of Red Urchins (lbs)")

ggplot(sites2.6.9.14, aes(x=Year, y=Annual.catch)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~site_id) +
  labs(y="Annual catch of red urchins (lbs)", x="Year") +
  theme_classic()

#categorical catch descriptors
for(i in 1:nrow(question1data)){
  if(question1data$Annual.catch[i] <25000){
    question1data$catch.level[i] <- "Low (<25,000)"
  }
  else if(question1data$Annual.catch[i] >24999 & question1data$Annual.catch[i] <1000000){
    question1data$catch.level[i] <- "Intermediate (25,000-1,000,000)"
  }
  else{
    question1data$catch.level[i] <- "High (>1,000,000)"
  }
}

ggplot(question1data, aes(x=catch.level, y=av.total.biomass, fill=Urchin)) +
  geom_boxplot() +
  labs(x="Annual Catch of Red Sea Urchins (lbs)", y="Urchin Biomass g/m^2", title="Urchin Biomass by Annual Catch") +
  scale_fill_manual(values = c("purple", "red")) +
  theme_classic() +
  theme(legend.position = c(0.87, 0.9))

head(question1data)
years <- group_by(question1data, Urchin, Year, reserve.status)
avs.plot <- summarize(years, average.biomass=mean(av.total.biomass))
avs.plot$reserve.status <- as.factor(avs.plot$reserve.status)
avs.plot <- as.data.frame(avs.plot)

labels <- c("0" = "Fished", "1" = "Reserve")

#biomass by catch
q2.red <- lm(av.total.biomass ~ Annual.catch, data=red)
summary(q2.red)
q2.purple <- lm(av.total.biomass ~ Annual.catch, data=purple)
summary(q2.purple)

###### Figure - averages across all sites after 2004 #######
ggplot(avs.plot, aes(x=Year, y=average.biomass, color=Urchin)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(Urchin~reserve.status) +
  scale_color_manual(values = c("purple", "red")) +
  xlim(2004,2019) +
  ylim(0,800) +
  theme_classic() +
  labs(y=expression("Average Biomass"~g/m^2)) +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(legend.position = "none") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_blank()
  )


###### Kelp vs Urchin Biomass #######
#load in annual kelp stipe counts
kelp <- read.csv("Annual_Kelp_Mean_Stipe_Density.csv")
kelp <- kelp[2:7] #get rid of "X" column
str(kelp)
#make sure site and time period are factors
kelp$site_id <- as.factor(kelp$site_id)
kelp$pre.post <- as.factor(kelp$pre.post)
#merge kelp values with urchin data
kelp.urchins <- merge(question1data, kelp, by=c("site_id","Year", "time.reserve", "reserve.status"))
head(kelp.urchins)

#keep the imortant columns for analyzing kelp vs urchin biomass
kelp.urchins <- kelp.urchins[c(1:7,15,21:22)]
#spread urchins into two columns so each year*site combo has reds, purples, and kelp
kelp.urchins1 <- spread(kelp.urchins, key=Urchin, value=av.total.biomass)
head(kelp.urchins1)

#all mean kelp stipe data plotted by site, separated into pre and post 2004
kelp.baci <- kelp.urchins[kelp.urchins$site_id=="1" | kelp.urchins$site_id=="2" | kelp.urchins$site_id=="3" | kelp.urchins$site_id=="4" | kelp.urchins$site_id=="5" | kelp.urchins$site_id=="6"| kelp.urchins$site_id=="7"| kelp.urchins$site_id=="8"| kelp.urchins$site_id=="9"| kelp.urchins$site_id=="10" | kelp.urchins$site_id=="11"| kelp.urchins$site_id=="14"| kelp.urchins$site_id=="15"| kelp.urchins$site_id=="16",]
levels(kelp.baci$Name)
neworder <- c("Gull Island","Hare Rock","Scorpion Anchorage","SE Sea Lion Rookery","Admiral's Reef","Cat Canyon","Fry's Harbor","Arch Point","Johnson's Lee North","Johnson's Lee South","Rodes Reef","Pelican Bay ","Wycoff Ledge","Yellow Banks")
kelp.baci.data2 <- arrange(mutate(kelp.baci, Name=factor(Name, levels=neworder)),Name)
head(kelp.baci.data2)

#### Figure - kelp biomass at all sites before/after ###
tiff("fig6.tiff", units="in", width=5, height=5, res=300)
ggplot(data=kelp.baci.data2,aes(x=Year,y=mean_stipe, group=pre.post)) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  facet_wrap(~Name) +
  geom_vline(xintercept=2004) +
  labs(y=expression("Mean Stipes"~m^-2)) +
  theme_classic() +
  theme(strip.text = element_text(size=7))
dev.off()

### Kelp BACI 

#start with simplest version
kelp.m1 <- lme(mean_stipe ~ pre.post + reserve.status + reserve.status*pre.post, random=~1|site_id, data=kelp.baci.data2)
summary(kelp.m1)
plot(kelp.m1) #issues here with clustering - try log
kelp.m2 <- lme(log(mean_stipe+1) ~ pre.post + reserve.status + reserve.status*pre.post, random=~1|site_id, data=kelp.baci.data2)
plot(kelp.m2)
#maybe some issues with this - cone and spread
#try temporal autocorrelation
kelp.m3 <- lme(log(mean_stipe+1) ~ pre.post + reserve.status + reserve.status*pre.post, random=~1|site_id, cor=corAR1(), data=kelp.baci.data2)
plot(kelp.m3)
summary(kelp.m3)
Anova(kelp.m3)
#no significant terms
#model with Year in error structure does not converge
#kelp.m4 <- lme(log(mean_stipe+1) ~ pre.post + reserve.status + reserve.status*pre.post, random=~1|Year/site_id, cor=corAR1(), data=kelp.baci.data2)
#no BACI contrast:
final.model.k <- kelp.m3
final.model.noIntercept.k <- update(kelp.m3, . ~ . -1)
estimates <- lsmeans::lsmeans(final.model.noIntercept.k, ~ reserve.status:pre.post, type = "response")
#baci effect, from Schwarz (2015)
est.k <- predict(ref.grid(final.model.noIntercept.k), type = "response") # get the estimates only (without CI)
names(est.k) <- c("CB","IB","CA","IA"); est # give names to the estimates vector
baci.kelp <- est.k["CA"]-est.k["CB"]-(est.k["IA"]-est.k["IB"])
baci.kelp #very small and positive: 0.291.  slight decrease in kelp in controls and increase in reserves

#kelp density by urchin biomass
ggplot(data=kelp.urchins,aes(x=av.total.biomass,y=mean_stipe, group=Urchin)) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  facet_wrap(~site_id) +
  theme_classic()

#one plot with everything on it for stipe density and biomass
ggplot(data=kelp.urchins,aes(x=av.total.biomass,y=mean_stipe, color=Urchin)) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  scale_color_manual(values=c("purple","red")) +
  labs(x="Urchin Biomass g/m^2", y="Kelp Stipe Density /m^2") +
  theme_classic()

#two plots by species
#### supplementary kelp by urchin biomass figure ####
tiff("kelp_urchin.tiff", units="in", width=5, height=5, res=300)
kelp.urch.plot <- ggplot(data=kelp.urchins,aes(x=av.total.biomass,y=mean_stipe, color=Urchin)) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  scale_color_manual(values=c("purple","red")) +
  labs(x=expression(paste("Urchin Biomass g ",m^-2)),y=expression(paste("Kelp Biomass g ",m^-2))) +
  facet_wrap(~Urchin, scales="free") +
  ylim(0,13) + 
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border= element_rect(colour = "black", fill=NA, size=1),
        panel.background = element_blank(),
        legend.position = "none")
egg::tag_facet(kelp.urch.plot)
dev.off()
#no strong relationship between urchin biomass and kelp stipe density

#### Predators BACI ####
#raw data from MBON
pred<- read.csv("SBCMBON_kelp_forest_integrated_fish_20181129.csv")
sheephead <- subset(pred, taxon_name=="Semicossyphus pulcher" & data_source=="kfm" & sample_method=="visualfish")
head(sheephead)
sheephead$Year <- year(sheephead$date)


#check sheephead dataset
str(sheephead)
sheephead$site_name <- as.numeric(sheephead$site_name)
sheephead.16 <- subset (sheephead, site_name < 17)
ggplot(sheephead.16, aes(y=count, x=Year)) +
  geom_bar(stat="identity") +
  labs(y="Total Sheephead Count") +
  facet_wrap(~site_name)

#isolate the 4 sites that became marine reserves (impact sites)
baci.sheephead <- subset(sheephead,site_name==2 | site_name==6 | site_name==9 | site_name==14)
#show sheephead counts in these 4 sites
ggplot(baci.sheephead, aes(y=count, x=Year)) +
  geom_bar(stat="identity") +
  labs(y="Total Sheephead Count") +
  facet_wrap(~site_name) +
  theme_classic()

#lobster and pyncopodia counts from MBON.  no conversion to biomass because
#lacking size info
predators <- read.csv("lobster_pycnopodia.csv")
head(predators)
#create dataframe of total and average counts by year*site
annual <- group_by(predators, taxa_id, Year, site_id)
predators1 <- as.data.frame(summarize(annual, total.count=sum(count), av.count=mean(count), sd.count=sd(count)))
summary(predators1)
str(predators1)
predators1$site_id <- as.factor(predators1$site_id)
#select the 4 sites that became reserves
bacisites <- subset(predators1,site_id=="2" | site_id=="6" | site_id=="9" | site_id=="14")
head(bacisites)
str(bacisites)
summary(bacisites)
#remove outliers (extremely high counts of pycnopodia - not sure if these
#are true outliers or an irregularity)
outlier <- subset(bacisites, total.count>100)
print(outlier)

#EDA predators 
ggplot(bacisites, aes(x=Year, y=total.count, fill=taxa_id)) +
  geom_bar(stat="identity") +
  facet_wrap(~site_id) +
  labs(y="Total Count",x="Year") +
  theme_classic() +
  theme(legend.position = c(0.85, 0.3))

#merge files.  consider BACI analysis
head(predators1)
head(sheephead)
#select matching columns, get rid of rest
sheephead1 <- sheephead[c(10:12,15:16,18:20)]
head(sheephead1)
#annual sheephead counts (total and average)
annual.sh <- group_by(sheephead1, Year, site_name, taxon_name, latitude, longitude)
sh1 <- as.data.frame(summarize(annual.sh, total.count=sum(count), sd.count=sd(count), av.count=mean(count)))
head(sh1)
names(sh1) <- c("Year", "site_id", "taxa_id", "latitude", "longitude", "total.count", "sd.count", "av.count")
str(sh1)
sh1$site_id <- as.factor(sh1$site_id)

#merge both predator datasets
all.preds <- merge(sh1, predators1, by=c("Year","site_id"))
head(all.preds)
summary(all.preds)
str(all.preds)
baci.data$Year <- as.numeric(baci.data$Year)
str(baci.data)
names(all.preds)[c(6:8,10:12)] <- c("total.count.sh","sd.count.sh","av.count.sh", "total.count.invert","av.count.invert","sd.count.invert") 
#merge in BACI data
head(baci.data) 
all.orgs <- merge(all.preds, baci.data, by=c("site_id","Year"))
head(all.orgs)
summary(all.orgs)
#remove anacapa
all.orgs <- all.orgs[all.orgs$site_id != "12" & all.orgs$site_id != "13",]
#add column for control and impact
for(i in 1:nrow(all.orgs)){
  if(all.orgs$site_id[i]=="2" | all.orgs$site_id[i]=="6" | all.orgs$site_id[i]=="9" | all.orgs$site_id[i]=="14"){
    all.orgs$site.class[i]<-"Impact"
  }
  else{
    all.orgs$site.class[i]<-"Control"
  }
}
head(all.orgs)
all.orgs$pre.post <- as.factor(all.orgs$pre.post)

#reorder so impact sites print first
levels(all.orgs$Name)
neworder <- c("Gull Island","Hare Rock","Scorpion Anchorage","SE Sea Lion Rookery","Admiral's Reef","Cat Canyon","Fry's Harbor","Arch Point","Johnson's Lee North","Johnson's Lee South","Rodes Reef","Pelican Bay ","Wycoff Ledge","Yellow Banks")
all.orgs2 <- arrange(mutate(all.orgs, Name=factor(Name, levels=neworder)),Name)
head(all.orgs2)


#EDA lobster and pycnopodia 
ggplot(data=all.orgs2,aes(x=Year,y=total.count.invert, color=taxa_id.y, group=interaction(taxa_id.y, pre.post))) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  labs(y="Total Count") +
  facet_wrap(~Name) +
  geom_vline(xintercept=2004) +
  theme_classic() +
  ylim(c(0,250)) +
  theme(legend.position = c(0.75, 0.1)) +
  scale_color_discrete(name="Taxa") 

head(all.orgs2)
pycnopodia <- subset(all.orgs2, taxa_id.y=="pycnopodia")
head(pycnopodia)
lobster <- subset(all.orgs2, taxa_id.y=="lobster")

#EDA pycnopodia
tiff("pycnopodia.tiff", units="in", width=5, height=5, res=300)
ggplot(data=pycnopodia,aes(x=Year,y=total.count.invert, group=pre.post)) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  labs(y="Pycnopodia Total Count") +
  facet_wrap(~Name) +
  geom_vline(xintercept=2004) +
  theme_classic() +
  theme(strip.text = element_text(size=7))+ 
  theme(axis.text.x = element_text(angle = 90))
dev.off()

#EDA lobster
tiff("lobster.tiff", units="in", width=5, height=5, res=300)
ggplot(data=lobster,aes(x=Year,y=total.count.invert, group=pre.post)) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  labs(y="Lobster Total Count") +
  facet_wrap(~Name) +
  geom_vline(xintercept=2004) +
  theme_classic() +
  theme(strip.text = element_text(size=7))+ 
  theme(axis.text.x = element_text(angle = 90))
dev.off()

#EDA sheephead
tiff("sheephead.tiff", units="in", width=5, height=5, res=300)
ggplot(data=all.orgs2,aes(x=Year,y=total.count.sh, group=pre.post)) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  labs(y="Sheephead Total Count") +
  facet_wrap(~Name) +
  geom_vline(xintercept=2004) +
  theme_classic() +
  theme(legend.position = c(0.75, 0.1)) + 
  theme(strip.text = element_text(size=7))+ 
  theme(axis.text.x = element_text(angle = 90))
dev.off()
str(all.orgs2)

#Example - Site 4 and 5
p1 <- ggplot(data=all.orgs2[all.orgs2$site_id=="4",],aes(x=Year,y=total.count.sh, group=pre.post)) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  labs(y="Total Count of Sheephead", title="Site 4") +
  geom_vline(xintercept=2004) +
  theme_classic() 

p2 <- ggplot(data=all.orgs2[all.orgs2$site_id=="5",],aes(x=Year,y=total.count.sh, group=pre.post)) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  labs(y="Total Count of Sheephead", title="Site 5") +
  geom_vline(xintercept=2004) +
  theme_classic() 

grid.arrange(p1,p2,nrow=1)

# sheephead baaci model 
bacimodel.sh <- lme(log(total.count.sh +1) ~ pre.post + site.class + site.class*pre.post, random=~1|Year/site_id, data=all.orgs)
summary(bacimodel.sh)
#time period is significant but the site class is not, nor is the interaction 
#between site class and time period.

sheephead2 <- subset(all.orgs, taxa_id.x=="Semicossyphus pulcher")
head(sheephead2)
sheephead2 <- sheephead2[c(1:2,6:8,31:32)]
str(sheephead2)
#find means and demonstrate contrast is small (not significant)
means.sh <- lsmeans(bacimodel.sh, ~pre.post*site.class)
means.sh
car::Anova(bacimodel.sh)
1.152-1.854 #difference for control sites
0.917-1.847 #difference for impact sites
-0.702-(-0.93) #difference of differences

#### Size Classes over time #####
#going back to older version of biomass doc with individual sizes still listed
size <- read.csv("KFM_Raw_Biomass_Edited2.csv")
head(size)
summary(size)

str(size)
#original test: size groupings based off of visual inspection of the data
for(i in 1:nrow(size)){
  if(size$Size_mm[i] < 82){
    size$size_class[i] <- "juvenile"
  }
  else if(size$Size_mm[i] > 81 & size$Size_mm[i] < 125){
    size$size_class[i] <- "small adult"
  }
  else{
    size$size_class[i] <- "large adult"
  }
}

red.size <- subset(size, Urchin=="red")
purple.size <- subset(size, Urchin=="purple")

#Chi Square Test for all Fished and Reserve Sites lumped together
reserve.sizeR <- subset(red.size, time.reserve>0)
summary(reserve.sizeR)
summary.red.reserve <- reserve.sizeR %>%
  group_by(size_class) %>%
  summarize (n = n()) %>%
  mutate(freq = n / sum(n))
summary.red.reserve <- as.data.frame(summary.red.reserve)
str(summary.red.reserve)
summary.red.reserve$reserve.status <- 1

fished.sizeR <- subset(red.size, time.reserve==0)
summary.red.fished <- fished.sizeR %>%
  group_by(size_class) %>%
  summarize (n = n()) %>%
  mutate(freq = n / sum(n))
summary.red.fished <- as.data.frame(summary.red.fished)
str(summary.red.fished)
summary.red.fished$reserve.status <- 0
red.size.table.all <- merge(summary.red.reserve, summary.red.fished, by="size_class")
names(red.size.table.all) <- c("size_class","number.reserve","freq.reserve","reserve.status1","number.fished","freq.fished","reserve.status0")
red.size.chsq <- red.size.table.all[c(2,5)]
rownames(red.size.chsq) <- red.size.table.all$size_class
red.size.chsq
chisq.test(red.size.chsq)

### BACI for reds
#apporach 1: get annual values for frequency
freq.red <- red.size %>%
  count(Year, site_id,size_class) %>%
  group_by(Year, site_id) %>%
  mutate(prop = prop.table(n))
freq.red <- as.data.frame(freq.red)
red.size2 <- red.size %>%
  group_by(Year, site_id, Annual.catch, time.reserve, size_class) %>%
  summarize(n=n())
red.size2 <- as.data.frame(red.size2)
head(red.size2)
freq.red2 <- merge(freq.red, red.size2, by=c("Year","site_id","size_class","n"))
head(freq.red2)

#approach 2:
#change size class groupings
size2 <- read.csv("KFM_Raw_Biomass_Edited2.csv")
str(size2)
## broken into size classes based on Becca Selden's paper
for(i in 1:nrow(size2)){
  if(size2$Size_mm[i] < 36){
    size2$size_class[i] <- "small"
  }
  else if(size2$Size_mm[i] > 35 & size2$Size_mm[i] < 51){
    size2$size_class[i] <- "medium"
  }
  else if(size2$Size_mm[i] > 50 & size2$Size_mm[i] < 71){
    size2$size_class[i] <- "large"
  }
  else{
    size2$size_class[i] <- "very large"
  }
}
head(size2)

size2$size_class <- as.factor(size2$size_class)
#first, calculate the proportion of individuals in each size class
summary.size <- size2 %>%
  group_by(size_class, Year, site_id, Urchin) %>%
  summarize (n = n()) %>%
  mutate(freq = n / sum(n))
head(summary.size)
summary.size <- as.data.frame(summary.size)
summary(summary.size)
str(summary.size)
summary.size$site_id <- as.factor(summary.size$site_id)
#create a new data frame with annual size frequencies and all the other urchin data
newdf <- merge(summary.size, baci.data, by=c("Year","site_id","Urchin"))
head(newdf)
summary(newdf)
#calculate the biomass in each class by multiplying the proportion of 
#individuals in that size class by the average biomass in that site/year
newdf$biomass.in.class <- newdf$freq * newdf$av.total.biomass
#choose first 16 sites
newdf1 <- newdf[newdf$site_id=="1" | newdf$site_id=="2" | newdf$site_id=="3" | newdf$site_id=="4" | newdf$site_id=="5" | newdf$site_id=="6"| newdf$site_id=="7"| newdf$site_id=="8"| newdf$site_id=="9"| newdf$site_id=="10" | newdf$site_id=="11"| newdf$site_id=="12"| newdf$site_id=="13"| newdf$site_id=="14"| newdf$site_id=="15"| newdf$site_id=="16",]
levels(newdf1$Name)
#get rid of anacapa
newdf1 <- newdf1[newdf1$site_id != "12" & newdf1$site_id != "13",]
#reorder for plots
neworder <- c("Gull Island","Hare Rock","Scorpion Anchorage","SE Sea Lion Rookery","Admiral's Reef","Cat Canyon","Fry's Harbor","Arch Point","Johnson's Lee North","Johnson's Lee South","Rodes Reef","Pelican Bay ","Wycoff Ledge","Yellow Banks")
newdf1 <- arrange(mutate(newdf1, Name=factor(Name, levels=neworder)),Name)
head(newdf1)
#add in control/impact
for(i in 1:nrow(newdf1)){
  if(newdf1$site_id[i]=="2" | newdf1$site_id[i]=="6" | newdf1$site_id[i]=="9" | newdf1$site_id[i]=="14"){
    newdf1$site.class[i]<-"Impact"
  }
  else{
    newdf1$site.class[i]<-"Control"
  }
}
head(newdf1)
newdf1$size_class <- factor(newdf1$size_class, levels = c("small", "medium", "large","very large"))

#### Supplementary Figures ####
newdf1.red <- subset(newdf1, Urchin=="red")
head(newdf1.red)
names(newdf1.red)[4] <- "Size class"
str(newdf1.red)

#figure - red size classes (stacked bar) over time
tiff("redsizeclass.tiff", units="in", width=5, height=5, res=300)
ggplot(data=newdf1.red, aes(fill=`Size class`, y=biomass.in.class, x=Year)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Name) +
  theme_classic() +
  theme(legend.position = c(0.75, 0.08)) +
  labs(y=expression(paste("Urchin Biomass g ",m^-2)),x="Year") +
  geom_vline(xintercept=2004) +
  theme(strip.text = element_text(size=7)) +
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7))
dev.off()


newdf1.purple <- subset(newdf1, Urchin=="purple")
names(newdf1.purple)[4] <- "Size class"

#figure - purple size classes (stacked bar) over time
tiff("purplesizeclass.tiff", units="in", width=5, height=5, res=300)
ggplot(data=newdf1.purple, aes(fill=`Size class`, y=biomass.in.class, x=Year)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Name) +
  theme_classic() +
  theme(legend.position = c(0.75, 0.08)) +
  labs(y=expression(paste("Urchin Biomass g ",m^-2)),x="Year") +
  geom_vline(xintercept=2004) +
  theme(strip.text = element_text(size=7)) +
  theme(legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7))
dev.off()

##### Figure - small urchin BACI ####

small <- subset(newdf1, size_class=="small")
tiff("fig4.tiff", units="in", width=5, height=5, res=300)
ggplot(data=small,aes(x=Year,y=biomass.in.class, color=Urchin, group=interaction(Urchin, pre.post))) +
  geom_point(shape=20) +
  geom_smooth(method="lm", size=0.5) + 
  labs(y=expression(paste("Small Urchin Biomass g ",m^-2)),x="Year") +
  facet_wrap(~Name) +
  geom_vline(xintercept=2004) +
  scale_color_manual(values=c("purple","red")) +
  theme_classic() +
  theme(legend.position = c(0.75, 0.08)) +
  theme(axis.text.x = element_text(angle = 90)) + 
  theme(strip.text = element_text(size=7))
dev.off()

#mixed effects model for all small individuals
m.juv <- lme(biomass.in.class ~ pre.post + site.class + site.class*pre.post, random=~Urchin|site_id, data=small)
summary(m.juv)
plot(m.juv)
#cone of residuals - take log
m.juv2 <- lme(log(biomass.in.class+1) ~ pre.post + site.class + site.class*pre.post, random=~Urchin|site_id, data=small)
summary(m.juv2)
plot(m.juv2)
#interaction term is significant (p=0.0352), value is positive
#check for lags
pacf(residuals(m.juv2))
acf(residuals(m.juv2))
#short lags

#add in temporal autocorrelation 
m.juv3 <- lme(log(biomass.in.class+1) ~ pre.post + site.class + site.class*pre.post, random=~Urchin|site_id, cor=corAR1(),data=small)
summary(m.juv3)
plot(m.juv3)

#add in Year in error structure
m.juv4 <- lme(log(biomass.in.class+1) ~ pre.post + site.class + site.class*pre.post, random=~Urchin|site_id/Year, cor=corAR1(),data=small)
summary(m.juv4)
plot(m.juv4)
qqPlot(residuals(m.juv4))

#mixed effects model - small reds
juv.red <- subset(small, Urchin=="red")
head(juv.red)
m.juv.red <- lme(biomass.in.class ~ pre.post + site.class + site.class*pre.post, random=~1|site_id, data=juv.red)
summary(m.juv.red)
plot(m.juv.red)
#cone residuals, use log
m.juv.red2 <- lme(log(biomass.in.class+1.5) ~ pre.post + site.class + site.class*pre.post, random=~1|site_id, data=juv.red)
summary(m.juv.red2)
plot(m.juv.red2)
# interaction term is significant (p=0.0444), value is positive
pacf(residuals(m.juv.red2))
acf(residuals(m.juv.red2))
#short lag, add in AR(1) and Year in error structure
m.juv.red3 <- lme(log(biomass.in.class+1.5) ~ pre.post + site.class + site.class*pre.post, random=~1|site_id/Year, cor=corAR1(),data=juv.red)
summary(m.juv.red3)

#mixed effects model - small purples
juv.purple <- subset(small, Urchin=="purple")
head(juv.purple)
m.juv.purple <- lme(biomass.in.class ~ pre.post + site.class + site.class*pre.post, random=~1|site_id, data=juv.purple)
summary(m.juv.purple)
plot(m.juv.purple) #cone - use log
m.juv.purple2 <- lme(log(biomass.in.class+1) ~ pre.post + site.class + site.class*pre.post, random=~1|site_id, data=juv.purple)
summary(m.juv.purple2)
plot(m.juv.purple2)
qqPlot(residuals(m.juv.purple2))
pacf(residuals(m.juv.purple2)) #only lag of 1
acf(residuals(m.juv.purple2))

#interaction term is not significant, but pre.post is (p=0.0014)
#run without interaction
m.juv.purple3 <- lme(log(biomass.in.class+1) ~ pre.post + site.class, random=~1|site_id, data=juv.purple)
summary(m.juv.purple3)
plot(m.juv.purple3)
#time period is significant (p=0.0024)
#add in AR(1) and Year
m.juv.purple4 <- lme(log(biomass.in.class+1) ~ pre.post + site.class, random=~1|Year/site_id, cor=corAR1(),data=juv.purple)
summary(m.juv.purple4)
plot(m.juv.purple4)
# after adding that in time period loses significance, but barely (p=0.0715)

# showing size distributions for sites 1-16
site1 <- subset(size, site_id==1)
ggplot(site1, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 1") +
  theme_classic()


site2 <- subset(size, site_id==2)
ggplot(site2, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 2") +
  theme_classic()

site3 <- subset(size, site_id==3)
ggplot(site3, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 3") +
  theme_classic()

site4 <- subset(size, site_id==4)
ggplot(site4, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 4") +
  theme_classic()

site5 <- subset(size, site_id==5)
ggplot(site5, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 5")+
  theme_classic()

site6 <- subset(size, site_id==6)
ggplot(site6, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 6")+
  theme_classic()

site7 <- subset(size, site_id==7)
ggplot(site7, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 7")+
  theme_classic()

site8 <- subset(size, site_id==8)
ggplot(site8, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 8")+
  theme_classic()

site9 <- subset(size, site_id==9)
ggplot(site9, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 9")+
  theme_classic()

site10 <- subset(size, site_id==10)
ggplot(site10, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 10")+
  theme_classic()

site11 <- subset(size, site_id==11)
ggplot(site11, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 11")+
  theme_classic()

site12 <- subset(size, site_id==12)
ggplot(site12, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 12")+
  theme_classic()

site13 <- subset(size, site_id==13)
ggplot(site13, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 13")+
  theme_classic()

site14 <- subset(size, site_id==14)
ggplot(site14, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 14")+
  theme_classic()

site15 <- subset(size, site_id==15)
ggplot(site15, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 15")+
  theme_classic()

site16 <- subset(size, site_id==16)
ggplot(site16, aes(x=Size_mm, fill=Urchin)) +
  geom_histogram() +
  scale_fill_manual(values=c("purple","red")) +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 16")+
  theme_classic()

#example: repeat for separate red and purple
red <- subset(size, Urchin=="red")
purple <- subset(size, Urchin=="purple")

site16.r <- subset(red, site_id==16)
ggplot(site16.r, aes(x=Size_mm)) +
  geom_histogram() +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 16 - Red Urchins")

site1.r <- subset(red, site_id==1)
ggplot(site1.r, aes(x=Size_mm)) +
  geom_histogram() +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 1 - Red Urchins")

site2.r <- subset(red, site_id==2)
ggplot(site2.r, aes(x=Size_mm)) +
  geom_histogram() +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 2 - Red Urchins")

site3.r <- subset(red, site_id==3)
ggplot(site3.r, aes(x=Size_mm)) +
  geom_histogram() +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 3 - Red Urchins")

site4.r <- subset(red, site_id==4)
ggplot(site4.r, aes(x=Size_mm)) +
  geom_histogram() +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 4 - Red Urchins")

site5.r <- subset(red, site_id==5)
ggplot(site5.r, aes(x=Size_mm)) +
  geom_histogram() +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 5 - Red Urchins")

site6.r <- subset(red, site_id==6)
ggplot(site6.r, aes(x=Size_mm)) +
  geom_histogram() +
  facet_wrap(~Year) +
  labs(x="Test Diameter (mm)",y="Frequency", title="Site 6 - Red Urchins")

##### Anacapa sites - analyzed separately (marine reserve since 1978) #####
anacapa <- question1data[question1data$IslandCode=="AN",]
anacapa$site_id <- as.factor(anacapa$site_id)
#old anacapa sites monitored from the beginning
anacapa.old <- subset(anacapa, site_id=="11" | site_id=="12" | site_id=="13")
head(anacapa.old)
for(i in 1:nrow(anacapa.old)){
  if(anacapa.old$Year[i]<=2004){
    anacapa.old$pre.post[i]<-0
  }
  else{
    anacapa.old$pre.post[i]<-1
  }
}

#### Figure - anacapa sites urchin biomass over time ####
tiff("fig5.tiff", units="in", width=5, height=5, res=300)
ggplot(anacapa.old, aes(x=Year, y=av.total.biomass, col=Urchin)) +
  geom_point() +
  geom_smooth(method=lm) +
  scale_color_manual(values=c("purple","red")) +
  facet_wrap(~Name) +
  labs(y=expression(paste("Urchin Biomass g ",m^-2)),x="Year") +
  theme_classic() +
  theme(legend.position = c(0.1, 0.87)) +
  theme(axis.text.x = element_text(angle = 90))
dev.off()
#in general, reds are decreasing at Anacapa (but site 11 has highest decrease and that is outside the marine reserve)

#model - total urchin biomass over time at anacapa
anacapalm <- lme(av.total.biomass ~ Year, random=~Urchin|site_id, data=anacapa)
summary(anacapalm)
plot(anacapalm)
#log transform
anacapalm2 <- lme(log(av.total.biomass +1) ~ Year, random=~Urchin|site_id, data=anacapa)
plot(anacapalm2)
qqPlot(residuals(anacapalm2))
summary(anacapalm2)

#ANOVA - difference in means between reserve and non reserve sites (using all anacapa sites)
summary(anacapa)
anova1 <- aov(anacapa$av.total.biomass ~ anacapa$site_id)
summary(anova1)
anova2 <- aov(anacapa$av.total.biomass ~ anacapa$reserve.status)
summary(anova2)
#significant diff between reserve and non reserve

#need to split up by species
anacapa.red <- subset(anacapa, Urchin=="red")
anacapa.purple <- subset(anacapa, Urchin=="purple")
anova3 <- aov(av.total.biomass ~ reserve.status, data=anacapa.red)
summary(anova3)
anova4 <- aov(av.total.biomass ~ reserve.status, data=anacapa.purple)
summary(anova4)
#signif diff remains for both species

#just use sites 11-13 (old sites)
anacapa.red1 <- subset(anacapa.old, Urchin=="red")
anacapa.purple1 <- subset(anacapa.old, Urchin=="purple")
anova5 <- aov(av.total.biomass ~ reserve.status, data=anacapa.red1)
summary(anova5)
#sig diff in mean red biomass between site 11 + 12/13

#ANCOVA on sites 11,12, and 13
ancova <- aov(av.total.biomass ~ Year + reserve.status, data=anacapa.red1)
Anova(ancova, type="III")
summary(lm(av.total.biomass ~ Year*reserve.status, data=anacapa.red1))
#not significant interaction term
fit1 <- aov(av.total.biomass ~ Year*reserve.status, data=anacapa.red1)
summary(fit1)
#not significant interaction term, drop from model
fit2 <- aov(av.total.biomass ~ Year + reserve.status, data=anacapa.red1)
summary(fit2)
anova(fit1,fit2)
#interaction not significant, reserve status is significant 

fit3 <- aov(av.total.biomass ~ Year*reserve.status, data=anacapa.purple1)
summary(fit3)
#significant interaction term
#just for demonstration purposes, dropping interaction term:
fit4 <- aov(av.total.biomass ~ Year+reserve.status, data=anacapa.purple1)
summary(fit4)
anova(fit3, fit4)
#interaction term is significant, significant interaction between reserve status and year
