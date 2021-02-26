#original raw dataset of urchin counts 1991-2016
#NB: some of the original data wrangling had many more columns
#because I was examining environmental variables as well.  the files
#were modified and saved to include these variables.  as a result some 
#column subsetting may not work.

count <- read.csv("quad_urchins1.csv")
head(count)
str(count)
count$site_id <- as.factor(count$site_id)

#chart of site info from KFM
site.info <- read.csv("Sites_KFM.csv")
head(site.info)
str(site.info)
#delete unnecessary columns
site.info <- site.info[,-c(1,2,8)]
#keep names consistent
names(site.info)[1]<-"site_id"
site.info$site_id <- as.factor(site.info$site_id)
#merge counts with associated site information
count2 <- merge(count, site.info, by="site_id", type="full")
head(count2)
str(count2)

#this part was not necessary for BACI analysis but was used when looking
#at fishing effort in the associated blocks containing sites
#catch data from CDFG
catch <- read.csv("FishingBlocks_Catch.csv", header=T)
head(catch) # added x before years, missing data, NAs should be zero
catch[is.na(catch)] <- 0
str(catch) # all numbers - blocks should be factor 
catch$Block <- as.factor(catch$Block)  
#each year is a column
names(catch)[2:27] <- 1991:2016
head(catch)
#change dataframe so each row is one data point from one year
catch.melt <- melt(catch, id="Block")
head(catch.melt)
names(catch.melt)[1:3] <- c("Fishing.Block","Year","Annual.catch")

#merge counts with annual catch
count.all <- merge(count2, catch.melt, by= c("Fishing.Block", "Year"))
head(count.all)
str(count.all)
summary(count.all)
#make sure fishing block is a factor
count.all$Fishing.Block <- as.factor(count.all$Fishing.Block)

#time.reserve = time since marine reserve established
#used 2004 as year 1 (2003 = year 0)
count.all$time.reserve <- NULL
for(i in 1:nrow(count.all)){
  if (count.all$site_id[i] == "33" |
      count.all$site_id[i] == "12"|
      count.all$site_id[i] == "13") {
    count.all$time.reserve[i] <- count.all$Year[i] - 1977
  } else if (count.all$site_id[i] == "2" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "23" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "24" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "25" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "6" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "27" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "28" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "9" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "37" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "36" & count.all$Year[i] >= 2004 | count.all$site_id[i] == "14" & count.all$Year[i] >= 2004) {
    count.all$time.reserve[i] <- count.all$Year[i] - 2003
  } else {
    count.all$time.reserve[i] <- 0
  }
}

str(count.all)
summary(count.all)
count.all <- count.all[!(count.all$site_id %in% c("17", "18", "19","20")),] #remove San Clemente sites
count.all[count.all$site_id=="20",] # just checking

#let's make sure counts are integers
count.all$count..m.2 <- round(count.all$count..m.2,digits=0)
head(count.all)
summary(count.all)
str(count.all)

#read in size information
rawbiomass <- read.csv("KFM_Raw_Biomass_Edited1.csv")
head(rawbiomass)
#remove column with Xs (due to edit in excel)
rawbiomass<-rawbiomass[-1]
rawbiomass$site_id <- as.factor(rawbiomass$site_id)
str(rawbiomass)
#remove San Clemente sites
rawbiomass2 <- rawbiomass[!(rawbiomass$site_id %in% c("17", "18", "19","20")),]

#add columns converting size info to mass
rawbiomass2$Wet.mass <- NULL
for(i in 1:length(rawbiomass2$Size_mm)){
  if(rawbiomass2$Urchin[i]=="red"){
    rawbiomass2$Wet.mass[i] <- .00059 * (rawbiomass2$Size_mm[i])^2.917
  }
  else if(rawbiomass2$Urchin[i]=="purple"){
    rawbiomass2$Wet.mass[i] <- .00059 * (rawbiomass2$Size_mm[i])^2.870
  }
}

#find average yearly 
mass <- group_by(rawbiomass2, site_id, Year, Urchin)
av.biomass <- summarize(mass, individuals.sized = n(), av.wet.mass = mean(Wet.mass, na.rm=T), av.size = mean(Size_mm, na.rm=T))
av.biomass <- as.data.frame(av.biomass)
head(av.biomass)
#great now do the same for count
count <- group_by(count.all, site_id, Year, Urchin)
av.count <- summarize(count, total.count=sum(count), Nquads = n(), av.count.m2 = mean(count..m.2, na.rm=T))
head(av.count)
av.count <- as.data.frame(av.count)
summary(av.count)
# merge
all.data <- merge(av.biomass, av.count, by= c("site_id", "Year", "Urchin"))
head(all.data)
str(all.data)
#create new column for average biomass 
all.data$av.total.biomass <- all.data$av.wet.mass * all.data$av.count.m2


#adding 2017-2019 to biomass since 1991
#starting with size information - raw data from KFM
new.size <- read.csv("urchins sizes_2017_2019.csv")
head(new.size)
#rename columns to match
names(new.size)[c(1,5,9)] <- c("site_id", "Name","Year")
#select only purple and red urchin sizes
new.size <- new.size[new.size$Urchin=="purple" | new.size$Urchin =="red",]

# create dataframe 
new.size2 <- as.data.frame(expandRows(new.size, "NoOfInd"))

# find average size at each site*year combination. maintain columns for
#site, island (code and name), site name, year, and urchin species
new.yearly.size <- new.size2 %>%
  group_by(site_id, IslandCode, IslandName, Name,Year, Urchin) %>%
  summarise(av.size = mean(Size_mm), sample.size = n(), sd.wetmass=sd(Size_mm))
new.yearly.size <- as.data.frame(new.yearly.size)

# using formulas from Reed et al. to convert sizes to biomass
for(i in 1:length(new.yearly.size$av.size)){
  if(new.yearly.size$Urchin[i]=="red"){
    new.yearly.size$av.wetmass[i] <- .00059 * (new.yearly.size$av.size[i])^2.917
  }
  else if(new.yearly.size$Urchin[i]=="purple"){
    new.yearly.size$av.wetmass[i] <- .00059 * (new.yearly.size$av.size[i])^2.870
  }
}

#read in raw data from KFM for counts
new.biomass <- read.csv("1m_urchins_RawData_2017-2019.csv")
head(new.biomass)

#rename columns
names(new.biomass)[c(1,5,8,9)] <- c("site_id", "Name", "Urchin","Year")

#yearly summary values for each site*year combination
#total count, average count per m^2, standard deviation, and number of quadrats sampled
new.yearly.biomass <- new.biomass %>%
  group_by(site_id, IslandCode, IslandName, Name, Year, Urchin) %>%
  summarise(total.count = sum(Total),av.count.m2 = mean(Urch.m.2),sd.count=sd(Urch.m.2), Nquads=2*n())
new.yearly.biomass <- as.data.frame(new.yearly.biomass)
head(new.yearly.biomass)
#delete Coronado urchin
new.yearly.biomass <- new.yearly.biomass[new.yearly.biomass$Urchin != "Coronado urchin",]
summary(new.yearly.biomass)
#merge average yearly sizes with average yearly biomass at each site
new.yearly.biomass2 <- merge(new.yearly.size, new.yearly.biomass)
#new column for average total biomass as a product of average mass and average count per m^2 
new.yearly.biomass2$av.total.biomass <- new.yearly.biomass2$av.wetmass * new.yearly.biomass2$av.count.m2
head(new.yearly.biomass2)
#make sure site is a factor
new.yearly.biomass2$site_id <- as.factor(new.yearly.biomass2$site_id)
#delete extraneous columns
new.yearly.biomass2 <- new.yearly.biomass2[,-7]
new.yearly.biomass2 <- new.yearly.biomass2[,-3]

#create columns that are found in original dataframe (1991-2016)
new.yearly.biomass2$latitude <- NA
new.yearly.biomass2$longitude <- NA
new.yearly.biomass2$Depth <- NA

#fill in time since became a marine reserve
new.yearly.biomass2$time.reserve <- NA
for(i in 1:nrow(new.yearly.biomass2)){
  if (new.yearly.biomass2$site_id[i] == "33" |
      new.yearly.biomass2$site_id[i] == "12"|
      new.yearly.biomass2$site_id[i] == "13") {
    new.yearly.biomass2$time.reserve[i] <- new.yearly.biomass2$Year[i] - 1977
  } else if (new.yearly.biomass2$site_id[i] == "2" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "23" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "24" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "25" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "6" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "27" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "28" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "9" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "37" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "36" & new.yearly.biomass2$Year[i] >= 2004 | new.yearly.biomass2$site_id[i] == "14" & new.yearly.biomass2$Year[i] >= 2004) {
    new.yearly.biomass2$time.reserve[i] <- new.yearly.biomass2$Year[i] - 2003
  } else {
    new.yearly.biomass2$time.reserve[i] <- 0
  }
}

new.yearly.biomass2$reserve.status <- NA #this value is filled in in paper script
new.yearly.biomass2$Annual.catch <- NA #did not update this info for BACI
head(new.yearly.biomass2)

#manually merged new.yearly.biomass2 with original yearly biomass in excel
#since there was an error with rbind
#named file Biomass_1991_2019.csv