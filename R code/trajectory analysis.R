rm(list = ls())

library(dplyr)
library(traj)
library(runner)
library(rworldmap)
library(caret) #try xgboost
#library(tidymodels)


###age_adjust to Australian population profile

setwd('D:/work/SAHMRI/COVID-19/Data')

source('D:/work/SAHMRI/COVID-19/R code/age adjusted weights international.R')

plot_death <- function(country) {
  df <- deaths %>% filter(location == country)
  par(mfrow=c(1,2))
  plot(df$day, df$death_rate_week)
  lines(df$day, df$death_rate_smooth, col="red")
  
  plot(df$day, df$total_death_rate)
  lines(df$day, df$total_death_rate_smooth, col="red")
  
  
}

###weights for all countries in WHO
# weights_all <- calc_weights("China",as.vector(t(pop_list)))
# save(weights_all, file="weights_UN_China.Rdata")
# load("weights_UN_China.Rdata")
# 
# 
# length(t(pop_list))
# 
# country_codes <- world_pop %>%
#   select(Country, Country.code) %>%
#   rename(country=Country)
# 

# 
# weights_ISO <-  weights_all %>%
#   inner_join(country_codes, by="country") %>%
#   inner_join(UN_iso_codes, by=c("Country.code"="UN_code")) %>%
#   arrange(country) %>%
#   select(weight_death, ISO3, Country.code) %>%
#   mutate(ISO3=substr(ISO3,1,3))
# 
# save(weights_ISO, file="weights_ISO_China.Rdata")

load("weights_ISO_China.Rdata")


UN_iso_codes <- read.csv("Country UN ISO codes.csv") %>%
  rename(ISO_country=country)  %>%
  filter(ISO3 != "OWID_WRL") %>%
  mutate(ISO3=substr(ISO3,1,3))


minfu <- 90  #minimum number of days after first death is recorded
mindeath <- 50  #exclude regions with total number of deaths less than this
maxday <- paste0("day",minfu)

#################international########################
#ECDC data
# ecdcdata <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
#                  na.strings = "",  stringsAsFactors = F) %>%
#   mutate(Date = as.Date(dateRep, format = "%d/%m/%Y")) %>%
#   select(Date, cases, deaths, popData2018)  #fileEncoding = "UTF-8-BOM",
# countries <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/ecdc/locations.csv",
#                       stringsAsFactors = F,na.strings = "")
# owid <-  read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",
#   stringsAsFactors = F,na.strings = "") 

owid <-  read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv',
                  stringsAsFactors = F,na.strings = "")                 


deaths <- owid %>%
  mutate( Date = as.Date(date, format = "%Y-%m-%d")) %>%
  select(Date, location, new_deaths, iso_code, population)  %>%
  left_join(weights_ISO, by=c("iso_code" = "ISO3")) %>%
  filter(!is.na(weight_death)) %>%
  group_by(location) %>%
  mutate(mindate = min(Date),
    day = row_number(), #Date-mindate-1,
    new_deaths = replace_na(new_deaths, 0),
    total_deaths = cumsum(new_deaths),
    maxdeath=max(total_deaths),
    mean_deaths_3 = mean_run(
    x = new_deaths, 
    k = 3, 
    idx = Date, na_rm = TRUE),
    mean_deaths_7 = mean_run(
    x = new_deaths, 
    k = 7, 
    idx = Date, na_rm = TRUE),
    total_deaths_7 = mean_run(
      x = total_deaths, 
      k = 7, 
      idx = Date, na_rm = TRUE),
    death_rate_week = weight_death*1000000*mean_deaths_7/population,
    total_death_rate=weight_death*1000000*total_deaths_7/population,
    #total_death_rate_smooth = as.vector(smooth(total_death_rate, kind="S", twiceit=T)),
    #deaths_smooth=as.vector(smooth(mean_deaths_7, kind="S", twiceit=T)),
    #death_rate_smooth = weight_death*1000000*mean_deaths_7/population,
    deathdays =total_deaths >= 1) %>%
  ungroup()

n_nation <- unique(deaths$location)

###spline fit to total death rate
total_death_rate_smooth <- NULL
for (i in seq(length(n_nation))) {
  nation <- deaths %>% filter(location == n_nation[i])
 
  spl <- with(nation, smooth.spline(day,total_death_rate))
  nation$total_death_rate_smooth <- spl$y
  nation$total_death_rate_smooth[nation$total_death_rate_smooth <0] = 0
  
  spl2 <- with(nation, smooth.spline(day,death_rate_week))
  nation$death_rate_smooth <- spl2$y
  nation$death_rate_smooth[nation$death_rate_smooth <0] = 0
  
  
  nation <- nation %>%
    select(Date, iso_code,death_rate_smooth, total_death_rate_smooth)
  
  total_death_rate_smooth <- bind_rows(total_death_rate_smooth, nation)
 
}


deaths <- deaths %>%
  left_join(total_death_rate_smooth, by = c("Date","iso_code")) %>%
  group_by(location) %>%
  filter(deathdays & maxdeath >= mindeath) %>%
  mutate(day=row_number(),
         maxfu = max(day),
  )  %>%
  ungroup() %>%
  select(-deathdays)

summary(deaths$maxfu)
summary(deaths$mean_deaths_3)
summary(deaths$death_rate_smooth)
summary(deaths$total_death_rate_smooth)


plot_death("Brazil") 

#big discontinuity
plot_death("India")  
plot_death("Ireland")
plot_death("Chile")
plot_death("Ecuador")
plot_death("Guatemala")
plot_death("Cameroon")


###trajectory analysis
###using number of deaths
##set up data in the correct format for the traj package
##time data frame
days <- deaths %>%
  filter(maxfu >= minfu & day <= minfu) %>%
  select(day, iso_code) %>%
  pivot_wider(names_from=day,
              names_prefix = "time.",
              values_from =day ) %>%
  # rename(ID=iso_code) %>%
  mutate(ID = as.integer(row_number())) %>%
  as.data.frame()

IDs <- days %>%
  select(ID, iso_code) %>%
  left_join(UN_iso_codes, by=c("iso_code"="ISO3"))

  
days <- days %>% 
  select(-iso_code) 

days <- bind_cols(days$ID,days[,1:minfu] )
colnames(days)[1] <- "ID"

###number of deaths
deaths_traj <- deaths %>%
  filter(maxfu >= minfu & day <= minfu) %>%
  select(day, iso_code, mean_deaths_7) %>%
  pivot_wider(names_from=day,
              names_prefix = "day",
              values_from =mean_deaths_7 ) %>%
  mutate(ID = as.integer(row_number())) %>%
  #rename(ID=iso_code) %>%
  select(-iso_code) %>%
  as.data.frame()

deaths_traj <- bind_cols(deaths_traj$ID,deaths_traj[,1:minfu] )
colnames(deaths_traj)[1] <- "ID"

##step measures
s1 = step1measures(deaths_traj, days, ID = TRUE)
head(s1$measurements)

s2 = step2factors(s1)
head(s2$factors)

#s3 = step3clusters(s2, nclusters=5) 
s3 = step3clusters(s2)

summary(s3)
head(s3$clusters)
s3$clust.distr
plot(s3)

plotMeanTraj(s3, ylim=c(0,3000))
plotMedTraj(s3, ylim=c(0,3000))

###using weighted death rate  - mean deaths past 7 days
death_rate <- deaths %>%
  filter(maxfu >= minfu & day <= minfu) %>%
  select(day, iso_code, death_rate_smooth) %>%
  pivot_wider(names_from=day,
              names_prefix = "day",
              values_from =death_rate_smooth ) %>%
  #rename(ID=iso_code) %>%
  mutate(ID = as.integer(row_number())) %>%
  select(-iso_code)  %>%
  as.data.frame()


death_rate <- bind_cols(death_rate$ID,death_rate[,1:minfu] )
colnames(death_rate)[1] <- "ID"


r1 = step1measures(death_rate, days, ID = TRUE)

r2 = step2factors(r1)
head(r2$factors)

r3 = step3clusters(r2)  
#r3 = step3clusters(r2,nclusters = 3)

clusters <- r3$clusters
death_rate_clusters <- death_rate %>%
  left_join(clusters, by="ID") %>%
  filter(!is.na(cluster)) %>%
  left_join(IDs,by="ID" )

r3$clust.distr
death_rate_clusters %>% filter(cluster !=2) %>% select(cluster,ISO_country)


head(clusters)
summary(r3)

maxclust <- max(death_rate_clusters$cluster)

plot(r3, ylim=c(0,15))

plotMeanTraj(r3, ylim=c(0,2))
plotMedTraj(r3, ylim=c(0,5))
plotBoxplotTraj(r3)
plotCombTraj(r3)  #
plotCombTraj(r3,stat.type = "median", ylim=c(0,1), col=1:maxclust) 
legend("topleft", legend=1:maxclust,
       pch=1:maxclust,lty=1:maxclust, col=1:maxclust)



###total deaths
total_death_rate <- deaths %>%
  filter(maxfu >= minfu & day <= minfu) %>%
  select(day, iso_code, total_death_rate_smooth) %>%
  pivot_wider(names_from=day,
              names_prefix = "day",
              values_from =total_death_rate_smooth ) %>%
  #rename(ID=iso_code) %>%
  mutate(ID = as.integer(row_number())) %>%
  select(-iso_code)  %>%
  as.data.frame()


total_death_rate <- bind_cols(total_death_rate$ID,total_death_rate[,1:minfu] )
colnames(total_death_rate)[1] <- "ID"


t1 = step1measures(total_death_rate, days, ID = TRUE)

t2 = step2factors(t1)
head(t2$factors)

t3 = step3clusters(t2)  
#t3 = step3clusters(r2,nclusters = 4)

clusters <- t3$clusters
(maxclust <- max(clusters$cluster))

total_death_rate_clusters <- death_rate %>%
  left_join(clusters, by="ID") %>%
  filter(!is.na(cluster)) %>%
  left_join(IDs,by="ID" )

t3$clust.distr
total_death_rate_clusters %>% filter(cluster %in% c(1,2)) %>% select(cluster,ISO_country)
total_death_rate_clusters %>% filter(cluster ==3) %>% select(cluster,ISO_country)

head(clusters)
summary(t3)

plot(t3)

plotMeanTraj(t3, ylim=c(0,400))
plotMedTraj(t3, ylim=c(0,500))
plotBoxplotTraj(t3)
plotCombTraj(t3)  #
plotCombTraj(t3,ylim=c(0,350), col=1:maxclust) 
legend("topleft", legend=1:maxclust,
       pch=1:maxclust,lty=1:maxclust, col=1:maxclust)

  
