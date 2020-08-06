rm(list = ls())

library(dplyr)
library(traj)
library(runner)

mindeath <- 25 #exclude regions with total number of deaths less than this 100 days after first death

owid <-  read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv',
    stringsAsFactors = F,na.strings = "")
load("./Data/weights_ISO_China.Rdata")
UN_iso_codes <- read.csv("./Data/ISO3 to UN code.csv",stringsAsFactors = F) %>%
     mutate(ISO3=substr(ISO3,1,3))


deaths <- owid %>%
  mutate( Date = as.Date(date, format = "%Y-%m-%d")) %>%
  #mutate(weight_death=1) %>%
  select(Date, location, new_deaths, iso_code, population)  %>%
  left_join(weights_ISO, by=c("iso_code" = "ISO3")) %>%
  filter(!is.na(weight_death) & !is.na(iso_code)) %>%
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
    total_deaths_3 = mean_run(
      x = total_deaths, 
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
    mean_deaths_14 = mean_run(
      x = new_deaths, 
      k = 14, 
      idx = Date, na_rm = TRUE),
    total_deaths_14 = mean_run(
      x = total_deaths, 
      k = 14, 
      idx = Date, na_rm = TRUE),
    death_rate_week = weight_death*1000000*mean_deaths_7/population,
    death_rate_14 = weight_death*1000000*mean_deaths_14/population,
    total_death_rate=weight_death*1000000*total_deaths_14/population,
    deathdays =total_deaths >= 1) %>%
  ungroup()

n_nation <- unique(deaths$location)
length(n_nation)


###spline fit to smooth death rates
total_death_rate_smooth <- NULL
for (i in seq(length(n_nation))) {
  nation <- deaths %>% filter(location == n_nation[i])
 
  if (max(nation$maxdeath) > 0) {
    spl <- with(nation, smooth.spline(day,total_death_rate, spar=0.3)) #
    nation$total_death_rate_smooth <- spl$y
    nation$total_death_rate_smooth[nation$total_death_rate_smooth <0] = 0
    
    
    spl2 <- with(nation, smooth.spline(day,death_rate_14)) #, spar=0.6
    nation$death_rate_smooth <- spl2$y
    nation$death_rate_smooth[nation$death_rate_smooth <0] = 0
    
  } else {
    nation$total_death_rate_smooth <- 0
    nation$death_rate_smooth <- 0
  }

  nation <- nation %>%
    select(Date, iso_code,death_rate_smooth, total_death_rate_smooth)
  
  total_death_rate_smooth <- bind_rows(total_death_rate_smooth, nation)
 
}


deaths <- deaths %>%
  left_join(total_death_rate_smooth, by = c("Date","iso_code")) %>%
  group_by(location) %>%
  filter(deathdays) %>%
  mutate(day=row_number(),
         maxfu = max(day))  %>%  
  select(-deathdays) %>%
  ungroup() 

day100 <- deaths %>%
  group_by(location) %>%
  filter(day==100) %>%
  mutate(death100=total_deaths) %>%
  select(location, death100)


deaths <- deaths %>%
  left_join(day100, by="location") %>%
  filter(death100 >= mindeath)


###trajectory analysis

trajt <- function(minfu, s = F) {
  maxday <- paste0("day",minfu)
  
  d <- deaths %>%
    group_by(location) %>%
    filter(maxfu >= minfu & day <= minfu) %>%
    arrange(location , day) %>%
    ungroup()
  
  days <- d %>%
    select(day, iso_code) %>%
    pivot_wider(names_from=day,
                names_prefix = "time.",
                values_from =day ) %>%
    mutate(ID = as.integer(row_number())) %>%
    as.data.frame()
  
  IDs <- days %>%
    select(ID, iso_code) %>%
    left_join(UN_iso_codes, by=c("iso_code"="ISO3"))
  
  
  days <- days %>% 
    select(-iso_code) 
  
  days <- bind_cols(days$ID,days[,1:minfu] )
  colnames(days)[1] <- "ID"
  
  ###total deaths
  total_death_rate <- d %>% 
    select(day, iso_code, total_death_rate_smooth) %>%
    pivot_wider(names_from=day,
                names_prefix = "day",
                values_from =total_death_rate_smooth ) %>%
    mutate(ID = as.integer(row_number())) %>%
    select(-iso_code)  %>%
    as.data.frame()
  
  
  total_death_rate <- bind_cols(total_death_rate$ID,total_death_rate[,1:minfu] )
  colnames(total_death_rate)[1] <- "ID"
  
  t3 <- wrapperTraj(total_death_rate, days, ID = TRUE)
  print(t3)
  
  total_clusters <- t3$clusters
  (maxclust <- max(total_clusters$cluster))
  
  total_death_rate_clusters <- total_death_rate %>%
    left_join(total_clusters, by="ID") %>%
    filter(!is.na(cluster)) %>%
    left_join(IDs,by="ID" ) %>%
    rename(cluster_total_death = cluster)
   
  print(summary(t3))
  print(t3$clust.distr)
  
  fn <- paste0(minfu," day trajectory.Rdata" )
  if (s)
    save(total_death_rate_clusters, file=fn)
  
  return(t3)
   
}

###50 day trajectories
traj50 <- trajt(50,s=F)  #1=68, 2=45
save(traj50, file="traj50.Rdata")

##100 day trajectories
traj100 <- trajt(100, s=T) 
save(traj100, file="./Data/traj100.Rdata")
