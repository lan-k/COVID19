rm(list = ls())

###create rates for age adjusted plots
library(tidyverse)
library(runner)
library(stringi)
library(urltools)


setwd("D:/work/SAHMRI/COVID-19/Data")

#source("D:/work/SAHMRI/COVID-19/R code/data sources.R")  #international data sources

load(file="COVID_region.Rdata")

usa_ny <- region_data %>% filter(region == "USA-New York")
italy_lombardy <- region_data %>% filter(region == "ITA-Lombardia")
spain_madrid <- region_data %>% filter(region == "ESP-Madrid")
netherlands <- region_data %>% filter(region == "Netherlands")
swiss_geneva <- region_data %>% filter(region == "CHE-Geneva")
france_iledefrance <- region_data %>% filter(region == "FRA-Ile-de-France")
iceland <- region_data %>% filter(region == "Iceland")
sweden <- region_data %>% filter(region == "Sweden")
aus <- region_data %>% filter(region == "Australia")

##################Aus states####################
aus_popn = c(8089817,6596039,5094510, 1751963, 2621509, 
             534457,245929, 426704,25365571)/100000  #per 100k
aus_ICU = c(874, 476,413,188,162,50,22,44,2378)
#NSW, Vic, Qld, SA, WA, Tas, NT, ACT, Australia


####age adjusted weights for states compared with SA
weights_aus <- read.csv("Weights states to Aus Ferguson.csv",stringsAsFactors = F)


state_rate <- function(abbrev, posn) {
  ##read in state data and age adjust  
  pop <- aus_popn[posn]
  icu_perpop <- aus_ICU[posn]/pop
  
  wt <- weights_aus %>% filter(state==abbrev)
  
  statefile <- paste0(abbrev,"_covid19_data.csv",sep='')
  
  df <- read.csv(statefile, stringsAsFactors = F) %>%
    filter(!(is.na(Confirmed) & is.na(Deaths) & is.na(Hospitalised) & is.na(ICU))) %>%
    mutate(Date=as.Date(Date, format = "%d/%m/%Y")) %>%
    select(Date, Confirmed,  Deaths, ICU, Hospitalised) %>% 
    rename(cases = Confirmed) %>%
    mutate(state = abbrev,
           day=row_number(),
           day_count = cases - lag(cases),
           day_change = day_count/lag(cases),
           hosp_rate = Hospitalised/pop*wt$weight_hosp,
           ICU_rate = ICU/(pop)*wt$weight_ICU,
           death_rate = Deaths/(pop)*wt$weight_death,
           case_rate = cases/pop,
           cum_rolling_3 = sum_run(
             x = day_count, 
             k = 3, 
             idx = Date)) 
  
  day100 = min(df$day[df$cases >= 100])
  
  
  
  df <- df %>% select(Date,cases, case_rate,hosp_rate,ICU_rate,death_rate,day ) %>%
    mutate(regions = abbrev, ICUbed_rate = icu_perpop*wt$weight_ICU,
           pop = pop, day = day-day100 + 1) %>%
    filter(day >0)
  
  return(df)
  
}

nsw_rate <- state_rate('NSW',1)
vic_rate <-state_rate('Vic',2)
qld_rate <- state_rate('Qld',3)
sa_rate <- state_rate('SA',4)
wa_rate <- state_rate('WA',5)
tas_rate <- state_rate('Tas',6)


#################international########################

popn <- read.csv("Population profiles with ICU.csv", stringsAsFactors = F) %>%
  mutate(icu_perpop = ICUBeds/(populationServed/100000))


#####data for plotting####

weights <- read.csv("Weights world to Aus Ferguson.csv",stringsAsFactors = F)


country_rate <- function(df, label, label2=label){
  #first argument is the dataframe with area level data
  #second argument is country name in populationServed in Population profiles with ICU.csv
  #third argument is the region name if used
  
  wt <- weights %>% filter(country==label)
  pop <-  popn$populationServed[popn$name == label2]/100000 ##area population
  icu_perpop <- popn$icu_perpop[popn$name == label2]
  
  
  
  df <- df %>%
    filter(!(is.na(cases) & is.na(deaths) & is.na(hospitalized) & is.na(icu))) %>%
    mutate(day=row_number(),
           day_count = cases - lag(cases),
           case_rate = cases/pop,
           hosp_rate = hospitalized/pop*wt$weight_hosp,
           ICU_rate = icu/(pop)*wt$weight_ICU,
           death_rate = deaths/(pop)*wt$weight_death,
           cum_rolling_3 = sum_run(
             x = day_count, 
             k = 3, 
             idx = Date))
  
  day100 = min(df$day[df$cases >= 100 & !is.na(df$cases)])
  
  
  df <- df %>%
    select(Date,cases, case_rate,hosp_rate,ICU_rate,death_rate,day ) %>%
    mutate(regions = label2, ICUbed_rate = icu_perpop*wt$weight_ICU, 
           pop = pop, day = day-day100 + 1) %>%
    filter(day >0)
  
  return(df)
  
}

###country rates
# load(file="COVID_overseas.Rdata")
# netherlands <- overseas_data %>%filter(region == "Netherlands")
# france_iledefrance <- overseas_data %>%filter(region == "FRA-Ile-de-France")

#spain_rate <- country_rate(spain,"Spain" )
madrid_rate <-country_rate(spain_madrid,"Spain" ,"ESP-Madrid")
#italy_rate <- country_rate(italy,"Italy" )
lombardy_rate <- country_rate(italy_lombardy,"Italy" ,"ITA-Lombardia")
netherlands_rate <- country_rate(netherlands,"Netherlands" )
geneva_rate <- country_rate(swiss_geneva,"Switzerland","CHE-Geneva" )
#usa_rate <- country_rate(usa,"USA" )
nyc_rate <- country_rate(usa_ny,"USA","USA-New York" )
iledefrance_rate <- country_rate(france_iledefrance, "France","FRA-Ile-de-France")
iceland_rate <- country_rate(iceland, "Iceland")
sweden_rate <- country_rate(sweden, "Sweden")
aus_rate <- country_rate(aus, "Australia")



