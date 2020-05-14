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
