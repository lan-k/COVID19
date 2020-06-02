
###create data sources for age adjusted plots
library(tidyverse)
library(runner)
library(stringi)
library(urltools)

usa_ny <- read.csv('https://covidtracking.com/api/v1/states/NY/daily.csv',stringsAsFactors = F) 

dates <- sapply(usa_ny$date, function(s) {stri_sub(s, 5, 2) <- '/'
                              stri_sub(s, 8, 2) <- '/'
                              return(s)})

usa_ny <- usa_ny %>%
  mutate(Date = as.Date(dates,format='%Y/%m/%d' ), region = "USA-New York") %>%
  select(-hospitalized)  %>%   #this is cumulative
  rename(cases =positive ,
         icu = inIcuCurrently ,
         hospitalized = hospitalizedCurrently,
         deaths = death) %>%
  arrange(Date) %>%
  select(Date, cases, deaths, hospitalized, icu, recovered, region)

italy <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/italy/Italy.tsv", 
                  sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "Italy")

italy_lombardy <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/italy/ITA-Lombardia.tsv", 
                           sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "ITA-Lombardia")

spain <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/spain/Spain.tsv",
                  sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "Spain")

spain_madrid <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/spain/ESP-Madrid.tsv",
                         sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "ESP-Madrid") 

##source for madrid hospital data
madrid_hosp <- read.csv("Madrid.csv",stringsAsFactors = F) %>%
  mutate(Date=as.Date(Date, format = "%d/%m/%Y")) %>%
  select(Date, cases, hospitalized, icu)

#replace icu and hosp values for madrid
spain_madrid <- spain_madrid %>%
  select(-c(cases, icu, hospitalized)) %>%
  left_join(madrid_hosp, by="Date") 


####netherlands ICU 

netherlands_icu <- read.csv("https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/nice_ic_by_day.csv", 
                            stringsAsFactors = F) %>%
  mutate(Date = as.Date(Datum, format = "%Y-%m-%d")) %>%
  rename(icu=intakeCount) %>%
  select(Date, icu)

netherlands_hosp <- read.csv("https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/rivm_NL_covid19_hosp_municipality.csv",
                             stringsAsFactors = F) %>%
  mutate(Date=as.Date(Datum, format = "%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(hospitalized = sum(Aantal))  ##cumulative not daily data so not used

netherlands <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/netherlands/Netherlands.tsv",
                        sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "Netherlands") %>%
  select(-icu) %>%
  left_join(netherlands_icu, by="Date")

swiss_geneva <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/switzerland/CHE-Geneva.tsv",
                         sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "CHE-Geneva")

france_iledefrance <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/france/FRA-Ile-de-France.tsv",
                               sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "FRA-Ile-de-France" )


iceland <- read.csv("https://raw.githubusercontent.com/tryggvigy/CoronaWatchIS/master/data/covid_in_is_all.cvs", 
                    stringsAsFactors = F) %>%
  mutate(Date= as.Date(Dagsetning, format = '%Y-%m-%d'), region = "Iceland") %>%
  rename(cases = 5, deaths = 7, recovered = 9, hospitalized = 13, icu=14 ) %>%
  select(Date,cases, deaths, hospitalized, icu,recovered, region)

iceland$icu[iceland$Date < as.Date("2020-03-14", format = "%Y-%m-%d")] <- NA


sweden_icu <- read.csv("Sweden.csv",stringsAsFactors = F)%>%
  mutate(Date=as.Date(Date, format = "%d/%m/%Y")) %>%
  select(Date, icu)

sweden <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/ecdc/Sweden.tsv",
                   sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "Sweden" ) %>%
  select(-icu) %>%
  left_join(sweden_icu, by="Date")


aus <- read.csv("Aus_covid19_data.csv", stringsAsFactors = F) %>%
  mutate(Date=as.Date(Date, format = "%d/%m/%Y")) %>%
  select(-Ventilated) %>%
  rename(cases=Confirmed, deaths = Deaths, region = Country_Region, 
         hospitalized = Hospitalised, icu=ICU, recovered=Recovered)

####save data
region_data <- bind_rows(usa_ny,italy_lombardy, spain_madrid,netherlands,
                         swiss_geneva,france_iledefrance, iceland,sweden, aus) %>%
  select(-time)

save(region_data, file="COVID_region.Rdata")

