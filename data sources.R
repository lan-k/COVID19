rm(list = ls())

###create data sources for age adjusted plots
library(tidyverse)
library(runner)
library(stringi)
library(urltools)


setwd("D:/work/SAHMRI/COVID-19/Data")


#################international########################

# usa <- read.csv('https://covidtracking.com/api/us/daily.csv',stringsAsFactors = F) 
# 
# dates <- sapply(usa$date, function(s) {stri_sub(s, 5, 2) <- '/'
#                               stri_sub(s, 8, 2) <- '/'
#                               return(s)})
# 
# usa <- usa %>%
#   mutate(Date = as.Date(dates,format='%Y/%m/%d' )) %>%
#   select(-hospitalized)  %>%   #this is cumulative
#   rename(cases =positive ,
#          icu = inIcuCurrently ,
#          hospitalized = hospitalizedCurrently,
#          deaths = death) %>%
#   arrange(Date) %>%
#   select(Date, cases, deaths, hospitalized, icu, recovered)

usa_ny <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/unitedstates/USA-New%20York.tsv", 
                   sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "USA-New York")

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

##another source for madrid hospital data
madrid_hosp <- read.csv("https://raw.githubusercontent.com/jtamargo/covid19/master/COVID19_hospit_prevalencia.csv",
                        stringsAsFactors = F) %>%
  filter(hospitalizados == "Madrid") %>%
  pivot_longer(-hospitalizados, names_to="time", values_to="hospitalized") %>%
  mutate(time_str = gsub("([-])\\1+","-",paste0("2020-",gsub("\\.","-",gsub("[^0-9.-]", "", time)))),
         Date = as.Date(time_str, format = "%Y-%d-%m")) %>%
  select(Date,hospitalized)

madrid_icu <- read.csv("https://raw.githubusercontent.com/jtamargo/covid19/master/COVID19_UCI_prevalencia.csv",
                       stringsAsFactors = F) %>%
  filter(UCI == "Madrid") %>%
  pivot_longer(-UCI, names_to="time", values_to="icu") %>%
  mutate(time_str = gsub("([-])\\1+","-",paste0("2020-",gsub("\\.","-",gsub("[^0-9.-]", "", time)))),
         Date = as.Date(time_str, format = "%Y-%d-%m")) %>%
  select(Date,icu)


#replace icu and hosp values for madrid
spain_madrid <- spain_madrid %>%
  select(-c(icu, hospitalized)) %>%
  left_join(madrid_hosp, by="Date") %>%
  left_join(madrid_icu, by="Date")

#replace values after 26/4/2020
# max_spain <- min(max(spain_madrid$Date), max(madrid_icu$Date))
# madrid_stop <- as.Date("2020-04-26", format = "%Y-%m-%d")
# spain_madrid$icu[spain_madrid$Date >= madrid_stop & spain_madrid$Date <= max_spain] <- madrid_icu$icu[madrid_icu$Date >= madrid_stop & madrid_icu$Date <= max_spain]
# spain_madrid$hospitalized[spain_madrid$Date >= madrid_stop & spain_madrid$Date <= max_spain] <- madrid_hosp$hospitalized[madrid_hosp$Date >= madrid_stop & madrid_hosp$Date <= max_spain]




####netherlands ICU is here intakecount 

netherlands_icu <- read.csv("https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/nice_ic_by_day.csv", 
                            stringsAsFactors = F) %>%
  mutate(Date = as.Date(Datum, format = "%Y-%m-%d")) %>%
  rename(icu=intakeCount) %>%
  select(Date, icu)

netherlands_hosp <- read.csv("https://raw.githubusercontent.com/J535D165/CoronaWatchNL/master/data/rivm_NL_covid19_hosp_municipality.csv",
                             stringsAsFactors = F) %>%
  mutate(Date=as.Date(Datum, format = "%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(hospitalized = sum(Aantal))  ##is this cumulative?

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

# france_idf <- read.csv("https://raw.githubusercontent.com/opencovid19-fr/data/master/dist/chiffres-cles.csv",
#                    stringsAsFactors = F) %>% 
#   filter(maille_code == "REG-11" ) %>%
#   rename(cases = cas_confirmes, deaths = deces, hospitalized = hospitalises, 
#          icu=reanimation, recovered = gueris) %>%
#   mutate(Date=as.Date(date, format = "%Y-%m-%d"), region = "FRA-Ile-de-France" ) 
# 
# france_idf_cases <-france_idf %>%
#   select(Date, cases) %>%
#   filter(!is.na(cases))
# 
# 
# france_idf_hosp <-france_idf %>%
#   select(Date, deaths, hospitalized, icu, recovered) %>%
#   filter(!is.na(deaths))
# 
# france_iledefrance <- full_join(france_idf_cases,france_idf_hosp, by="Date" )


# iceland <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/iceland/Iceland.tsv",
#                     sep="\t", skip = 3,stringsAsFactors = F) %>%
#   mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "Iceland" )

iceland <- read.csv("https://raw.githubusercontent.com/tryggvigy/CoronaWatchIS/master/data/covid_in_is_all.cvs", 
                    stringsAsFactors = F) %>%
  mutate(Date= as.Date(Dagsetning, format = '%Y-%m-%d'), region = "Iceland") %>%
  rename(cases = 5, deaths = 7, recovered = 9, hospitalized = 13, icu=14 ) %>%
  select(Date,cases, deaths, hospitalized, icu,recovered, region)

iceland$icu[iceland$Date < as.Date("2020-03-14", format = "%Y-%m-%d")] <- NA


#https://raw.githubusercontent.com/tryggvigy/CoronaWatchIS/master/data/covid_in_is_all.cvs
###col 5=  cum cases, col 7 = cum deaths, col 13 = hosp, 14 = ICU



sweden_icu <- read.csv("Sweden.csv",stringsAsFactors = F)%>%
  mutate(Date=as.Date(Date, format = "%d/%m/%Y")) %>%
  select(Date, icu)

sweden <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/ecdc/Sweden.tsv",
                   sep="\t", skip = 3,stringsAsFactors = F) %>%
  mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "Sweden" ) %>%
  select(-icu) %>%
  left_join(sweden_icu, by="Date")

# aus <- read.csv("https://raw.githubusercontent.com/neherlab/covid19_scenarios/master/data/case-counts/ecdc/Australia.tsv",
#                 sep="\t", skip = 3,stringsAsFactors = F) %>%
#   mutate(Date=as.Date(time, format = "%Y-%m-%d"), region = "Australia") %>%
#   select(-icu,-hospitalized,-recovered)

# aus_icu <- read.csv("Aus_covid19_data.csv") %>%
#   mutate(Date=as.Date(Date, format = "%d/%m/%Y")) %>%
#   select(Date, ICU, Hospitalised,Recovered) %>% 
#   rename(hospitalized = Hospitalised, icu=ICU, recovered=Recovered)
# 
# aus <- left_join(aus,aus_icu, by="Date")

aus <- read.csv("Aus_covid19_data.csv", stringsAsFactors = F) %>%
  mutate(Date=as.Date(Date, format = "%d/%m/%Y")) %>%
  select(-Ventilated) %>%
  rename(cases=Confirmed, deaths = Deaths, region = Country_Region, 
         hospitalized = Hospitalised, icu=ICU, recovered=Recovered)

####save overseas data
region_data <- bind_rows(usa_ny,italy_lombardy, spain_madrid,netherlands,
                         swiss_geneva,france_iledefrance, iceland,sweden, aus) %>%
  select(-time)

save(region_data, file="COVID_region.Rdata")
save(netherlands, file="netherlands.Rdata")
save(france_iledefrance, file="france_iledefrance.Rdata")
save(iceland, file="iceland.Rdata")
save(spain_madrid, file="madrid.Rdata")
#save(spain_madrid, file="madrid_26042020.Rdata")
