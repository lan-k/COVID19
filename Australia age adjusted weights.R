library(tidyverse)
#library(runner)


###age_adjust to Australian population profile

setwd('D:/work/SAHMRI/COVID-19/Data')


world_pop <- read.csv("World population by age 2020.csv", stringsAsFactors = F) %>%
  select(-Country.code,-Type,-year)

Aus_pop <- read.csv('ABS age groups.csv', stringsAsFactors = F) %>%
  select(age_group, Aus) %>%
  mutate(prop_pop_Aus = Aus/max(Aus))


###populations are per 1000

rel_rate <- read.csv('Rates China Ferguson.csv', stringsAsFactors = F) %>%
  mutate(agegrp = row_number() ) %>%
  left_join(Aus_pop, by="age_group") %>%
  mutate(prop_hosprate_Aus = prop_pop_Aus * prop_hosp80,
         prop_icurate_Aus = prop_pop_Aus * prop_ICU80,
         prop_deathrate_Aus = prop_pop_Aus * prop_death80) %>%
  select(agegrp,prop_hosp80,prop_ICU80,prop_death80,
         prop_hosprate_Aus,prop_icurate_Aus,prop_deathrate_Aus ) %>%
  filter(agegrp != 10)
  

rel_rate_overall <- rel_rate %>%
  summarise(prop_hosprate_Aus = sum(prop_hosprate_Aus),
            prop_icurate_Aus = sum(prop_icurate_Aus),
            prop_deathrate_Aus = sum(prop_deathrate_Aus))



country_pop <- function(label, abbrev=label) {
  
  ##remove age categories 95+
  df <- world_pop %>%
    filter(Country == label) %>%
    pivot_longer(-Country, names_to = "Age_group", values_to = "Count") %>%
    mutate(rownum = row_number(),
           minage = (rownum-2)*5,
           maxage = rownum*5 - 1,
           age_group = paste0(paste0(as.character(minage), "-"),as.character(maxage)),
      pop = sum_run(
      x = Count, 
      k = 2, 
      idx = rownum))
  
  
  pop80 <- colSums(df[17:21,"Count"])
  df$pop[df$minage == 80] <- pop80

  df <- df %>% filter((rownum %% 2 == 0))



  df$age_group[df$minage == 80]<- "80+"

  df <- df %>% filter(minage <=80) %>%
    select(-rownum, -Age_group, -minage, -maxage, -Count, -age_group) %>%
    mutate(agegrp = row_number())


  poptotal = colSums(df[,"pop"])


  
  df_weight <- df %>%
    left_join(rel_rate, by="agegrp") %>%
    mutate(prop_pop = pop/poptotal,
           prop_hosprate = prop_hosp80 * prop_pop,
           prop_ICUrate = prop_ICU80 * prop_pop,
           prop_deathrate = prop_death80 * prop_pop) %>%
    summarise(weight_hosp = rel_rate_overall$prop_hosprate_Aus/sum(prop_hosprate),
              weight_ICU = rel_rate_overall$prop_icurate_Aus/sum(prop_ICUrate),
              weight_death = rel_rate_overall$prop_deathrate_Aus/sum(prop_deathrate))
  df_weight$country <- abbrev

  return(df_weight)
  
}

usa <- country_pop("United States of America", "USA")
spain <- country_pop("Spain")  
italy <- country_pop("Italy") 
switzerland <- country_pop("Switzerland")
netherlands <- country_pop("Netherlands")
china <- country_pop("China")
france <- country_pop("France")
uk <- country_pop("United Kingdom","UK")
iceland <- country_pop("Iceland")
sweden <- country_pop("Sweden")
australia <- country_pop("Australia")  ##WHO slightly different to population from ABS ERP


weights_world <- bind_rows(usa, spain, italy,switzerland,netherlands, 
                              france, uk, iceland,sweden, china, australia)
write.csv(weights_world, "Weights world to Aus Ferguson.csv",row.names=FALSE)

