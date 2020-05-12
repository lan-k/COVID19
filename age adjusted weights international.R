rm(list = ls())

library(tidyverse)
#library(runner)

###age_adjust to Australian population profile


world_pop <- read.csv("World population by age 2020.csv", stringsAsFactors = F) %>%
  select(-Country.code,-Type,-year)

###populations are per 1000


load(file="china_rate.Rdata")
  


country_pop <- function(label) {
  

  ##remove age categories 95+
  df <- world_pop %>%
    filter(Country == label) %>%
    pivot_longer(-Country, names_to = "age_group", values_to = "Count") %>%
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
    select(-rownum,  -minage, -maxage,-age_group, -Count) %>%
    mutate(agegrp = row_number())

  poptotal = colSums(df[,"pop"])
  df$prop_pop = df$pop/poptotal
  
  return(df)
  
}


calc_weights <- function(ref, adj_pop, abbrev=adj_pop) {
  
  ref_pop <- country_pop(ref)
  
  rel_rate <- china_rate %>%
    filter(age_group != "Total")%>%
    mutate(agegrp=row_number()) %>%
    left_join(ref_pop, by="agegrp") %>%
    mutate(prop_hosprate_ref = prop_pop * prop_hosp80,
           prop_icurate_ref = prop_pop * prop_ICU80,
           prop_deathrate_ref = prop_pop * prop_death80) %>%
    select(age_group, agegrp,prop_hosp80,prop_ICU80,prop_death80,
           prop_hosprate_ref,prop_icurate_ref,prop_deathrate_ref ) %>%
    filter(agegrp != 10)
  
  
  rel_rate_overall <- rel_rate %>%
    summarise(prop_hosprate_ref = sum(prop_hosprate_ref),
              prop_icurate_ref = sum(prop_icurate_ref),
              prop_deathrate_ref = sum(prop_deathrate_ref))
  

  
  df <- country_pop(adj_pop)
  
  df_weight <- df %>%
    left_join(rel_rate, by="agegrp") %>%
    mutate(
           prop_hosprate = prop_hosp80 * prop_pop,
           prop_ICUrate = prop_ICU80 * prop_pop,
           prop_deathrate = prop_death80 * prop_pop) %>%
    summarise(weight_hosp = rel_rate_overall$prop_hosprate_ref/sum(prop_hosprate),
              weight_ICU = rel_rate_overall$prop_icurate_ref/sum(prop_ICUrate),
              weight_death = rel_rate_overall$prop_deathrate_ref/sum(prop_deathrate))
  df_weight$country <- abbrev
  df_weight$reference <- ref
  
  return(df_weight)
  
}



usa <- calc_weights("Australia","United States of America", "USA")
spain <- calc_weights("Australia","Spain")  
italy <- calc_weights("Australia","Italy") 
switzerland <- calc_weights("Australia","Switzerland")
netherlands <- calc_weights("Australia","Netherlands")
china <- calc_weights("Australia","China")
france <- calc_weights("Australia","France")
uk <- calc_weights("Australia","United Kingdom","UK")
iceland <- calc_weights("Australia","Iceland")
sweden <- calc_weights("Australia","Sweden")
brazil <- calc_weights("Australia","Brazil")
russia <- calc_weights("Australia","Russia")

weights_world <- bind_rows(usa, spain, italy,switzerland,netherlands, 
                              france, uk, iceland,sweden, china)


