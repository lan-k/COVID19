

library(tidyverse)
library(runner)


###world populations by age group per 1000
world_pop <- read.csv("./Data/World population by age 2020.csv", stringsAsFactors = F) %>%
  select(-Country.code,-Type,-year) 

#this is the list of countries that can be used in adjustment
pop_list <- world_pop %>% select(Country) 

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
    select(-c(rownum,  minage, maxage,age_group, Count)) %>%
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
           prop_hosprate_ref,prop_icurate_ref,prop_deathrate_ref ) 
  
  
  rel_rate_overall <- rel_rate %>%
    summarise(prop_hosprate_ref = sum(prop_hosprate_ref),
              prop_icurate_ref = sum(prop_icurate_ref),
              prop_deathrate_ref = sum(prop_deathrate_ref))
  
  #weightlist <- list()
  weight <- data.frame()
  for (i in 1:length(as.vector(adj_pop))) {
    df <- country_pop(adj_pop[i])
    
    df_weight <- df %>%
      left_join(rel_rate, by="agegrp") %>%
      mutate(
        prop_hosprate = prop_hosp80 * prop_pop,
        prop_ICUrate = prop_ICU80 * prop_pop,
        prop_deathrate = prop_death80 * prop_pop) %>%
      summarise(weight_hosp = rel_rate_overall$prop_hosprate_ref/sum(prop_hosprate),
                weight_ICU = rel_rate_overall$prop_icurate_ref/sum(prop_ICUrate),
                weight_death = rel_rate_overall$prop_deathrate_ref/sum(prop_deathrate))
    df_weight$country <- abbrev[i]
    df_weight$reference <- ref
    
    #weightlist[[i]] <- df_weight
    weight <- bind_rows(weight,df_weight)
    
  }
  
  #weight <- bind_rows(weightlist)
  return(weight)
  
}

#####example of a function call####
##names must match countries in pop_list
country_list <- c("United States of America","Spain","Italy","Switzerland","Netherlands",
                  "China" ,"France","United Kingdom","Iceland","Sweden","Brazil",
                  "Russian Federation", "Algeria", "Indonesia",	"Iran (Islamic Republic of)")

abbrev_list <- c("USA","Spain","Italy","Switzerland","Netherlands",
                 "China" ,"France","UK","Iceland","Sweden","Brazil",
                 "Russia", "Algeria", "Indonesia","Iran")

weights_world <- calc_weights("China",country_list, abbrev_list)





