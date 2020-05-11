library(tidyverse)
library(runner)


###age_adjust

setwd('D:/work/SAHMRI/COVID-19/Data')

# 
# world_pop_all <- read.csv('world pop by age.csv', skip=1) %>%
#   filter(Reference.date..as.of.1.July. == 2020) %>%
#   select(-Index, -Variant,-Notes,-Parent.code) %>%
#   rename(year = Reference.date..as.of.1.July. )

#write.csv(world_pop_all, "World population by age 2020.csv", row.names = F)

world_pop <- read.csv("World population by age 2020.csv", stringsAsFactors = F) %>%
  select(-Country.code,-Type,-year)
###populations are per 1000

SA_Rate <- read.csv('Rates China Ferguson SA.csv', stringsAsFactors = F) %>%
  mutate(agegrp = row_number() )#SA_rates

# SA_Rate <- read.csv('Age adjustment to SA.csv', stringsAsFactors = F) %>%
#   mutate(agegrp = row_number() )

SA_Rate_overall <-  SA_Rate  %>% filter(age_group == 'Total')

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
    left_join(SA_Rate, by="agegrp") %>%
    mutate(prop_pop = pop/poptotal,
           prop_hosprate = hosp_rate_per100k * prop_pop,
           prop_ICUrate = crit_rate_per100k * prop_pop,
           prop_deathrate = death_rate_per100k * prop_pop) %>%
    summarise(weight_hosp = SA_Rate_overall$prop_hosprate_SA/sum(prop_hosprate),
              weight_ICU = SA_Rate_overall$prop_ICUrate_SA/sum(prop_ICUrate),
              weight_death = SA_Rate_overall$prop_deathrate_SA/sum(prop_deathrate))
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
australia <- country_pop("Australia")  ##slightly differnt to weights from ABS ERP


weights_world_SA <- bind_rows(usa, spain, italy,switzerland,netherlands, 
                              france, uk, iceland,sweden, china, australia)
                          
#write.csv(weights_world_SA, "Weights world to SA.csv",row.names=FALSE)
write.csv(weights_world_SA, "Weights world to SA Ferguson.csv",row.names=FALSE)

###Iceland weights are 1.1.6, 1.20,1.22
####on 12/4/2020 there were 8 deaths/1689
