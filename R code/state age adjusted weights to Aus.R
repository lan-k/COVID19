library(dplyr)
library(runner)

###age_adjust

setwd('D:/work/SAHMRI/COVID-19/Data')
# SA_Rate <- read.csv('Age adjustment to SA.csv', stringsAsFactors = F)

Aus_pop <- read.csv('ABS age groups.csv', stringsAsFactors = F) %>%
  #select(age_group, Aus) %>%
  mutate(prop_pop_Aus = Aus/max(Aus))

totals <-  Aus_pop %>% filter(age_group == 'Total')

###populations are per 1000

rel_rate <- read.csv('Rates China Ferguson.csv', stringsAsFactors = F) %>%
  mutate(agegrp = row_number() ) %>%
  left_join(Aus_pop, by="age_group") %>%
  mutate(prop_hosprate_Aus = prop_pop_Aus * prop_hosp80,
         prop_icurate_Aus = prop_pop_Aus * prop_ICU80,
         prop_deathrate_Aus = prop_pop_Aus * prop_death80) %>%
  select(age_group,prop_hosp80,prop_ICU80,prop_death80,
         prop_hosprate_Aus,prop_icurate_Aus,prop_deathrate_Aus ) %>%
  filter(age_group != 'Total')


rel_rate_overall <- rel_rate %>%
  summarise(prop_hosprate_Aus = sum(prop_hosprate_Aus),
            prop_icurate_Aus = sum(prop_icurate_Aus),
            prop_deathrate_Aus = sum(prop_deathrate_Aus))




###weights for each state
weight_fn <- function(state, tot) {
  st <- rlang::parse_expr(state)  #get rid of quotations
  df <- Aus_pop %>%
    select(age_group, !!st) %>%
    right_join(rel_rate, by="age_group") %>%
    mutate(prop_pop = !!st/tot,
           prop_hosprate = prop_hosp80 * prop_pop,
           prop_ICUrate = prop_ICU80 * prop_pop,
           prop_deathrate = prop_death80 * prop_pop)
  
  df_weight <- df %>%
    summarise(weight_hosp = rel_rate_overall$prop_hosprate_Aus/sum(prop_hosprate),
              weight_ICU = rel_rate_overall$prop_icurate_Aus/sum(prop_ICUrate),
              weight_death = rel_rate_overall$prop_deathrate_Aus/sum(prop_deathrate))
  df_weight$state <- state
  
  return(df_weight)
  
}


weights_NSW <- weight_fn("NSW", totals$NSW) #NSW has younger popn than SA, so weights are > 1
weights_Vic <- weight_fn("Vic", totals$Vic)
weights_Qld <- weight_fn("Qld", totals$Qld)
weights_SA <- weight_fn("SA", totals$SA)
weights_WA <- weight_fn("WA", totals$WA)
weights_Tas <- weight_fn("Tas", totals$Tas)
weights_NT <- weight_fn("NT", totals$NT)
weights_ACT <- weight_fn("ACT", totals$ACT)
weights_Aus <- weight_fn("Aus", totals$Aus)

##multiply other state's rates by these weights to get them adjusted for SA's population
weights_Aus <- bind_rows(weights_NSW, weights_Vic, weights_Qld,weights_SA, weights_WA,
                             weights_Tas, weights_NT, weights_ACT, weights_Aus)


write.csv(weights_Aus, "Weights states to Aus Ferguson.csv",row.names=FALSE)


  