# Load packages
library(dplyr)
library(Lahman)

# Prepare Data
season_stats <- Batting %>%
  select(-stint) %>%
  group_by(playerID, yearID) %>%
  summarize_if(is.numeric, sum) %>%
  ungroup()
calculateSeasonAge = function(birth_month, season_year, birth_year){
  ifelse(birth_month <= 6, season_year - birth_year, season_year - birth_year - 1)
}
season_stats %>%
  inner_join(Master, by = "playerID") %>%
  filter(debut >= "1988-01-01") %>%
  mutate(age = calculateSeasonAge(birthMonth,yearID, birthYear))
