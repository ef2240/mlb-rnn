# Load packages
library(dplyr)
library(Lahman)

# Define players in sample (position players debuting since 1988)
eligible_players <- Appearances %>%
  inner_join(Master, by = "playerID") %>%
  group_by(playerID) %>%
  filter(sum(G_p) / sum(G_all) < 0.5 & debut >= "1988-01-01") %>%
  distinct(playerID) %>%
  unlist()

# Conform to one row per season
season_stats <- Batting %>%
  select(-stint) %>%
  group_by(playerID, yearID) %>%
  summarize_if(is.numeric, sum) %>%
  ungroup()

# Filter to eligible seasons
eligible_seasons <- season_stats %>%
  filter(playerID %in% eligible_players)

# Engineer Features
engineerFeatures <- function(seasons, master = Master){
  seasons %>%
    inner_join(master, by = "playerID") %>%
    mutate(age = calculateSeasonAge(birthMonth,yearID, birthYear),
           PA = AB + BB + HBP + SH + SF)
}
calculateSeasonAge <- function(birth_month, season_year, birth_year){
  ifelse(birth_month <= 6, season_year - birth_year, season_year - birth_year - 1)
}

engineerFeatures(eligible_seasons)
