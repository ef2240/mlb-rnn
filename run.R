# Load packages
library(dplyr)
library(Lahman)

# Define constants
counting_stats <- c("X1B", "X2B", "X3B", "HR", "UIBB", "IBB", "HBP", "SB", "CS")

# Define players in sample (position players debuting since 1988)
eligible_players <- Appearances %>%
  inner_join(Master, by = "playerID") %>%
  group_by(playerID) %>%
  filter(sum(G_p) / sum(G_all) < 0.5 & 
           debut >= "1988-01-01") %>%
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
  filter(playerID %in% eligible_players &
           AB + BB + HBP + SH + SF > 0)

# Engineer Features
engineerFeatures <- function(seasons, counting_stats, master = Master, normalize = TRUE){
  stats <- seasons %>%
    mutate(PA = AB + BB + HBP + SH + SF,
           X1B = H - X2B - X3B - HR,
           UIBB = BB - IBB) %>%
    inner_join(master, by = "playerID") %>%
    mutate(age = calculateSeasonAge(birthMonth, yearID, birthYear)) %>%
    select(playerID, yearID, PA, counting_stats) %>%
    arrange(playerID, yearID) %>%
    group_by(playerID) %>%
    mutate(season_number = row_number())
  if (normalize) {
    stats <- stats %>%
      mutate_at(counting_stats, funs(. / PA))
  }
  return(stats)
}
calculateSeasonAge <- function(birth_month, season_year, birth_year){
  ifelse(birth_month <= 6, season_year - birth_year, season_year - birth_year - 1)
}

engineerFeatures(eligible_seasons, counting_stats = counting_stats)
