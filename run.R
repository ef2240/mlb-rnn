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

# Engineer statistics and prepare data frame
engineerStats <- function(seasons, counting_stats, normalize = TRUE){
  stats <- seasons %>%
    mutate(PA = AB + BB + HBP + SH + SF,
           X1B = H - X2B - X3B - HR,
           UIBB = BB - IBB) %>%
    inner_join(Master, by = "playerID") %>%
    mutate(age = calculateSeasonAge(birthDay, birthMonth, birthYear, yearID)) %>%
    group_by(playerID) %>%
    mutate(season_number = min_rank(yearID))
  if (normalize) {
    stats <- stats %>%
      mutate_at(counting_stats, funs(. / PA))
  }
  return(stats)
}
calculateSeasonAge <- function(birth_day, birth_month, birth_year, season_year){
  season_date <- as.Date(sprintf("%d-06-01", season_year))
  birth_date <- as.Date(paste(birth_year, birth_month, birth_day, sep = "-"))
  age <- time_length(season_date - birth_date, unit = "years")
  return(age)
}
arrangeData <- function(stats, counting_stats){
  stats %>%
    select(playerID, yearID, season_number, age, PA, counting_stats) %>%
    arrange(playerID, yearID)
}
prepareDataFrame <- function(seasons, counting_stats){
  stats <- engineerStats(seasons, counting_stats)
  arrangeData(stats, counting_stats)
}
prepareDataFrame(eligible_seasons, counting_stats)
