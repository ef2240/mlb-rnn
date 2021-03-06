# Load packages
library(dplyr)
library(lubridate)
library(tidyr)
library(Lahman)
library(R6)
library(logging)

battingDataPreparer <- R6Class(
  public = list(
    data = NULL,
    variables = NULL,
    earliest_debut = NA,
    logger = getLogger('battingDataPreparer'),
    
    initialize = function(season_info_variables, player_info_variables, season_stats, 
                          earliest_debut = 1871){
      basicConfig("INFO")
      self$variables <- list(season_info = season_info_variables,
                             player_info = player_info_variables,
                             season_stats = season_stats)
      self$earliest_debut <- sprintf("%d-01-01", earliest_debut)
      self$data <- private$prepareData()
    }
  ),
  
  private = list(
    prepareData = function(){
      # Filter to eligible seasons
      eligible_players <- private$getEligiblePlayers()
      batting_stats <- Batting %>%
        private$collapseStints() %>% 
        select(-stint)
      self$logger$info("Filter out players with no plate appearances")
      eligible_batting_seasons <- batting_stats %>%
        filter(playerID %in% eligible_players &
                 AB + BB + HBP + SH + SF > 0)
      
      # Prepare data
      stats <- private$engineerStats(eligible_batting_seasons, self$variables$season_stats)
      positions <- private$getPrimaryPosition()
      stats <- inner_join(stats, positions, by = c("playerID", "yearID"))
      data <- private$arrangeData(stats, self$variables$season_info, 
                                  self$variables$player_info, self$variables$season_stats)
      self$logger$info("%d seasons and %d batters in dataset", nrow(data), 
                       n_distinct(data$playerID))
      return(data)
    },
    
    # Define players in sample (position players debuting since 1988)
    getEligiblePlayers = function(){
      self$logger$info("Get batters debuting after %s", self$earliest_debut)
      Appearances %>%
        inner_join(Master, by = "playerID") %>%
        group_by(playerID) %>%
        filter(sum(G_p) / sum(G_all) < 0.5 & 
                 debut >= self$earliest_debut) %>%
        distinct(playerID) %>%
        unlist()
    },
    
    # Function to conform to one row per season
    collapseStints = function(data){
      data %>%
        group_by(playerID, yearID) %>%
        summarize_if(is.numeric, sum) %>%
        ungroup()
    },
    
    # Engineer statistics and prepare data frame
    engineerStats = function(seasons, counting_stats, normalize = TRUE){
      self$logger$info("Engineer stats, age, and season number")
      stats <- seasons %>%
        mutate(PA = AB + BB + HBP + SH + SF,
               X1B = H - X2B - X3B - HR,
               UIBB = BB - IBB) %>%
        inner_join(Master, by = "playerID") %>%
        mutate(age = private$calculateSeasonAge(birthDay, birthMonth, birthYear, yearID)) %>%
        group_by(playerID) %>%
        mutate(season_number = min_rank(yearID)) %>%
        ungroup()
      if (normalize) {
        self$logger$info("Normalize counting stats")
        stats <- stats %>%
          mutate_at(counting_stats, funs(. / PA))
      }
      return(stats)
    },
    
    calculateSeasonAge = function(birth_day, birth_month, birth_year, season_year){
      season_date <- as.Date(sprintf("%d-06-01", season_year))
      birth_date <- as.Date(paste(birth_year, birth_month, birth_day, sep = "-"))
      age <- time_length(season_date - birth_date, unit = "years")
      return(age)
    },
    
    getPrimaryPosition = function(){
      self$logger$info("Get primary position by season")
      Appearances %>%
        private$collapseStints() %>%
        select(-G_all, -GS, -G_batting, -G_defense, -G_ph, -G_pr, -G_of) %>%
        gather(position, games, -playerID, -yearID) %>%
        group_by(playerID, yearID) %>%
        top_n(1, wt = games) %>%
        slice(1) %>%
        ungroup() %>%
        select(-games) %>%
        mutate(position = factor(gsub("G_", "", position)))
    },
    
    arrangeData = function(stats, season_info_fields, player_info_fields, counting_stats){
      stats %>%
        select(playerID, yearID, season_info_fields, player_info_fields, counting_stats) %>%
        arrange(playerID, yearID)
    }
  )
)
