dataConformer <- R6Class(
  classname = "dataConformer",
  public = list(
    variables = list(),
    data_with_dummies = NA,
    num_seasons = NA,
    test_year = NA,
    one_hot_sep = "__",
    logger = getLogger('dataConformer'),
    
    initialize = function(data, num_seasons, test_year){
      data_with_dummies <- private$createDummies(data$data, unlist(data$variables))
      data$variables$season_info <- private$getAllVariableNames(data$variables$season_info, 
                                                                colnames(data_with_dummies))
      data$variables$player_info <- private$getAllVariableNames(data$variables$player_info, 
                                                                colnames(data_with_dummies))
      self$variables <- data$variables
      self$test_year <- test_year
      self$num_seasons <- num_seasons

      self$data_with_dummies <- data_with_dummies[order(data_with_dummies$playerID,
                                                        data_with_dummies$yearID), ]
    },
    
    getConformedBuildData = function(type){
      self$logger$info("Conforming %s data", type)
      
      max_season_num <- max(self$data_with_dummies$season_number)
      model_data <- sapply(1:max_season_num, private$getConformedDataSingle, type = type)
      
      self$logger$info("Combining data for all season numbers")
      data <- apply(model_data[-1, ], 1, abind, along = 1)
      item_ids <- bind_rows(model_data["item_ids", ])
      return(list(data = data,
                  item_ids = item_ids,
                  specifications = list(num_seasons = self$num_seasons, 
                                        test_year = self$test_year)))
    }
  ),
  
  private = list(
    
    createDummies = function(data, variables, sep = self$one_hot_sep){
      self$logger$info("Finding variables which need to be dummied")
      dummies <- private$inferDummies(data, variables)
      self$logger$info("Dummy variables found: %s", dummies)
      levels <- lapply(data[, dummies], levels)
      data_with_dummies <- private$oneHotEncode(data, levels, sep)
      return(data_with_dummies)
    },
    inferDummies = function(data, variables){
      variables <- unique(unlist(variables))
      data_variables <- data[, variables]
      multiple_levels <- sapply(data_variables, private$testMultipleLevels)
      multiple_levels <- names(multiple_levels)[multiple_levels]
      return(multiple_levels)
    },
    testMultipleLevels = function(vec){
      return((is.character(vec) | is.factor(vec)) & length(unique(vec)) > 2)
    },
    oneHotEncode = function(data, levels, sep){
      self$logger$info("One hot encoding %s", names(levels))
      levels <- data.frame(variable = rep(names(levels), sapply(levels, length)),
                           value = unlist(levels),
                           stringsAsFactors = FALSE)
      for (row in seq_len(nrow(levels))){
        variable <- levels$variable[row]
        value <- levels$value[row]
        new_column <- paste(variable, value, sep = sep)
        data[[new_column]] <- as.integer(data[[variable]] == value)
      }
      data[, unique(levels$variable)] <- NULL
      return(data)
    },
    
    getAllVariableNames = function(variables, data_columns){
      all_variables <- lapply(variables, private$getVariableNames, data_columns = data_columns)
      return(unlist(all_variables))
    },
    getVariableNames = function(variable, data_columns){
      if (variable %in% data_columns){
        return(variable)
      } else {
        return(paste(variable, self$levels[[variable]], sep = self$one_hot_sep))
      }
    },
    
    getConformedDataSingle = function(season_num, type){
      self$logger$info("Conforming data for season_number %d", season_num)
      item_ids <- private$getItemIds(season_num = season_num, type = type)
      player_info <- private$getPlayerInfo(season_num = season_num, ids = item_ids)
      season_sequence <- private$getSeasonSequence(season_num = season_num, ids = item_ids)
      next_season_stats <- private$getNextSeasonStats(season_num = season_num, ids = next_ids)
      return(list(item_ids = item_ids,
                  player_info = player_info,
                  season_sequence = season_sequence,
                  next_season_stats = next_season_stats))
    },
    getItemIds = function(season_num, type, data = self$data_with_dummies, test_year = self$test_year){
      item_ids <- subset(data, season_number == season_num)
      if (type == "train"){
        item_ids <- subset(item_ids, yearID < test_year)
      } else {
        item_ids <- subset(item_ids, yearID == test_year)
      }
      item_ids <- item_ids[, c("playerID", "yearID")]
      self$logger$info("%d seasons found", nrow(item_ids))
      return(item_ids)
    },
    getPlayerInfo = function(season_num, ids, vars = self$variables$player_info, 
                             data = self$data_with_dummies){
      player_info <- inner_join(data, ids, by = c("playerID", "yearID"))
      player_info <- player_info[, vars]
      player_info <- as.matrix(player_info)
      player_info[is.na(player_info)] <- 0
      return(season_info)
    },
    getSeasonSequence = function(season_num, item_ids, num_seasons = self$num_seasons, 
                                 vars = c(self$variables$season_stats, self$variables$season_info), 
                                 data = self$data_with_dummies){
      filtered <- data %>%
        inner_join(item_ids, by = c("playerID", "yearID"))
      if (season_num == 1) {
        num_samples <- nrow(filtered)
        stat_sequence <- array(0, c(num_samples, num_seasons, length(vars)))
      } else {
        stat_sequence <- subset(data, season_number < season_num & playerID %in% filtered$playerID)
        first_season <- max(1, season_num - num_seasons)
        last_season <- max(num_seasons, season_num - 1)
        stat_sequence <- complete(stat_sequence, playerID, season_number = first_season:last_season)
        stat_sequence$season_number <- ifelse(stat_sequence$season_number >= season_num, 0, stat_sequence$season_number) # So the newly completed rows will be all 0s and be masked
        stat_sequence <- stat_sequence[, vars]
        stat_sequence <- array(unlist(stat_sequence, use.names = FALSE), 
                               dim = c(num_seasons, nrow(stat_sequence) / num_seasons, length(vars)))
        stat_sequence <- aperm(stat_sequence, c(2, 1, 3))
        dimnames(stat_sequence) <- list(NULL, NULL, vars)
        stat_sequence[is.na(stat_sequence)] <- 0
      }
      return(stat_sequence)
    },
    getNextSeasonStats = function(season_num, ids, vars = self$variables$season_stats, data = self$data_with_dummies){
      next_season_stats <- inner_join(data, ids, by = c("playerID", "yearID"))
      next_season_stats <- next_season_stats[[var]]
      return(next_season_stats)
    }
  )
)
