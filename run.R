# Run
batting_data <- battingDataPreparer$new(season_info_variables = c("position", "age", "season_number", "PA"),
                                        player_info_variables = c("weight", "height", "bats"),
                                        season_stats = c("X1B", "X2B", "X3B", "HR", "UIBB", "IBB", "HBP", "SB", "CS"),
                                        earliest_debut = 1988)
data_conformer <- dataConformer$new(batting_data, 
                                    num_seasons = 7, 
                                    test_year = 2015)
data_conformer$getConformedBuildData(type = "train")
