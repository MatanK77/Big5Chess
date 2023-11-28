# Necessary Packages
library(tidyverse)
library(chessR)
library(reactablefmtr)


# Acquiring Blitz Data
a_magnus <- get_raw_chessdotcom(usernames = "MagnusCarlsen", year_month = c(202111:202311))

a_hikaru <- get_raw_chessdotcom(usernames = "Hikaru", year_month = c(202111:202311)) 

a_nihal <- get_raw_chessdotcom(usernames = "nihalsarin", year_month = c(202111:202311)) 

a_danya <- get_raw_chessdotcom(usernames = "DanielNaroditsky", year_month = c(202111:202311)) 

a_alireza <- get_raw_chessdotcom(usernames = "Firouzja2003", year_month = c(202111:202311)) 


# Combining and Filtering for 3+0 Regular Blitz With Elo Diff Between -300 and 300
a_top <- bind_rows(a_magnus, a_hikaru, a_nihal, a_danya, a_alireza) %>%
  mutate(
    WhiteElo = as.numeric(WhiteElo),
    BlackElo = as.numeric(BlackElo),
    elo_diff = WhiteElo - BlackElo
  ) %>%
  filter(GameRules == "chess" & TimeClass == "blitz" & TimeControl == 180 & between(elo_diff, -300, 300))
 
# Adding Some Columns for Player's Elo Difference, Identifying Their Elo, Result Score, and Expected Result
 a_top <- a_top %>%
  mutate(
    player_elo_diff = case_when(
      Username == White ~ WhiteElo - BlackElo,
      Username == Black ~ BlackElo - WhiteElo
    ),
    player_elo = case_when(
      Username == White ~ WhiteElo,
      Username == Black ~ BlackElo
    ),
    player_result_value = case_when(
      Username == White & Result == "1-0" ~ 1,
      Username == Black & Result == "0-1" ~ 1,
      Username == White & Result == "0-1" ~ 0,
      Username == Black & Result == "1-0" ~ 0,
      TRUE ~ 0.5
    ),
    elo_expected_score = 1 - (1 / (1 + 10^(player_elo_diff / 400))),
    .after = BlackElo
  )
 
 
 # Player Density Plots by Rating Difference
 a_top %>%
   ggplot(aes(x = player_elo_diff)) +
   geom_density()
 
 a_top %>%
   ggplot(aes(x = player_elo_diff)) +
   geom_density() +
   facet_wrap(~Username) +
   labs(title = "Big 5 Often Have a Big Rating Advantage in 3+0, Especially Hikaru", x = "Rating Advantage",
        caption = "3+0 Blitz Last 2 Years. Data From Chesscom API using chessR")
 
 # Score vs Expected by Rating Diff Plots
 a_top %>%
   ggplot(aes(x = player_elo_diff, y = player_result_value)) +
   geom_smooth() +
   geom_line(aes(x = player_elo_diff, y = elo_expected_score)) +
   scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1)) +
   scale_x_continuous(breaks = c(-200, -100, 0, 100, 200, 300), limits = c(-200, 300)) +
   labs(title = "Big 5 Underperform Expectation vs Similar Competition and Overperform vs Weaker Players", x = "Rating Advantage",
        y = "Expected Score (Win is 1)",caption = "Black Line is Elo-Based Expected Score. 3+0 Blitz Last 2 Years. Data From Chesscom API using chessR")
 
 a_top %>%
   ggplot(aes(x = player_elo_diff, y = player_result_value)) +
   geom_smooth(aes(color = Username), se = FALSE, alpha = 0.6) +
   geom_line(aes(x = player_elo_diff, y = elo_expected_score), linewidth = 1.5) +
   scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0, 1)) +
   scale_x_continuous(breaks = c(-200, -100, 0, 100, 200, 300), limits = c(-200, 300)) +
   labs(title = "Big 5 Indvidual Score Trends by Rating Advantage", x = "Rating Advantage",
        y = "Expected Score (Win is 1)", caption = "Black Line is Elo-Based Expected Score. 3+0 Blitz Last 2 Years. Data From Chesscom API using chessR")
 
 
 # Acquiring Title Data
 GM <- jsonlite::fromJSON("https://api.chess.com/pub/titled/GM") %>%
   data.frame()
 IM <- jsonlite::fromJSON("https://api.chess.com/pub/titled/IM") %>%
  data.frame()
 FM <- jsonlite::fromJSON("https://api.chess.com/pub/titled/FM") %>%
   data.frame()

 GM$players <- tolower(GM$players)
 IM$players <- tolower(IM$players)
 FM$players <- tolower(FM$players)
 a_top$White <- tolower(a_top$White)
 a_top$Black <- tolower(a_top$Black)

 # Adding Opponent Rating and Title Columns
 a_top <- a_top %>%
   mutate(
     opponent = case_when(
       tolower(Username) == White ~ Black,
       tolower(Username) == Black ~ White
     ),
     opponent_rating = case_when(
       tolower(Username) == White ~ BlackElo,
       tolower(Username) == Black ~ WhiteElo
     ), 
     `Opponent Title` = case_when(
       opponent %in% GM$players ~ "GM",
       opponent %in% IM$players ~ "IM",
       opponent %in% FM$players ~ "FM",
       TRUE ~ "Other/Unknown"
     ), .before = WhiteElo
   )
 
 
 # Overall Resutls by Title
 big5_summary <- a_top %>%
   group_by(`Opponent Title`) %>%
   summarize(
     Games = n(),
     `Avg Opponent Rating` = round(mean(opponent_rating), digits = 0),
     Score = sum(player_result_value),
     `Score Above Expected` = round(sum(player_result_value - elo_expected_score), digits = 1),
     `Score Above Expected Per Game` = round(sum(player_result_value - elo_expected_score) / n(), digits = 2)
   )
 
 # Each Member of Big 5 vs Title
 big5_summary_each <- a_top %>%
   group_by(Username, `Opponent Title`) %>%
   summarize(
     Games = n(),
     `Avg Opponent Rating` = round(mean(opponent_rating), digits = 0),
     Score = sum(player_result_value),
     `Score Above Expected` = round(sum(player_result_value - elo_expected_score), digits = 1),
     `Score Above Expected Per Game` = round(sum(player_result_value - elo_expected_score) / n(), digits = 2)
   ) %>%
   filter(Games >= 50)
 
 # Individual Matchups
 big5_summary_matchups <- a_top %>%
   group_by(Username, opponent) %>%
   summarize(
     Games = n(),
     `Avg Opponent Rating` = round(mean(opponent_rating), digits = 0),
     Score = sum(player_result_value),
     `Score Above Expected` = round(sum(player_result_value - elo_expected_score), digits = 1),
     `Score Above Expected Per Game` = round(sum(player_result_value - elo_expected_score) / n(), digits = 2)
   ) %>%
   filter(Games >= 75) %>%
   arrange(desc(`Score Above Expected Per Game`))
 
 table_1 <- reactable(big5_summary,
   theme = fivethirtyeight()
 )
 
 table_1 %>%
   add_title("Big 5 By Opponent Title", font_size = 25)  
 
 table_2 <- reactable(big5_summary_matchups,
                      theme = fivethirtyeight()
 )
 
 table_2 %>%
   add_title("Big 5 Matchups", font_size = 25)  
 
 
 


