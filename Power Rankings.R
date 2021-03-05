

install.packages("distill")
distill::create_post("NFL Power Rankings")

# load libraries
library("nflfastR")
library("tidyverse")

# Connect to data
con <- url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")
nfl_games <- readRDS(con)
close(con)

skimr::skim(nfl_games)
summary(nfl_games)

table(nfl_games$location)

## Filter to 2020 games
nfl_2020 <- nfl_games %>%
  filter(season == 2020)


## Result is our point differential
# organize data with team, opponent, location, result
home <- nfl_2020 %>%
  filter(game_type == "REG",
         location == "Home") %>%
  select(result, home_team, away_team) %>%
  rename(Point_Diff = result,
         Team = home_team,
         Opp = away_team) %>%
  mutate(Location = "Home")

road <- nfl_2020 %>%
  filter(game_type == "REG",
         location == "Home") %>%
  select(result, home_team, away_team) %>%
  rename(Point_Diff = result,
         Team = away_team,
         Opp = home_team) %>%
  mutate(Location = "Road",
         Point_Diff = Point_Diff * -1)

neutral1 <- nfl_2020 %>%
  filter(game_type == "REG",
         location != "Home") %>%
  select(result, home_team, away_team) %>%
  rename(Point_Diff = result,
         Team = home_team,
         Opp = away_team) %>%
  mutate(Location = "Neutral")

neutral2 <- nfl_2020 %>%
  filter(game_type == "REG",
         location != "Home") %>%
  select(result, home_team, away_team) %>%
  rename(Point_Diff = result,
         Team = away_team,
         Opp = home_team) %>%
  mutate(Location = "Neutral",
         Point_Diff = Point_Diff * -1)


# We will use our Power numbers to predict the playoffs
test_data <- nfl_2020 %>%
  filter(game_type != "REG")


# join data
model_data <- bind_rows(
  home,
  road,
  neutral1,
  neutral2
)


new_games <- test_data

## Function to update data
update_model_data <- function(model_data, new_games) {


  home <- new_games %>%
    filter(location == "Home") %>%
    select(result, home_team, away_team) %>%
    rename(Point_Diff = result,
           Team = home_team,
           Opp = away_team) %>%
    mutate(Location = "Home")

  road <- new_games %>%
    filter(location == "Home") %>%
    select(result, home_team, away_team) %>%
    rename(Point_Diff = result,
           Team = away_team,
           Opp = home_team) %>%
    mutate(Location = "Road",
           Point_Diff = Point_Diff * -1)

  neutral1 <- new_games %>%
    filter(location != "Home") %>%
    select(result, home_team, away_team) %>%
    rename(Point_Diff = result,
           Team = home_team,
           Opp = away_team) %>%
    mutate(Location = "Neutral")

  neutral2 <- new_games %>%
    filter(location != "Home") %>%
    select(result, home_team, away_team) %>%
    rename(Point_Diff = result,
           Team = away_team,
           Opp = home_team) %>%
    mutate(Location = "Neutral",
           Point_Diff = Point_Diff * -1)


  # combine data
  model_data <- bind_rows(
    model_data,
    home,
    road,
    neutral1,
    neutral2
  )

  return(model_data)
}

new_data <- update_model_data(model_data = model_data, new_games = test_data)

##############################
##     Linear Regression    ## --------------------------------------------------------------------------
##############################

#' For the ratings, I will use the Team, Opponent and the Location
#' of the game (Home, Neutral, Road) to predict the Point Differential.
#' The ratings will be normalized with an average equal to 0.


lm_fit <- lm(Point_Diff ~ Team + Opp + Location, data = model_data)
summary(lm_fit)




##############################
##         Rankings         ## --------------------------------------------------------------------------
##############################


# scale with average team = 0
scale_factor <- mean(lm_fit$coefficients[2:32])

## Get Team Ratings and Rank
rankings <- data.frame("Team" = sort(unique(model_data$Team)),
                       "Rating" = rep(NA, 32)) %>%
  mutate(Rating = c(0, lm_fit$coefficients[2:32]) - scale_factor,
         Rank = rank(-Rating)) %>%
  select(Rank, Team, Rating) %>%
  arrange(Rank)

rankings

## plot rankings
rankings %>%
  ggplot(aes(x = Rating, y = reorder(Team, Rating))) +
  geom_col(aes(fill = if_else(Rating >= 0, "#2c7bb6", "#d7181c"))) +
  geom_text(aes(
    label = Team,
    color = if_else(Rating >= 0, "#2c7bb6", "#d7181c"),
    hjust = if_else(Rating > 0, -0.1, 1.1)
  ),
  fontface = "bold"
  ) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  # theme_538() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(-0.2, 0.3, 0.1)) +
  labs(
    x = "",
    y = "EPA per Dropback",
    title = "The majority of teams had positive EPA/dropback",
    subtitle = "But there are some clear outliers",
    caption = "Data: @nflfastR | Plot: @theFirmAISports"
  )


update_ratings <- function(model_data) {

  # train model
  lm_fit <- lm(Point_Diff ~ Team + Opp + Location, data = model_data)

  # scale with average team = 0
  scale_factor <- mean(lm_fit$coefficients[2:32])

  ## Get Team Ratings and Rank
  rankings <- data.frame("Team" = sort(unique(model_data$Team)),
                         "Rating" = rep(NA, 32)) %>%
    mutate(Rating = c(0, lm_fit$coefficients[2:32]) - scale_factor,
           Rank = rank(-Rating)) %>%
    select(Rank, Team, Rating) %>%
    arrange(Rank)

  return(rankings)
}


update_ratings(model_data = new_data)

##############################
##        Predictions       ## --------------------------------------------------------------------------
##############################

## predictions
model_data$pred_score <- predict(lm_fit, newdata = model_data)
model_data$win <- ifelse(model_data$Point_Diff > 0, 1, 0)

# logistic regression to get win probability
glm_pointspread <- glm(win ~ pred_score, data = model_data, family = "binomial")
model_data$win_prob <- predict(glm_pointspread, newdata = model_data, type = "response")


## Plot results
ggplot(data = model_data, aes(x = pred_score, y = win_prob, color = factor(win))) +
  geom_point() +
  scale_color_manual(values = c("red", "green")) +
  labs(
    title = "ACC Power Ratings",
    subtitle = "2018-19 Season",
    x = "Power Rating Spread",
    y = "Win Probability",
    color = 'Actual Result'
  ) +
  theme_bw()



##############################
##       Save Rankings      ## --------------------------------------------------------------------------
##############################

# Save Rankings
write_csv(rankings, here("data/ACCRankings1819.csv"))



