# Load Libraries and Data ----
library(nflfastR)
library(tidyverse)

# Load play-by-play data for a specific season----
pbp_data <- load_pbp(2024)

# Filter the data to include plays with non-NA success and EPA values----
filtered_data <- pbp_data %>%
  filter(!is.na(success), !is.na(epa))

# Extract unique game-level data----
# Deduplicate game-level scores
game_scores <- filtered_data %>%
  group_by(game_id, home_team, away_team) %>%
  summarise(
    total_home_score = max(total_home_score, na.rm = TRUE),
    total_away_score = max(total_away_score, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate offensive metrics
offense_metrics <- filtered_data %>%
  group_by(posteam) %>%
  summarise(
    offense_success_rate = mean(success, na.rm = TRUE),
    offense_epa = mean(epa, na.rm = TRUE),
    .groups = "drop"
  )

# Aggregate points scored from game_scores
offensive_ratings <- game_scores %>%
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = "location",
    values_to = "posteam"
  ) %>%
  mutate(points_scored = if_else(location == "home_team", total_home_score, total_away_score)) %>%
  group_by(posteam) %>%
  summarise(
    games_played = n(),
    total_points = sum(points_scored, na.rm = TRUE),
    avg_points_scored = mean(points_scored, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(offense_metrics, by = "posteam")

# Calculate defensive metrics
defensive_ratings <- filtered_data %>%
  group_by(defteam) %>%
  summarise(
    defense_success_rate = mean(success, na.rm = TRUE),
    defense_epa = mean(epa, na.rm = TRUE),
    .groups = "drop"
  )

# Merge offensive and defensive metrics
team_ratings <- offensive_ratings %>%
  left_join(defensive_ratings, by = c("posteam" = "defteam"))

# Print team ratings
print(team_ratings)

# Fit regression model
lm_points_scored <- lm(avg_points_scored ~ offense_success_rate + offense_epa, data = team_ratings)

# Print model summary
summary(lm_points_scored)

predict_spread <- function(home_team, away_team, hfa = 1.5) {
  # Standardize predictors globally
  mean_success_rate <- mean(team_ratings$offense_success_rate, na.rm = TRUE)
  sd_success_rate <- sd(team_ratings$offense_success_rate, na.rm = TRUE)
  mean_epa <- mean(team_ratings$offense_epa, na.rm = TRUE)
  sd_epa <- sd(team_ratings$offense_epa, na.rm = TRUE)
  
  # Standardize offensive metrics for home and away teams
  home <- team_ratings %>% filter(posteam == home_team) %>%
    mutate(
      offense_success_rate_std = (offense_success_rate - mean_success_rate) / sd_success_rate,
      offense_epa_std = (offense_epa - mean_epa) / sd_epa
    )
  away <- team_ratings %>% filter(posteam == away_team) %>%
    mutate(
      offense_success_rate_std = (offense_success_rate - mean_success_rate) / sd_success_rate,
      offense_epa_std = (offense_epa - mean_epa) / sd_epa
    )
  
  # Predict average points scored
  home_points <- predict(lm_points_scored, newdata = home)
  away_points <- predict(lm_points_scored, newdata = away)
  
  # Calculate spread (home - away) + home field advantage
  spread <- (home_points - away_points) + hfa
  return(round(spread, 2))
}

# Week 12 games
future_games <- tibble(
  home_team = c("CIN"),
  away_team = c("PIT"),
  home_field_advantage = 2.17  # Default home field advantage
)

# Predict spread for future games
future_games <- future_games %>%
  mutate(
    predicted_spread = mapply(predict_spread, home_team, away_team, home_field_advantage)
  )

# View the results
print(future_games)

# Export results
write_csv(future_games, "week_12_predicted_spreads.csv")

# Add a power rating to team_ratings----

game_scores <- game_scores %>%
  mutate(
    home_win = if_else(total_home_score > total_away_score, 1, 0),
    away_win = if_else(total_away_score > total_home_score, 1, 0)
  )

team_wins <- game_scores %>%
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = "location",
    values_to = "team"
  ) %>%
  mutate(wins = if_else(location == "home_team", home_win, away_win)) %>%
  group_by(team) %>%
  summarise(total_wins = sum(wins, na.rm = TRUE), .groups = "drop")

team_ratings <- team_ratings %>%
  left_join(team_wins, by = c("posteam" = "team"))

# Fit a regression model to predict wins using team metrics
lm_model <- lm(total_wins ~ avg_points_scored + offense_success_rate + offense_epa - 
                 defense_success_rate - defense_epa, data = team_ratings)

# Extract coefficients as weights
summary(lm_model)

print(colnames(team_ratings))

cor(team_ratings$total_wins, team_ratings$defense_success_rate, use = "complete.obs")
cor(team_ratings$total_wins, team_ratings$defense_epa, use = "complete.obs")
cor(team_ratings$total_wins, team_ratings$offense_success_rate, use = "complete.obs")
cor(team_ratings$total_wins, team_ratings$offense_epa, use = "complete.obs")

# QB data based on the provided text
qb_data <- tibble(
  posteam = c("KC", "DET", "BUF", "PHI", "SF", "PIT", "MIN", 
              "LAC", "HOU", "GB", "WAS", "SEA", "CIN", "MIA", "LA", 
              "IND", "ATL", "NYJ", "NO", "DAL", "CHI", "JAX", "NYG", 
              "TEN", "NE", "LV", "CAR", "ARI", "BAL", "CLE", "DEN", "TB"),
  qb_value = c(7.0, 3.5, 6.5, 4.5, 4, 2.5, 2.5, 
               5.5, 5.5, 4.5, 2.5, 3.5, 6.0, 4.0, 5.5, 
               2.0, 3, 2.5, 2.5, 0.5, 2.5, 0.5, 0.5, 
               1.5, 2.5, 0.5, 1.5, 4.0, 6.5, 2.5, 2.5, 3.0)
)

# View the QB data
print(qb_data)

# Merge QB data into team_ratings
team_ratings <- team_ratings %>%
  left_join(qb_data, by = "posteam")

# View the updated team_ratings with QB values
print(head(team_ratings))

team_ratings <- team_ratings %>%
  mutate(
    power_rating =  # Weighted importance of scoring
      offense_success_rate * 25 +  # Scaled offensive success rate
      qb_value  +                   # Significant QB impact
      offense_epa * 30 -          # Scaled offensive EPA
      defense_success_rate * 15 - # Penalize defensive success rate
      defense_epa * 20            # Penalize defensive EPA
  )

# Sort teams by power rating
ranked_teams <- team_ratings %>%
  arrange(desc(power_rating)) %>%
  select(posteam, power_rating, avg_points_scored, offense_success_rate, offense_epa, defense_success_rate, defense_epa)

# Display top teams
print(ranked_teams)

write_csv(ranked_teams, "Team Power Rankings.csv")

#cor(team_ratings$total_wins, team_ratings$qb_value, use = "complete.obs")

#What if I weighed last 6 weeks more Heavily----
current_week <- max(pbp_data$week, na.rm = TRUE)  # Find the latest week in the data
recent_weeks <- 6  # Number of weeks to weigh heavily

weighted_data <- pbp_data %>%
  mutate(
    week_weight = ifelse(week >= (current_week - recent_weeks + 1), 
                         (current_week - week + 1), 1)  # Linear weight based on recency
  )

weighted_metrics <- weighted_data %>%
  group_by(posteam) %>%
  summarise(
    offense_success_rate = sum(success * week_weight, na.rm = TRUE) / sum(week_weight, na.rm = TRUE),
    offense_epa = sum(epa * week_weight, na.rm = TRUE) / sum(week_weight, na.rm = TRUE),
    .groups = "drop"
  )

team_ratings <- team_ratings %>%
  select(-offense_success_rate, -offense_epa) %>%
  left_join(weighted_metrics, by = "posteam")

team_ratings <- team_ratings %>%
  mutate(
    power_rating = 
      offense_success_rate * 25 +
      qb_value + 
      offense_epa * 30 - 
      defense_success_rate * 15 - 
      defense_epa * 20
  )

ranked_teams <- team_ratings %>%
  arrange(desc(power_rating)) %>%
  select(posteam, power_rating, avg_points_scored, offense_success_rate, offense_epa, defense_success_rate, defense_epa)

# Display top teams
print(ranked_teams)


#write_csv(team_ratings, "Weighted Team Power Rankings.csv")


#ELO Ratings----
# Calculate defense averages (what each defense allows on average)
defense_averages <- filtered_data %>%
  group_by(defteam) %>%
  summarise(
    avg_epa_allowed = mean(epa, na.rm = TRUE),
    avg_success_allowed = mean(success, na.rm = TRUE),
    .groups = "drop"
  )

# Adjust offensive metrics using normalization
weighted_data <- weighted_data %>%
  left_join(defense_averages, by = c("defteam" = "defteam")) %>%
  mutate(
    adj_epa = epa - avg_epa_allowed,
    adj_success = success - avg_success_allowed # Normalized adjustment
  )

# Validate recency-weighted metrics
summary(weighted_metrics$offense_success_rate)
summary(weighted_metrics$offense_epa)


# Calculate opponent-adjusted offensive metrics for each team
offense_adjusted_metrics <- weighted_data %>%
  group_by(posteam) %>%
  summarise(
    adj_offense_epa = mean(adj_epa, na.rm = TRUE),
    adj_offense_success = mean(adj_success, na.rm = TRUE),
    .groups = "drop"
  )

# Merge adjusted metrics into team_ratings
team_ratings <- team_ratings %>%
  select(-offense_success_rate, -offense_epa) %>%  # Remove unadjusted metrics
  left_join(offense_adjusted_metrics, by = "posteam") %>%
  rename(
    offense_success_rate = adj_offense_success,
    offense_epa = adj_offense_epa
  )

# Calculate offensive averages (what each offense produces on average)
offense_averages <- filtered_data %>%
  group_by(posteam) %>%
  summarise(
    avg_epa_produced = mean(epa, na.rm = TRUE),
    avg_success_produced = mean(success, na.rm = TRUE),
    .groups = "drop"
  )

# Merge offensive averages into data
weighted_defense_data <- weighted_data %>%
  left_join(offense_averages, by = c("posteam" = "posteam")) %>%
  mutate(
    adj_defense_epa = epa - avg_epa_produced,  # Adjust defensive EPA by what the offense typically produces
    adj_defense_success = success - avg_success_produced # Adjust success rate similarly
  )

# Aggregate adjusted defensive metrics for each team
defense_adjusted_metrics <- weighted_defense_data %>%
  group_by(defteam) %>%
  summarise(
    adj_defense_epa = mean(adj_defense_epa, na.rm = TRUE),
    adj_defense_success = mean(adj_defense_success, na.rm = TRUE),
    .groups = "drop"
  )

# Merge adjusted defensive metrics into team_ratings
team_ratings <- team_ratings %>%
  select(-defense_success_rate, -defense_epa) %>%  # Remove unadjusted metrics
  left_join(defense_adjusted_metrics, by = c("posteam" = "defteam")) %>%
  rename(
    defense_success_rate = adj_defense_success,
    defense_epa = adj_defense_epa
  )

# Recalculate power ratings with adjusted metrics
team_ratings <- team_ratings %>%
  mutate(
    power_rating =
      offense_success_rate * 25 +
      qb_value + 
      offense_epa * 30 - 
      defense_success_rate * 15 - 
      defense_epa * 20
  )

# Rank teams by the updated power ratings
ranked_teams <- team_ratings %>%
  arrange(desc(power_rating)) %>%
  select(posteam, power_rating, qb_value, offense_success_rate, offense_epa, defense_success_rate, defense_epa)

# View top teams
print(ranked_teams)

write_csv(ranked_teams, "Adjusted Team Power Rankings.csv")

cor(team_ratings$total_wins, team_ratings$qb_value, use = "complete.obs")

model <- lm(total_wins ~ offense_success_rate + qb_value +
              offense_epa + defense_success_rate + defense_epa, data = team_ratings)

# Extract weights from the coefficients
summary(model)





